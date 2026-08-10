---
id: ingest
title: IngestChainHistory
sidebar_position: 2
---

# IngestChainHistory

The bulk-load phase. Drains immutable chain history into UNLOGGED PostgreSQL
tables via parallel COPY streams (one libpq connection per table) and exits
once it reaches the **rollback boundary** at `nodeTip − k`.

The driver is [`DbSync.Phase.Ingest.Consumer`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/Consumer.hs);
the epoch-boundary cascade lives in
[`DbSync.Phase.Ingest.Boundary`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/Boundary.hs).

## The consumer loop

The consumer is a single thread that drains the receiver's block queue in
**batches of up to 100**, runs each block through `processBlock`, and tracks
epoch transitions. Per-block work (parsing, extractors, LSM lookups)
dominates by orders of magnitude over per-batch overhead, so the exact
ceiling has little effect on throughput.

```mermaid
flowchart TD
    Queue[("Block queue<br/>filled by receiver")]
    Drain["drainTBQueue<br/>(up to 100 blocks)"]
    Loop["for each block:<br/>parseBlock → processBlock"]
    Epoch{"epoch<br/>changed?"}
    Boundary["handleEpochBoundary<br/>(see below)"]
    Boundary2{"batch done:<br/>crossed nodeTip − k?"}
    Exit["exit; hand off to<br/>PreparingForVolatileTail"]

    Queue --> Drain --> Loop
    Loop --> Epoch
    Epoch -- "yes" --> Boundary --> Boundary2
    Epoch -- "no" --> Boundary2
    Boundary2 -- "no" --> Drain
    Boundary2 -- "yes" --> Exit
```

:::note `MsgRollback` is unreachable here
The consumer exits at `nodeTip − k`, and the chain cannot roll back
below that boundary, so no rollback marker can arrive while Ingest is
running.

The receiver does **not** filter them. `deliverRollback` enqueues every
non-confirming marker regardless of depth; the guarantee comes from the
consumer's own exit, not from receiver-side gating. If one does arrive,
the consumer panics rather than silently corrupting bulk-loaded data.
:::

## ID strategy

COPY has no return channel for generated IDs. The Ingest resolver covers
that gap with two cooperating pieces:

- **Counter** ([`DbSync.Phase.Ingest.Counter`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/Counter.hs)) —
  per-table monotonic counters that hand out fresh IDs in-process.
- **DedupStore** ([`DbSync.Phase.Ingest.DedupStore`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/DedupStore.hs)) —
  ten LSM-tree tables, one per dedup-eligible natural key (stake
  credential, pool key hash, slot leader, multi-asset id, script hash,
  datum hash, redeemer-data hash, DRep hash, committee hash, voting
  anchor), mapping each to the ID assigned the first time we saw it.

For each block, `processBlock` (in `DbSync.Extractor.Pipeline`) pre-assigns
the shared IDs — `BlockId`, `SlotLeaderId`, per-tx `TxId`, per-output
`TxOutId` — *before* any extractor runs. Extractors then consume the
pre-assigned IDs, so they're textually independent.

## LSM-backed scratch state

The DedupStore and the UTxO store (below) sit on a shared `LsmSession`
([`DbSync.Phase.Ingest.LsmSession`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/LsmSession.hs)),
using the same [`lsm-tree`](https://hackage.haskell.org/package/lsm-tree)
library the Cardano LedgerDB uses for its on-disk UTxO. The choice is
deliberate: both are write-dominated key-value stores with bursty hot-path
traffic and periodic compaction at epoch boundaries.

The session lives under `<state-dir>/dbsync-ledger/ingest-lsm/`. Its
tables close at the Ingest → Prep handoff, but the session directory is
deleted only after Prep completes — Prep uses it as the restart anchor.
Follow never consults it.

Every epoch boundary persists a snapshot. A **full compaction — snapshot
plus reopen — runs every 20th boundary**, not every one.

:::info Why rebuild dedup from PG, not LSM
A restart in Ingest reopens the existing session, but `BootResume`
rebuilds the dedup tables from PG rather than from the LSM tables. Writes
land in LSM continuously while the persist happens only at boundaries, so
LSM can be ahead of `last_committed_slot`. PG is the truth source.
:::

See [Architecture › Storage backends](../architecture#storage-backends)
for the broader picture of LSM vs PG roles.

## Inline UTxO resolution

The UTxO extractor needs `tx_in.tx_out_id` for every input it writes. In
Follow, that's a SQL lookup against the existing `tx_out` row. In Ingest,
the row may not be in PG yet — COPY workers are still draining the queue.

The fix is an LSM-backed in-process cache:
[`Phase.Ingest.UtxoStore`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/UtxoStore.hs)
maps `tx-hash → (TxId, [(TxOutId, value)])` and is populated as each tx's
outputs are pre-assigned. A later tx in the same block spending an earlier
tx's output resolves through the cache; misses fall through to a post-load
**resolve pass** in `PreparingForVolatileTail`.

Consumed outputs are deleted from the cache so it tracks the **live UTxO
set**, not chain history. On mainnet this caps the LSM table at the
current UTxO size rather than total tx-output count.

## Epoch boundaries

The consumer detects an epoch transition by comparing `sdEpochNo` between
adjacent blocks. On a cross, control passes to `handleEpochBoundary`, which
runs a **pipelined cascade**:

1. Flush the loader stream — commit every per-table COPY in flight. The
   commits fan out concurrently, one round-trip per connection.
2. Snapshot the per-epoch buffers: the address buffer and the consumed-by
   buffer.
3. **Await the tx-out worker**, which is draining the *previous* epoch's
   job.
4. Flush the pending deposits.
5. Advance `dbsync_sync_state` for the previous pending epoch.
6. **Enqueue the just-finished epoch** as the tx-out worker's next job.
7. Stash this epoch's snapshot for the next boundary to advance.
8. Reopen the loader stream for the next epoch.
9. Await the LSM persist or compaction. It was spawned before step 1, so
   it overlaps the PG-bound steps, and it must settle before the boundary
   block's extractors resolve against the stores.
10. Write the `epoch_sync_stats` row, then run a major GC, then emit the
    per-epoch summary line.

The await in step 3 comes **before** the enqueue in step 6. That ordering
is what produces the one-epoch lag described below; swapping them would
collapse it.

The GC has two gates. It runs only when the epoch took more than 10
seconds **and** the live heap has outgrown the last boundary
collection's baseline (`2 * live >= 3 * base`).

The **one-epoch lag** is deliberate. `dbsync_sync_state` always reflects the last
epoch the tx-out worker has fully resolved, so a crash mid-epoch can be
cleanly cleaned up with `deleteRowsPastSlot` on resume. The pipelining
means the consumer doesn't wait for the worker on the hot path — it only
blocks on the *previous* epoch's drain at the next boundary, by which time
the worker has almost always finished.

## Exit: the rollback boundary

Cardano's protocol security parameter `k` (2160 on mainnet) is the maximum
rollback depth. Below `nodeTip − k` the chain cannot roll back; above it
any block could be reverted. The Ingest consumer exits when the last
applied block reaches that boundary, because above it dbsync needs the
per-block transactional path that
[`FollowingVolatileTail`](following-volatile-tail) provides.

The receiver publishes the boundary as a `TVar (Maybe BlockNo)`, updated
on every observed tip. The comparison is `lastBlock >= boundary` on
**block numbers**; the slot plays no part.

The check runs once per drained **batch**, after `processBatch` returns —
not once per block. It costs one `readTVarIO` and a comparison.

Once the consumer exits, the orchestrator cancels the receiver, releases
the loader stream + tx-out worker + dedup stores + UTxO store, and runs
[`PreparingForVolatileTail`](preparing) before flipping to Follow.

## Crash recovery

`IngestChainHistory` can resume from any committed epoch boundary. The
boot decision (`BootResume`) reads the sync-state row, rebuilds the dedup
maps from PG, populates the cost-model cache, and (when ledger is on)
loads the chosen on-disk snapshot.

Any rows past `last_committed_slot` are deleted by `deleteRowsPastSlot`
during boot. UNLOGGED tables are zero-cost to clean up since they're not
WAL-logged anyway.
