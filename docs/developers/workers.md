---
id: workers
title: Workers
sidebar_position: 4
---

# Workers

Work that runs **alongside** the main pipeline rather than inside it. None
of these workers sit on the hot path; an idle or stalled side channel
slows nothing on the consumer thread.

Three subsystems, each with its own module root under
`DbSync.Worker.*`:

- **Ledger Worker** — applies blocks to an in-RAM `LedgerDB` to produce
  per-block and per-epoch ledger-derived state.
- **TxOut Worker** — drains per-epoch buffers from Ingest's main loop and
  back-fills FK columns the COPY path couldn't.
- **OffChain Fetcher** — background HTTP fetchers for pool and
  Conway-vote metadata. Run when `off_chain_pools` / `off_chain_votes`
  are enabled; misses never block ingest.

## Ledger Worker

Lives under [`DbSync.Worker.Ledger.*`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Worker/Ledger/Worker.hs).

:::info Optional
Enabled by `ledger.enabled = true` in the profile. The ledger-
disabled path allocates none of the worker's state — no second
block queue, no snapshot writer, no `BlockLedgerData` wiring.
:::

### What it does

When enabled, the worker runs on its own thread and consumes a dedicated
copy of the chainsync block stream (the receiver fans `MsgForward` into a
second `TBQueue` when ledger is on). For each block it:

1. Applies the block to the in-memory `LedgerDB`.
2. Writes the resulting `ApplyResult` into a shared `TVar` the rest of
   the app reads from.
3. Signals epoch boundaries via a separate `TMVar` so the consumer can
   coordinate per-epoch work.

A second async — the **snapshot writer** — persists `LedgerDB` snapshots
to disk so a restart can resume without replaying from genesis.

### LSM backing

The `LedgerDB` is the V2 design from `ouroboros-consensus:lsm`: an
LSM-tree-backed on-disk UTxO set with in-memory caches for recent
blocks. Snapshots are written incrementally — the in-memory state and
the on-disk state advance together rather than dumping a full ledger
state on each snapshot. This is the same `lsm-tree` library dbsync uses
for its own [Ingest scratch state](phases/ingest#lsm-backed-scratch-state);
see [Architecture › Storage backends](architecture#storage-backends) for
the broader picture.

On disk the LedgerDB lives under `<state-dir>/dbsync-ledger/` proper,
separate from the Ingest scratch session at
`<state-dir>/dbsync-ledger/ingest-lsm/`.

### What it produces

Per-block:

- The block's **deposits map** — per-tx deposit amounts indexed by tx
  hash. Only populated for txs with stake registrations, pool
  registrations, or governance certs.
- Protocol-param **stake-key deposit** and **pool deposit** as of the
  block (extracted from the ledger PParams).

Per-epoch:

- **Rewards** — full per-account rewards snapshot at the boundary.
- **ada_pots** — the four canonical Cardano pots.
- **EpochParam** — the protocol parameters in effect for the epoch.
- **StakeDistribution** — the stake distribution that drives leader
  election.

### How it reaches extractors

Through `BlockLedgerData` on the `BlockContext`:

```haskell
data BlockLedgerData
  = LedgerDataOff
  | LedgerDataOn !LedgerOutputs
```

`LedgerDataOff` is the only shape when ledger is disabled — the worker
isn't even running. With `LedgerDataOn`, extractors read deposits and
deposit parameters directly off the context.

During `IngestChainHistory` with the worker on, `HasLedgerData
IngestEnv` blocks on the worker's per-block apply-result queue
(`takeBlockLedgerData`): the consumer never runs ahead of the ledger,
and a worker pause longer than the queue's banked results stalls block
processing. Protocol-param deposits additionally accumulate into
`epoch_param_pending` at epoch boundaries;
[`PreparingForVolatileTail`](phases/preparing) backfills the affected
columns once Ingest exits.

### Lifecycle across phases

The ledger worker + snapshot-writer asyncs stay alive across the entire
Ingest → Prep → Follow span. Their in-memory state keeps ticking through
Prep even though Prep itself doesn't touch the LedgerDB. On a clean
shutdown the worker drains its queue and writes a final snapshot; the
fingerprint file in the state directory pins the snapshot to this
chain's network magic + system start so a future boot refuses to attach
the wrong chain's ledger.

### Rollback

Rollbacks within the in-memory volatile buffer (the last ~100 blocks)
are handled by walking the buffer back to the target.

:::caution Deeper rollbacks
A rollback past the in-memory buffer panics with an
operator-actionable message. The recovery path is to restart dbsync
so the disk snapshot can be reloaded at the rollback point.
:::

## TxOut Worker

Lives under [`DbSync.Worker.TxOut.*`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Worker/TxOut/Worker.hs).
Active only during `IngestChainHistory`.

### What it does

The COPY path can't fill three columns at write time:

- `tx_out.address_id` — needs the dedup-resolved `address` row to exist.
- `collateral_tx_out.address_id` — same.
- `tx_out.consumed_by_tx_id` — needs the consuming `tx` row to exist
  (only enabled when `utxo.consumed_by_tx_id` is on).

Two per-epoch buffers accumulate the inputs:

- **`AddressBuffer`** — raw address bytes from each tx_out, paired with
  the assigned `TxOutId`, awaiting dedup.
- **`ConsumedByBuffer`** — `(producer_tx_out_id, consumer_tx_id)` pairs
  produced by the UTxO extractor on every cache hit.

At each Ingest epoch boundary, the consumer hands the buffers to the
worker as a `TxOutJob` and resets them. The worker drains the job on its
own dedicated PG connection: bulk-deduplicates the addresses (a single
`SELECT … WHERE hash IN (…)` round-trip), inserts the new ones, and runs
the back-fill UPDATEs. The four hook calls run in sequence on that
connection, so the worker cannot deadlock against itself on overlapping
`tx_out` rows.

### Back-pressure

The worker's job queue is bounded (`txOutWorkerQueueBound`). If the
worker falls more than that many epochs behind the consumer, the next
`enqueueTxOutJob` blocks, which back-presses the consumer and ultimately
the receiver. Steady-state operation keeps the worker within one or two
epochs of the consumer.

### Lifecycle

The worker is spawned alongside `IngestEnv` and drained at the Ingest →
Prep transition (`awaitTxOutDrained`). Follow doesn't use it — Follow's
buffered writer writes `address_id` synchronously in the per-block
`Pipeline`.

## OffChain Fetcher

Lives under [`DbSync.Worker.OffChain.*`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Worker/OffChain/Fetcher.hs).
Two independent workers — one for pool metadata, one for Conway
governance-vote metadata — spawned only when the matching extractor
(`off_chain_pools` / `off_chain_votes`) is enabled. Each owns its own
PG connection and its own HTTP connection pool.

### What they do

The two workers share a generic loop (`OffChainWorker`). Each cycle:

1. Loads a batch of pending refs from PG — anchors observed by the
   `off_chain_pools` / `off_chain_votes` extractors, plus earlier
   failures whose retry time has come due.
2. Fetches each over HTTP through a pluggable per-domain hook.
3. Persists the outcome: a data row on success, a fetch-error row on
   failure.

The pool worker writes `off_chain_pool_data` /
`off_chain_pool_fetch_error`; the vote worker writes the seven
`off_chain_vote_*` tables. Because the work is off the hot path, a
slow or unreachable host never blocks the consumer.

### Fetching safely

`newRestrictedManager` refuses to connect to private or loopback
addresses (SSRF / DNS-rebinding defence), and the URL validator allows
only `http(s)` GETs to non-localhost hosts. Payloads are size-capped
and hash-checked; the vote decoder understands the CIP-100 / 108 / 119
envelope and rewrites `ipfs://` URLs through configured gateways.

### Retry

Failed fetches back off exponentially, growing quickly over the first
few attempts and capping at one day. A permanently unreachable host
settles into one attempt per day rather than spamming the network.
