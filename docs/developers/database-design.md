---
id: database-design
title: Database design
sidebar_position: 8
---

# Database design

The PostgreSQL schema and write path are shaped around one observation:
catching up to mainnet is a multi-hour bulk load with no concurrent
readers, while staying at the tip is a slow per-block trickle with
strict atomicity requirements. The same schema serves both, but it
moves through transitional states between them.

## UNLOGGED during Ingest, LOGGED at handover

Every extractor data table is created `UNLOGGED` at boot. UNLOGGED
tables skip WAL writes on every row, which is the single biggest
PostgreSQL-side win on the bulk-load critical path. The price is that
UNLOGGED tables don't survive a crash.

Crash safety during Ingest comes from a different mechanism: the
sync-state row, updated at every epoch boundary, points at the slot
through which data is committed. On boot, `deleteRowsPastSlot`
removes any rows past that slot (cheap because UNLOGGED tables aren't
WAL-logged anyway), and the consumer restarts at the boundary.

Once Ingest exits, [`PreparingForVolatileTail`](phases/preparing)
flips every table `UNLOGGED → LOGGED`. With
`wal_level = minimal` the rewrite itself isn't WAL-logged; on
`wal_level = replica` it is, and the operator pays the cost.

:::tip
`scripts/postgres-tuning.conf` in the repo documents the trade-off
along with the rest of the bulk-sync PostgreSQL tuning. Reading it
before starting a multi-day mainnet sync is worth the few minutes.
:::

Three tables are LOGGED from day one: `dbsync_sync_state`, because it is
the resume marker; `epoch_param_pending`, the small bridging table Prep
consumes; and `epoch_finalized`, the one extractor-owned table that is
not created UNLOGGED.

## One COPY connection per table

[`DbSync.Db.Loader`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Loader.hs)
opens one libpq connection per data table and spawns a worker thread on
each. `Phase.Ingest.Writer` sits above it, composing the per-table
`write*Copy` functions into a `Writer` record; the connections and threads
belong to the loader.

The writer encodes rows, batches them into chunks, and picks the right
queue. The worker blocks in `PQ.putCopyData` on its dedicated connection
without contending with the others.

Why not a connection pool? Two reasons:

- COPY is a session-level mode. A connection in `COPY` mode can't
  service other queries; once it's COPYing it stays that way until
  the stream ends. A pool would either dedicate connections (the same
  as what we do, with extra bookkeeping) or thrash by ending and
  re-establishing COPY mode constantly.
- Per-table connections give the kernel scheduler natural parallelism.
  COPY is dominated by network round-trips and PostgreSQL-side
  parsing; running N table COPYs concurrently gets N×kernel-CPU on
  the encoder side and N×PG backends on the database side.

File descriptors are the one resource this design consumes heavily. At
boot, `Phase.Ingest.FdLimit` raises the process's **soft** limit to the
hard limit, capped at 1048576. It does not abort on a low limit: if the
OS refuses the raise it logs a warning and continues. Its stated
motivation is the Ingest LSM session rather than the COPY connections.

## ID assignment

Two strategies, one per phase.

### Ingest: counter + dedup

COPY has no return channel for generated IDs (`RETURNING` isn't
available in `COPY ... FROM STDIN`). The Ingest resolver hands out
IDs from two cooperating pieces:

- A per-table monotonic `Counter` that pre-assigns IDs as the parser
  walks the block. The block, every tx, every output, every input —
  all get their final IDs before the writer ever sees them.
- A `DedupStore` (LSM-tree) mapping natural keys to previously
  assigned IDs for the tables that need it: `stake_address`,
  `pool_hash`, `slot_leader`, `multi_asset`, `script`, `datum`,
  `redeemer_data`, `drep_hash`, `committee_hash`, and `voting_anchor`.

The pre-assignment is what makes extractors textually independent:
extractor `pool` doesn't need to wait for `stake_delegation` to write
its `stake_address` row before it can use the resulting ID — the ID
is already known via the shared dedup helper.

At resume time, the counters and dedup stores are rebuilt from PG
(`SELECT MAX(id) FROM ...`, `SELECT hash, id FROM ...`) rather than
relying on the on-disk LSM data, which might be ahead of
`last_committed_slot`.

### Follow: pre-allocated batches via sequences

Once Ingest is done and the tables are LOGGED, each id column gets a
PostgreSQL sequence attached at its current max. The Follow loop
allocates IDs in bulk per block:

1. Walk the parsed `GenericBlock` and count how many of each kind
   the block will need (`countAssignableIds`).
2. Issue a single libpq pipeline against the sequences via
   `allocateAllIds`.
3. Hand the batches to a buffered resolver that dispenses them as
   extractors call for IDs.

This avoids one `nextval` round-trip per row and one
`INSERT … RETURNING id` per record, both of which would dominate
per-block latency even with hasql pipelining.

The dedup tables still hit PG synchronously in Follow — one `SELECT`
per new natural key — but a per-block cache makes intra-block
sightings free.

## LSM-backed scratch state

Three LSM-tree backed stores sit on the Ingest write path:

| Store | Role | Eviction |
|---|---|---|
| **DedupStore** (10 tables) | Natural-key → assigned-ID maps for dedup tables. | Wiped at Ingest → Prep handoff. |
| **UtxoStore** | `tx-hash → (TxId, [(TxOutId, value)])` for inline input resolution. | Deleted entries when outputs are consumed; the whole session is wiped at Ingest → Prep. |
| **Cardano LedgerDB** | The V2 ledger UTxO set when ledger is enabled. | Persists across all phases and restarts. |

The first two share an `LsmSession` so they compact at the same epoch
boundary, halving the on-disk fsync cost. Both live under
`<state-dir>/dbsync-ledger/ingest-lsm/`. The LedgerDB lives under
`<state-dir>/dbsync-ledger/` proper.

[`lsm-tree`](https://hackage.haskell.org/package/lsm-tree) was chosen
because all three are write-dominated key-value stores with bursty
hot-path traffic and natural compaction points at epoch boundaries.
A B-tree (RocksDB, LMDB) optimises for read-mostly workloads we don't
have on these paths.

## Index strategy

Indexes are built in **four passes**, three of them in
[`PreparingForVolatileTail`](phases/preparing):

1. **Per-epoch resolver indexes**, built once shortly after boot on
   the still-UNLOGGED tables. These cover the per-epoch tx-out
   worker's bulk lookups: `tx_out` PK, `collateral_tx_out` PK,
   `address`'s unique hash index. Without these, the worker's joins
   degrade to full-heap hash joins late in Ingest. See
   [`Phase.Ingest.Indexes`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/Indexes.hs).
2. **Pre-resolve indexes**, the first thing Prep does. Cover the joins
   the CTAS rebuilds and the backfill UPDATEs use.
3. **Post-resolve indexes**, rebuilding the input-table indexes the CTAS
   rebuilds dropped.
4. **Production indexes**, the schema-driven full set from
   `tableIndexStatements`, driven by `tdPrimaryKey` and
   `tdUniqueConstraints`. Built **after** the UNLOGGED → LOGGED flip and
   **before** the final `ANALYZE`. That order matters: `SET LOGGED`
   rebuilds every index on the table inside the ALTER, so the flip runs
   against bare heaps and each production index is built exactly once.

All three pass `IF NOT EXISTS` so a re-run is a no-op. Non-concurrent
because nothing is reading the DB at this point — that unlocks
`max_parallel_maintenance_workers`.

Performance-only indexes (lookups that aren't enforcing uniqueness)
are currently in the hand-rolled pre-resolve set. A future change
extends `TableDef` with explicit index metadata.

## Rollback in Follow

The Follow `MsgRollback` cascade
([`Phase.Following.Rollback`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Following/Rollback.hs))
walks the schema's ownership graph in dependency order. It deletes by
**id threshold**, not by slot: it finds the first surviving `tx.id`,
`tx_out.id`, and `pool_update.id` after the target block, then deletes
every row at or above each. Block-keyed rows delete on `block_id`, and
epoch-keyed rows on `epoch_no`.

The graph comes from `tdParentRefs` on each `TableDef`, read through
`childrenOf`, which is why there is no parallel hand-coded ordering to
maintain.

:::caution `k`-bounded
Rollbacks deeper than `k` (2160 on mainnet) exceed protocol-level
guarantees. The Follow loop panics with an operator-readable
recovery message rather than silently corrupting the database.
:::

## Schema migrations

Schema migrations ship as ordered SQL files under `dbsync-db/migrations/`,
embedded into the binary at build time. At boot
[`DbSync.Db.Schema.Migration`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Migration.hs)
compares the version stamped on `dbsync_sync_state` against the version
the binary targets and, when the database is behind, applies the
intervening files in a single transaction and re-stamps the row. A
database built by a newer binary, or one whose shape has drifted with no
migration covering it, aborts boot. The full workflow — fingerprints,
the `gen-migration` tool, and the drift tests — is in
[Schema versioning and migrations](schema-versioning).

Alongside this is **config-driven schema selection**: the caller
flattens the enabled extractors' tables and `initSchema` emits DDL for
exactly that list. Enabling a new extractor on a fresh database gets you
the new tables. Enabling — or disabling — one on a populated database is
rejected at boot with a clear message.
