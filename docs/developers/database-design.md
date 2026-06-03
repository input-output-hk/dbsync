---
id: database-design
title: Database design
sidebar_position: 7
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
flips every table `UNLOGGED → LOGGED`. On profiles with
`wal_level = minimal` the rewrite itself isn't WAL-logged; on
`wal_level = replica` it is, and the operator pays the cost.

:::tip
`scripts/postgres-tuning.conf` in the repo documents the trade-off
along with the rest of the bulk-sync PostgreSQL tuning. Reading it
before starting a multi-day mainnet sync is worth the few minutes.
:::

The state-survival exception is `dbsync_sync_state` — that table is
LOGGED from day one because it's the resume marker. Same for
`epoch_param_pending`, the small bridging table that Prep consumes.

## One COPY connection per table

The Ingest writer ([`Phase.Ingest.Writer`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/Writer.hs))
opens one libpq connection per data table and spawns a worker thread
on each. The writer thread encodes a row, picks the right queue, and
hands off; the worker blocks on `getCopyData`/`putCopyData` against
its dedicated connection without contending with the others.

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

The number of file descriptors required is bounded:
`Phase.Ingest.FdLimit` checks `ulimit -n` at boot and aborts with an
operator-readable message if it's too low.

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
  `multi_asset`, `pool_hash`, `slot_leader`, `cost_model`.

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
| **DedupStore** (5 tables) | Natural-key → assigned-ID maps for dedup tables. | Wiped at Ingest → Prep handoff. |
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

Indexes are built in **three passes**, two of them in
[`PreparingForVolatileTail`](phases/preparing):

1. **Per-epoch resolver indexes**, built once shortly after boot on
   the still-UNLOGGED tables. These cover the per-epoch tx-out
   worker's bulk lookups: `tx_out` PK, `collateral_tx_out` PK,
   `address`'s unique hash index. Without these, the worker's joins
   degrade to full-heap hash joins late in Ingest. See
   [`Phase.Ingest.Indexes`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/Indexes.hs).
2. **Pre-resolve indexes**, the first thing Prep does. Cover the
   joins that the CTAS rebuilds and the backfill UPDATEs use.
3. **Production indexes**, the schema-driven full set from
   `tableIndexStatements`. Built after the FK resolves are done but
   before `ANALYZE` and the UNLOGGED → LOGGED flip. Driven by
   `tdPrimaryKey` and `tdUniqueConstraints` on each `TableDef`.

All three pass `IF NOT EXISTS` so a re-run is a no-op. Non-concurrent
because nothing is reading the DB at this point — that unlocks
`max_parallel_maintenance_workers`.

Performance-only indexes (lookups that aren't enforcing uniqueness)
are currently in the hand-rolled pre-resolve set. A future change
extends `TableDef` with explicit index metadata.

## Rollback in Follow

The Follow `MsgRollback` cascade
([`Phase.Following.Rollback`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Following/Rollback.hs))
walks the schema's FK graph in dependency order, issuing
`DELETE FROM <t> WHERE <fk> > <target>` per table per family. The FK
metadata for the walk comes from `tdForeignKeys` on each `TableDef`,
which is why there's no parallel hand-coded table to maintain — the
cascade derives the order from the schema itself.

:::caution `k`-bounded
Rollbacks deeper than `k` (2160 on mainnet) exceed protocol-level
guarantees. The Follow loop panics with an operator-readable
recovery message rather than silently corrupting the database.
:::

## Schema migrations

:::warning Not yet implemented
Schema migrations are intentionally not implemented yet. The repo
is greenfield and pre-release; adding a column or changing a type
currently means a full re-sync. The
[`DbSync.Db.Schema.Migration`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Schema/Migration.hs)
module exists as a placeholder for the migration runner that will
land before the first stable release.
:::

What does work today is **profile-driven schema selection**:
`initSchema` walks the enabled extractors and emits DDL for exactly
the tables they own. Enabling a new extractor on a fresh database
gets you the new tables; enabling it on a populated database is
rejected at boot with a clear message.
