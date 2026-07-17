---
id: preparing
title: PreparingForVolatileTail
sidebar_position: 3
---

# PreparingForVolatileTail

One-time post-load pass between Ingest and Follow. Runs on a single hasql
connection plus a parallel pool for the heavy work.

The driver is [`DbSync.Phase.Preparing.Run`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Preparing/Run.hs).

## What needs fixing

Ingest's COPY pipeline leaves three things in a transitional state:

- **FK columns** on `tx_in`, `collateral_tx_in`, `reference_tx_in`, and
  optionally `tx_out.consumed_by_tx_id` are NULL or unresolved. COPY can't
  fill them because the producer rows weren't necessarily in PG yet at the
  time the consumer rows were written.
- **A few `tx` columns** — phase-2 fee, phase-2 deposit, the
  ledger-disabled valid-contract deposit — can't be filled from the tx body
  alone. The parser leaves a sentinel or NULL.
- **Tables are UNLOGGED**, have no sequences attached, and have no
  production indexes. The COPY pipeline ran flat-out without them.

The Preparing pass walks all of that.

## The sequence

```mermaid
flowchart TD
    GUCs["session GUCs<br/>(maintenance_work_mem,<br/>parallel workers, …)"]
    PreIdx["pre-resolve indexes<br/>(scaffolding, on UNLOGGED tables)"]
    Resolve["resolve FKs<br/>(CTAS rebuilds for tx_in et al.<br/>+ ANALYZE rebuilt tables)"]
    PostIdx["post-resolve indexes<br/>(scaffolding the CTAS dropped)"]
    Analyze1["ANALYZE backfill tables<br/>(fresh planner stats)"]
    Backfill["backfill tx columns<br/>+ apply epoch_param_pending"]
    Drop["drop scaffolding indexes<br/>(flip rewrites bare heaps)"]
    Flip["UNLOGGED → LOGGED flip<br/>+ attach sequences<br/>(parallel pool, per table)"]
    Build["index build<br/>(production set, parallel pool,<br/>per index, built exactly once)"]
    EpochFin["backfill epoch_finalized<br/>(upsert needs its unique index)"]
    Analyze2["ANALYZE per table"]
    Seq["reset sequences<br/>past Ingest-assigned IDs"]

    GUCs --> PreIdx --> Resolve --> PostIdx --> Analyze1
    Analyze1 --> Backfill --> Drop --> Flip --> Build
    Build --> EpochFin --> Analyze2 --> Seq
```

:::warning Step ordering matters
Pre-resolve indexes must precede the FK rebuild — otherwise the
resolve and backfill UPDATEs hash-join heap-to-heap. Every index must
be dropped before the schema-mode flip — `ALTER TABLE … SET LOGGED`
rewrites the heap *and rebuilds every index on the table inside the
ALTER*, so an index alive at flip time is built twice. The
`epoch_finalized` backfill must follow the index build — its
`ON CONFLICT ("no")` upsert needs the unique index. Sequence reset
must follow the flip — the sequence has to exist before `setval` can
advance it. Resist the urge to reorder.
:::

Every step is bracketed by a uniform log pair at the default `info`
level, so a slow pass shows exactly which step is in flight:

```
Starting  | resolve  | tx_in.tx_out_id (table rebuild)
Completed | resolve  | tx_in.tx_out_id (table rebuild) | 12m 34s | ✓
Starting  | backfill | tx.fee (Byron txs)
Completed | backfill | tx.fee (Byron txs) | 45.67s | ✓ | 1,234,567 rows
```

A step that throws emits `Failed | … | ✗` with the elapsed time and
rethrows. The parallel flip and index-build steps additionally log one
pair per table / per index inside the outer step's pair.

## CTAS rebuild

For `tx_in`, `collateral_tx_in`, and `reference_tx_in`, the FK resolution
rebuilds the table from a `SELECT` joined against `tx` rather than
`UPDATE`-ing the existing rows. The rebuild is faster for full-table
fixes on UNLOGGED data, and it drops the input-table's old scaffolding
indexes along the way — which is why the **post-resolve indexes** step
comes straight after, before the backfill UPDATEs that need them. The
rebuild is a true `CREATE TABLE … AS SELECT`: PostgreSQL never
parallelises a query that writes through `INSERT`, but a CTAS `SELECT`
is eligible for a parallel join plan. NOT NULL / default / check /
identity constraints are re-attached from the `TableDef` after the
rename, and the recreated identity sequences are repositioned by the
final sequence-reset step. The rebuilt tables are `ANALYZE`d
immediately so the residual `consumed_by_tx_id` UPDATE doesn't plan
against zero statistics.

## UNLOGGED → LOGGED

Tables run UNLOGGED during Ingest so PostgreSQL skips WAL for every row
written. The price is that UNLOGGED tables don't survive a crash. The
flip rewrites each table's heap into the regular logged form:

- On profiles with `wal_level = minimal`, the rewrite itself isn't
  WAL-logged either — the whole transition is essentially free I/O-wise.
- On `wal_level = replica` (the default), the rewrite *is* WAL-logged.
  Operators get a warning at boot recommending the minimal setting for
  the duration of an initial sync.

:::tip Operational lever
The single biggest server-side knob on Prep wall-clock time is
`wal_level`. For one-time initial syncs against a non-replicated PG,
flipping to `minimal` for the duration of the sync avoids WAL-logging
both the heap rewrites and the index builds. See
`scripts/postgres-tuning.conf` in the repo.
:::

The flip runs concurrently across tables on a parallel pool (biggest
tables first, so the long rewrites overlap with the rest) because each
table's rewrite is independent. `SET LOGGED` rebuilds every index on
the table inside the ALTER, which is why the pass flips *bare* heaps:
the scaffolding is dropped beforehand and the production set is built
afterwards, so each production index is built exactly once — with full
`max_parallel_maintenance_workers` support and per-index pool fan-out
instead of serially inside each table's ALTER. A side benefit: the
rewrite compacts the dead tuples the backfill UPDATEs left behind, so
no pre-flip `VACUUM` is needed.

## Why ANALYZE twice

The first `ANALYZE` runs immediately after the CTAS rebuild: the planner
needs fresh statistics on the rebuilt input tables before the backfill
`UPDATE`s plan their joins. Autovacuum *will* eventually update stats on
UNLOGGED tables, but its last sample was taken mid-Ingest against tables
that no longer exist in their original form. Without the pass, the
backfills pick Nested Loop plans whose outer-side cardinality is off by
orders of magnitude.

The second `ANALYZE` (per table, after the schema flip) gives Follow's
query planner accurate stats before the first per-block transaction
arrives.

## Timing characteristics

Preparing is bounded by I/O on the CTAS rebuilds, the flip's heap
rewrites, and the index build. Throughput scales with
`max_parallel_maintenance_workers`, `maintenance_work_mem`, and the
pool size (all fixed in `DbSync.Phase.Preparing.Tuning`, sized for the
4-core / 16 GB target deployment).

For a mainnet `everything-profile` sync expect the pass to take tens of
minutes; the wall-clock is dominated by the `tx_in` rebuild, the
`UNLOGGED → LOGGED` heap rewrites, and the production index build (all
proportional to total row count). The per-step `Starting | Completed`
log pairs show where a given run spends its time.
