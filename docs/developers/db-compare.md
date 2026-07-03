---
id: db-compare
title: Comparing databases
sidebar_position: 11
---

# Comparing databases

`dbsync-compare` checks that this dbsync stores the same chain data as the
original [cardano-db-sync](https://github.com/IntersectMBO/cardano-db-sync) by
comparing two local PostgreSQL databases — an old-implementation snapshot and a
new-implementation sync — table by table and row by row.

It is a **standalone tool**: the CLI and all comparison logic live together
under `tests/compare/`, built as the `dbsync-compare` executable.

| Component | Location |
|---|---|
| Tool (CLI + modules) | [`tests/compare/`](https://github.com/input-output-hk/dbsync/tree/main/tests/compare) |
| Cabal stanza | [`tests/dbsync-tests.cabal`](https://github.com/input-output-hk/dbsync/blob/main/tests/dbsync-tests.cabal) |

## Running it

```bash
cabal build dbsync-compare
cabal run -v0 dbsync-compare -- --old-db dbsync_old --new-db dbsync
```

Both databases are expected on the local PostgreSQL instance. Connection flags
default to the same trust-auth setup the sync uses.

| Flag | Default | Meaning |
|---|---|---|
| `--old-db` | `dbsync_old` | Old-implementation database name |
| `--new-db` | `dbsync` | New-implementation database name |
| `--host` / `--port` | local socket | PostgreSQL host / port |
| `--user` / `--password` | trust auth | PostgreSQL credentials |
| `--max-epoch` | auto | Override the comparison ceiling |
| `--seed` | `42` | Seed for the block sampler (reproducible) |
| `--samples` | `10` | Random blocks sampled per era |
| `--eras` | all | Restrict to a comma list, e.g. `byron,babbage` |
| `--row-counts` | off | Also run the slow epoch-bounded row counts |
| `--no-spot-check` | on | Disable the per-era content check |
| `--no-structure` | on | Disable the schema structure comparison |

## What it checks

### Schema coverage (always)

Classifies every table in the old database as **comparable** (present and
populated on both sides) or **skipped** (absent in new, empty on one side,
new-only, …), and flags schema drift — an old table that maps to nothing in the
new schema. The run exits non-zero if a comparable table regresses.

### Schema structure (default)

Compares the physical shape of every table present on both sides:

- **Columns** — name by name (rename map applied: `dvt_p_p_*` → `dvt_pp_*`),
  with types collapsed to coarse buckets so domain and width differences
  (`lovelace` vs `bigint`) do not register while real shape changes
  (`bytea` vs `text`) do. An old column with no new counterpart fails the
  run; new-only columns are noted informationally. Known intentional
  differences are allowlisted: the inline address columns replaced by
  `address_id`, and protocol-parameter ratios stored as text.
- **Foreign keys** — the old database's FK constraints against the new
  schema's *declared* `ForeignKey` metadata plus any physical constraints
  (the new schema never emits `REFERENCES` constraints). An old FK edge
  with no new counterpart fails.
- **Unique constraints** — the old database's unique indexes/constraints
  against the new schema's declared unique constraints plus physical
  unique indexes (built late, in `PreparingForVolatileTail`).

Disable with `--no-structure`.

### Spot check (default)

Samples `--samples` random blocks per era below the ceiling, looks each one up
**by `block.hash` in both databases**, then walks `block → tx → child tables`
and compares only the stored fields. Surrogate ids are ignored; foreign keys are
translated to the parent's natural key (e.g. `tx_out.address_id` → the address
text, `stake_address_id` → `hash_raw`); every value is cast to text so column
type widening does not register as a difference. Any field or row mismatch is a
hard failure.

Matching on the block hash is what makes the check robust: the two databases sit
at different chain tips and assign different surrogate ids, but a block's hash
and its stored contents are stable. The **ceiling** is `min(old, new)` max epoch
− 1, and only blocks at or below it are sampled, so a mid-sync new database (or
one that is still behind) never causes spurious misses.

:::note Index-independent scoping
The new database builds most secondary indexes late in the sync, so every scope
predicate is phrased over primary-key id ranges (block ids, then tx ids derived
from the cumulative `block.tx_count`). No query depends on a secondary index on
`block_no`, `epoch_no`, or `block_id`, so coverage does not depend on how far
index creation has progressed.
:::

### Row counts (opt-in)

`--row-counts` adds an epoch-bounded `COUNT(*)` per comparable table. It is off
by default because it is slow on the largest tables (100M+ rows).

## Reading the output

```text
== Schema coverage ==
comparable: 59   skipped: 14

== Spot check - per-era content ==
skipped (no index on new): redeemer, datum, script, ...
OK    byron     sampled 10, matched 10
DIFF  babbage   sampled 10, matched 10
    tx_out index/0 data_hash: old=a1b2... new=NULL
content mismatches: 121
```

A `DIFF` line is followed by up to 20 offending rows per table, each naming the
natural key, the field, and the two values.

## Known differences

A clean run is the goal, but some differences are understood and tracked rather
than fixed in the tool. They are catalogued with file/line evidence in
[`Plan/DB-COMPARE-FINDINGS.md`](https://github.com/input-output-hk/dbsync/blob/main/Plan/DB-COMPARE-FINDINGS.md):

| Difference | Status |
|---|---|
| `tx_out.data_hash` NULL for inline datums | New bug — inline-datum extraction not yet wired |
| `stake_address` header byte (`e1` vs `f1`) + `script_hash` | New bug — script-stake credentials not distinguished |
| `slot_leader.description` always `Pool-` | Cosmetic regression — `pool_hash_id` is still correct |
| Byron `tx.fee` = 0 | Not a bug — backfilled when the sync reaches `PreparingForVolatileTail` |

## Extending coverage

To compare another child table, add a `TableSpec` to `tableSpecs` in
[`SpotCheck.hs`](https://github.com/input-output-hk/dbsync/blob/main/tests/compare/DbSync/Compare/SpotCheck.hs):
give its scope column, the natural-key columns, and the value columns —
translating any foreign keys to the parent's natural key. The per-era
rendering picks it up automatically.

## Related pages

- [Testing](testing) — the test suite this tool's library feeds into.
- [Database design](database-design) — the schema both databases implement.
