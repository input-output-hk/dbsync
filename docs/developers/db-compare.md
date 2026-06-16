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

It is **library-first**: the comparison logic lives in the `dbsync-compare-lib`
sublibrary so the test suite can assert on its results, and a thin CLI wraps it
for ad-hoc runs.

| Component | Location |
|---|---|
| Library | [`tests/compare-lib/DbSync/Compare/`](https://github.com/input-output-hk/dbsync/tree/main/tests/compare-lib/DbSync/Compare) |
| CLI | [`tests/compare/Main.hs`](https://github.com/input-output-hk/dbsync/blob/main/tests/compare/Main.hs) |
| Cabal stanzas | [`tests/dbsync-tests.cabal`](https://github.com/input-output-hk/dbsync/blob/main/tests/dbsync-tests.cabal) |

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

## What it checks

### Schema coverage (always)

Classifies every table in the old database as **comparable** (present and
populated on both sides) or **skipped** (absent in new, empty on one side,
new-only, …), and flags schema drift — an old table that maps to nothing in the
new schema. The run exits non-zero if a comparable table regresses.

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

:::note Index-aware coverage
The new database builds most secondary indexes late in the sync. A per-tx query
against an unindexed child table would sequentially scan a 100M-row table, so the
spot check first introspects the new database and **skips** any child table whose
scope column is not yet indexed, listing them under `skipped (no index on new)`.
Coverage widens automatically as the sync finishes building indexes.
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

To compare another child table, add a `ChildSpec` to `childSpecs` in
[`SpotCheck.hs`](https://github.com/input-output-hk/dbsync/blob/main/tests/compare-lib/DbSync/Compare/SpotCheck.hs):
give its scope column (must be indexed on the new database), the natural-key
columns, and the value columns — translating any foreign keys to the parent's
natural key. The index filter and per-era rendering pick it up automatically.

## Related pages

- [Testing](testing) — the test suite this tool's library feeds into.
- [Database design](database-design) — the schema both databases implement.
