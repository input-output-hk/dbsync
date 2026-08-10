---
id: testing
title: Testing
sidebar_position: 11
---

# Testing

How the test suite is organised and how to run it.

## Layout

Tests live in the [`tests/`](https://github.com/input-output-hk/dbsync/tree/main/tests)
workspace as three cabal targets sharing a single package:

| Target | Path | Role |
|---|---|---|
| `dbsync-testlib` | `tests/lib/` | Shared helpers — mock-chain harness, mock-node server, PG fixtures, hasql helpers, invariant checks. Imported by every spec. |
| `dbsync-test` | `tests/main/` | The runner. Hspec specs grouped by cost. |
| `dbsync-compare` | `tests/compare/` | The schema and row comparison tool. See [Comparing databases](db-compare). |

Plus a separate cabal package:

| Package | Path | Role |
|---|---|---|
| `dbsync-mock` | `tests/dbsync-mock/` | Vendored fork of upstream `Cardano.Mock.Forging.*`. Forges Conway-era blocks through a real ledger state. In its own package so its dependency footprint doesn't land on every unit-test compile. |

## Three tiers

The specs under `tests/main/` are split by cost:

| Tier | Path | What it tests | Needs |
|---|---|---|---|
| Unit | `main/unit/` | Pure code: parsers, extractors against synthetic blocks, DDL generation, config validation. | Nothing. |
| Integration | `main/integration/` | Touches PG but no chain. DDL round-trips, hasql statements, sync-state row, the Preparing pass, the rollback cascade. | A running `dbsync_test` PostgreSQL database. |
| End-to-end | `main/e2e/` | Full app through the mock chainsync server. Ingest → Prep → Follow lifecycle, restart scenarios, LSM persistence, replay-on-boot. | PG, plus the `dbsync-mock` forging chain. |

[`tests/main/Main.hs`](https://github.com/input-output-hk/dbsync/blob/main/tests/main/Main.hs)
wires every spec into one of five top-level `describe` blocks:
`"Unit tests"`, `"Property tests"`, `"Database integration"`,
`"End-to-end"`, and `"Test harness"`. Those names are the `--match`
prefixes.

Per-tier timeouts cap each spec — 30s unit and property, 120s
integration, 300s e2e — so a hang fails the run with a clear message
instead of stalling CI.

:::caution Do not add hspec `parallel`
Integration and e2e specs share the single `dbsync_test` database. The
migration-ladder spec issues `DROP SCHEMA public CASCADE`, so any spec
running beside it would see its tables vanish. This is also why CI runs
`cabal test all -j1`.
:::

## Running

```bash
# Everything
cabal test all

# Unit tests only
cabal test --test-options='--match "Unit tests"'

# One tier
cabal test --test-options='--match "End-to-end"'

# One spec
cabal test --test-options='--match "Database integration/DbSync.Db.Loader"'
```

:::tip
`--match` is hspec's substring matcher against the `describe` / `it`
path, so it must match the spec's own `describe` string — not its
module name. `DbSync.Db.LoaderSpec` matches nothing, because the spec
calls itself `"DbSync.Db.Loader (multi-threaded, full pipeline)"`.
:::

:::note PG access
PostgreSQL must be running, and the `dbsync_test` database must already
exist. **The tests do not create it.** CI provisions it through the
service container's `POSTGRES_DB`.

The name is the hard-coded constant `testDbName` in
[`DbSync.Test.Database`](https://github.com/input-output-hk/dbsync/blob/main/tests/lib/DbSync/Test/Database.hs).
No environment variable overrides it.

Integration and e2e specs create and drop their own *schema* inside
that database.
:::

## Shared helpers

A handful of modules under
[`DbSync.Test.*`](https://github.com/input-output-hk/dbsync/tree/main/tests/lib/DbSync/Test)
do the heavy lifting:

| Module | Role |
|---|---|
| `AppHarness` | Build an `AppArgs` from a `MockNode` so tests can call `runApp` directly. Config builders: `defaultTestConfig`, `ledgerEnabledTestConfig`, `configWithExtractors`, `allImplementedExtractors`. Also `withTempDir`, `waitForSyncComplete`, and the tracers. |
| `Database` | Test-DB lifecycle, `truncateAllTables`, the hard-coded `dbsync_test` connection settings. |
| `MockChain` | Forge Conway-era blocks through a real `Interpreter` and push them through `parseBlock` and the consumer pipeline. `forgeNextBlock`, `forgeNextBlocks`, `forgeUntilNextEpoch`, `buildRealisticTxs`. |
| `MockNode` | Vendored `Cardano.Mock.ChainSync.Server`. Drives the full ChainSync socket path for e2e tests. |
| `MockNode.Workload` | Pre-built block workloads for driving a mock node. |
| `PgAssertions` | Helpers like `countRows`, `countNulls`, `sequenceAdvanced`. |
| `PipelineEnv` | In-process `IdResolver` and a collecting `Writer` for unit tests on extractor bodies: `mkTestPipelineEnv`, `mkTestPipelineEnvOn`, `mkTestPipelineEnvWith`. |
| `Property.Invariants` | Pure pipeline runners: `runPureExtract`, `runPureExtractMany`, `syntheticSlotDetails`. Touches no PG. |
| `RecomputeInvariants` | The PG-state invariant checks: `epochFinalizedDriftCount`, `blockTxCountDriftCount`, `txOutSumDriftCount`, `duplicateEpochRowGroupCount`, `epochContiguityGapCount`, `consumedByDriftCount`. |
| `EpochRegression` | Epoch-boundary regression checks. |
| `Fixtures` | Sample genesis files, pre-forged blocks, the Conway test config. |
| `Lsm` | LSM-tree helpers for tests that touch the dedup stores or the UTxO store. |
| `Copy` | COPY-encoder helpers. |
| `E2E` | Common bootstrap for the e2e specs: `withAppSession`, `withAppSessionResume`. |
| `Hasql` | `withTestConnection`, `runStatement`, `runSession`. |
| `Helpers` | `waitFor`. |
| `Writer` | Lifecycle helpers for spinning up a Writer in isolation. |

## Test-fixture chains

The `dbsync-mock` package vendors enough of upstream cardano-chain-gen
to forge synthetic chains in any era. Tests use this to produce blocks
with specific tx shapes (stake registrations, pool registrations,
collateral, multi-asset minting, governance votes) without depending
on a real testnet.

The mock chain runs against a real ledger state, so any ledger-derived
data (rewards, epoch_stake, ada_pots) that the test wants to assert on
is computed by the same code that runs against mainnet.

See
[`tests/lib/DbSync/Test/MockChain.hs`](https://github.com/input-output-hk/dbsync/blob/main/tests/lib/DbSync/Test/MockChain.hs)
for the forging API; the e2e specs in
[`tests/main/e2e/DbSync/Phase/`](https://github.com/input-output-hk/dbsync/tree/main/tests/main/e2e/DbSync/Phase)
show end-to-end patterns.

## Writing a test for a new extractor

The path of least resistance for a new extractor:

1. **Unit test the block-to-rows function** in
   `tests/main/unit/DbSync/Extractor/<Name>Spec.hs`. Build a
   `GenericBlock`, run the extractor under `mkTestPipelineEnv`, and
   assert on the captured `Writer` calls. `runPureExtract` from
   `DbSync.Test.Property.Invariants` wraps that pattern.
2. **Add an e2e scenario** to an existing spec, or a new one under
   `tests/main/e2e/`, covering the round-trip through Ingest, Prep, and
   Follow. `forgeNextBlocks` and `forgeUntilNextEpoch` from `MockChain`
   are the primitives. Combine them with `withAppSession` and
   `waitForSyncComplete` to drive a config that enables your extractor.
3. **Add invariants** if your extractor touches anything cross-cutting
   such as foreign keys, dedup, or rollback. The checks in
   `DbSync.Test.RecomputeInvariants` recompute a value from the
   database and count the rows that disagree.

Conway is the default mock-chain era; older-era specs are explicit
about which era's primitives they use.

## CI

CI runs every tier in **one job on one machine**, as
`cabal test all -j1`, against a PostgreSQL service container. There is
no tier split. The full pass is the merge gate.

Run the same command locally. A plain `cabal test all` without `-j1`
is a known source of flakes, for the reason given above.
