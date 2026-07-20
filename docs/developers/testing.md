---
id: testing
title: Testing
sidebar_position: 8
---

# Testing

How the test suite is organised and how to run it.

## Layout

Tests live in the [`tests/`](https://github.com/input-output-hk/dbsync/tree/main/tests)
workspace as two cabal targets sharing a single package:

| Target | Path | Role |
|---|---|---|
| `dbsync-testlib` | `tests/lib/` | Shared helpers — mock-chain harness, mock-node server, PG fixtures, hasql helpers, hspec generators, invariant properties. Imported by every spec. |
| `dbsync-test` | `tests/main/` | The runner. Hspec specs grouped into three tiers. |

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
wires each spec into one of three top-level `describe` blocks
matching the directories. Per-tier timeouts cap individual specs (30s
unit, 120s integration, 300s e2e) so a hang fails the run with a
clear message instead of stalling CI.

## Running

```bash
# Everything
cabal test all

# Unit tests only
cabal test --test-options='--match "Unit"'

# A single integration spec
cabal test --test-options='--match "Database integration/DbSync.Db.LoaderSpec"'

# Single tier
cabal test --test-options='--match "End-to-end"'
```

:::tip
The `--match` filter is hspec's substring matcher against the
`describe` / `it` paths. Set it to whatever uniquely identifies the
spec you want — including spaces and slashes.
:::

:::note PG access
PostgreSQL needs to be running and the user invoking `cabal test`
needs `CREATEDB`. Integration and e2e specs create and tear down
their own schema against the `dbsync_test` database (configurable
via the `DBSYNC_TEST_DB` environment variable). See
[`DbSync.Test.Database`](https://github.com/input-output-hk/dbsync/blob/main/tests/lib/DbSync/Test/Database.hs).
:::

## Shared helpers

A handful of modules under
[`DbSync.Test.*`](https://github.com/input-output-hk/dbsync/tree/main/tests/lib/DbSync/Test)
do the heavy lifting:

| Module | Role |
|---|---|
| `AppHarness` | Build an `AppArgs` from a `MockNode` so tests can call `runApp` directly. Pre-baked profiles (`minimalProfile`, `defaultTestProfile`, `ledgerEnabledTestProfile`). |
| `Database` | Test-DB lifecycle, `truncateAllTables`, the `dbsync_test` connection settings. |
| `MockChain` | Forge Conway-era blocks through a real `Interpreter`, push them through `parseBlock` + the consumer pipeline. Used by integration specs that need ledger-derived data. |
| `MockNode` | Vendored `Cardano.Mock.ChainSync.Server`. Drives the full ChainSync socket path for e2e tests. |
| `PgAssertions` | Helpers like `countRows`, `countNulls`, `sequenceAdvanced`. |
| `PipelineEnv` | In-process `IdResolver` + collecting `Writer` for unit tests on extractor bodies. |
| `Generators` | QuickCheck generators for parser types. |
| `Property.Invariants` | Shared property-test invariants. |
| `Fixtures` | Sample genesis files, pre-forged blocks, the Conway test config. |
| `Lsm` | LSM-tree helpers for tests that touch the dedup stores or UTxO store. |
| `E2E` | Common bootstrap for the e2e specs. |
| `Hasql` | Connection helpers and `truncateAll`. |
| `Helpers` | Misc: `waitFor`, temp directories. |
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

1. **Unit test the pure projection** in `tests/main/unit/DbSync/Extractor/<Name>Spec.hs`.
   Build a `GenericBlock` directly with `DbSync.Test.Generators`,
   run the extractor under `PipelineEnv`, assert on the captured
   `Writer` calls.
2. **Add an e2e scenario** to an existing spec (or a new one under
   `tests/main/e2e/`) covering the round-trip through Ingest + Prep +
   Follow. The `MockChain` harness's `forgeNextBlocks` and
   `fillEpochs` are the primitives; combine them with
   `withTestDatabase` and `waitForSyncComplete` to drive a profile
   that includes your extractor.
3. **Add invariants** if your extractor touches anything cross-cutting
   (FKs, dedup, rollback). The properties in
   `DbSync.Test.Property.Invariants` run against every PG state the
   tests produce.

Conway is the default mock-chain era; older-era specs are explicit
about which era's primitives they use.

## CI

CI runs `cabal test all` against the same profile-bearing config a
local run uses. Unit + Property tiers run first; integration and e2e
follow on machines with PG installed. The full pass is the gate for
merging.
