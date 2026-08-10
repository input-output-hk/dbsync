---
id: repository-layout
title: Repository layout
sidebar_position: 13
---

# Repository layout

A map to find your way around. Per-module detail lives in the modules
themselves — start here, then `grep` or open the source.

## Workspace

```
.
├── dbsync/        # the sync engine (executable + library)
├── dbsync-db/     # shared schema layer: types, DDL, encoders, statements
├── dbsync-smash/  # SMASH stake-pool metadata server (stub)
├── tests/
│   ├── dbsync-tests.cabal   # test-suite, dbsync-testlib, dbsync-compare
│   ├── dbsync-mock/         # mock cardano-node forging primitives
│   ├── compare/             # the dbsync-compare tool
│   ├── data/ fixtures/      # test inputs
│   └── lib/ main/           # helper library and the runner
├── config-examples/  # preset config JSON files + pg-config example
├── scripts/       # operator scripts
├── docs/          # this documentation site
├── Plan/          # design notes and landed-work records
└── cabal.project  # workspace + CHaP pin
```

Build with `cabal build all` from the workspace root. The `dbsync` executable
runs via `cabal run dbsync`.

## `dbsync/` — the engine

Source under `dbsync/src/DbSync/`. Grouped by where the code sits in the
runtime call graph; the directory names tend to map cleanly onto the
[architecture](architecture) page.

| Directory | What's there |
|---|---|
| `App/` | Boot, CLI, config parsing, env records (`CoreEnv`, `IngestEnv`, `FollowEnv`), `runApp` orchestration. Start here when reading top-down. |
| `ChainSync/` | n2c socket connection and the `ChainSyncMsg` queue type. |
| `Parser/` | Era dispatch + `GenericBlock` / `GenericTx` definitions. |
| `Extractor.hs` + `Extractor/` | The `ExtractorDef` contract, `processBlock` dispatch, `Registry.hs` (the source of truth for which extractors exist), `SharedDedup.hs`, and one module per extractor (`Core`, `UTxO`, `Pool`, …). |
| `Resolver.hs`, `Writer.hs` | The two cross-phase interfaces extractors are polymorphic over. |
| `Phase/` | The phase state machine (`Type.hs`, `Current.hs`) plus a sub-directory per phase (`Ingest/`, `Preparing/`, `Following/`). |
| `Db/` | Connection management: hasql pool, libpq loader stream, transaction bracket. |
| `Worker/Ledger/` | Optional on-disk LSM-backed `LedgerDB` and snapshot writer. |
| `Worker/OffChain/` | HTTP fetcher for pool / vote metadata. |
| `Worker/TxOut/` | Per-epoch address-buffer and consumed-by-buffer drainer. |
| `SyncState/` | `dbsync_sync_state` row I/O and resume helpers. |
| `StateQuery/` | LocalStateQuery driver — computes per-block `SlotDetails`. |
| `Trace/` | contra-tracer wiring (`Backend.hs`, `Types.hs`), timing helpers, replay-progress log. |
| `Schema/` | `Version.hs` — the schema version and fingerprint this binary targets. |
| `StateQuery.hs`, `Trace.hs`, `App.hs` | Top-level re-export modules. |
| `Metrics.hs` | Metric record. Placeholder: nothing writes to it. |
| `Error.hs`, `Error/Render.hs`, `Util.hs`, `AppM.hs` | The `AppError` sum, crash rendering, small helpers, the `AppM env` newtype. |

Pattern: phases mirror each other. `Phase/Ingest/`, `Phase/Preparing/`, and
`Phase/Following/` each carry their own `Run.hs`, `Resolver.hs` (where
relevant), `Writer.hs`, and tuning module. When working on one phase, the
other two are useful as parallel references.

## `dbsync-db/` — the schema layer

Three sub-trees under `dbsync-db/src/DbSync/Db/`:

| Directory | What's there |
|---|---|
| `Schema/` | One module per domain group (`Core`, `UTxO`, `Pool`, `Governance`, …) exposing the `TableDef`s that group owns, plus DDL generation (`Generate.hs`, `Init.hs`, `Migration.hs`) and the run-state table (`SyncState.hs`). Note `Version.hs` is **not** here — it lives in the `dbsync` package. |
| `Statement/` | hasql statements for the Follow path and the Preparing pass. One module per domain group, mirroring `Schema/`, plus cross-cutting ones for the Loader primitives, Indexes, Sequences, Constraints, and Tuning. `Statement/Worker/` holds Resolve, Backfill, EpochParamPending, and Rollback. |
| `Loader/` | COPY-**text** encoders: tab-separated, `\N` for NULL. The single biggest determinant of Ingest throughput. |

Also at the top level: `Db/Types.hs` (`DbLovelace` etc.), `Db/Sql.hs` and
`Db/Sql/Refs.hs` (hand-written SQL fragments), `Util/Bech32.hs`,
`Util/DedupHash.hs`.

## `tests/` — the test workspace

Three cabal targets sharing the package:

- **`dbsync-testlib`** under `tests/lib/` — shared helpers: mock-chain
  harness, mock-node server, PG fixtures, hasql helpers, invariant checks.
- **`dbsync-test`** under `tests/main/` — the runner.
- **`dbsync-compare`** under `tests/compare/` — the schema and row
  comparison tool. See [Comparing databases](db-compare).

Specs tier by cost:

| Tier | Path | What it tests |
|---|---|---|
| Unit | `main/unit/` | Pure code. No PG, no chain. Fast. |
| Integration | `main/integration/` | Touches PG but no chain (DDL, hasql round-trips, checkpoints). |
| End-to-end | `main/e2e/` | Full app through the mock chainsync server. Slowest. |

Filter with `cabal test --test-options='--match "Unit tests"'`.

## `tests/dbsync-mock/` — vendored mock-chain primitives

Reduced fork of upstream `Cardano.Mock.Forging.*`. Lets tests forge txs in
each era and push them through a mock chainsync server. Lives in its own
cabal package because its dependency footprint is large enough that you
don't want it on every unit-test compile.

## `config-examples/`, `scripts/`, `docs/`

Not Haskell. `config-examples/` is the canonical home for shipped
config JSON (the six presets plus `pg-config.example.json`),
`scripts/` for operator-facing helpers, `docs/` for this site.
