---
id: repository-layout
title: Repository layout
sidebar_position: 9
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
│   ├── dbsync-tests.cabal   # test-suite + dbsync-testlib helper library
│   └── dbsync-mock/         # mock cardano-node forging primitives
├── profiles/      # preset profile JSON files
├── scripts/       # operator scripts
├── docs/          # this documentation site
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
| `Extractor.hs` + `Extractor/` | The `ExtractorDef` contract, `processBlock` dispatch, and one module per projection (`Core`, `UTxO`, `Pool`, …). |
| `Resolver.hs`, `Writer.hs` | The two cross-phase interfaces extractors are polymorphic over. |
| `Phase/` | The phase state machine (`Type.hs`, `Current.hs`) plus a sub-directory per phase (`Ingest/`, `Preparing/`, `Following/`). |
| `Db/` | Connection management: hasql pool, libpq loader stream, transaction bracket. |
| `Worker/Ledger/` | Optional in-RAM `LedgerDB` and snapshot writer. |
| `Worker/OffChain/` | HTTP fetcher for pool / vote metadata. |
| `Worker/TxOut/` | Per-epoch address-buffer and consumed-by-buffer drainer. |
| `SyncState/` | `dbsync_sync_state` row I/O and resume helpers. |
| `StateQuery/` | LocalStateQuery driver — computes per-block `SlotDetails`. |
| `Trace/` | contra-tracer wiring, watchdog, pulse, timing helpers, replay-progress log. |
| `Metrics.hs` | Prometheus metric definitions (placeholder). |
| `Error.hs`, `Util.hs`, `AppM.hs` | The `AppError` sum, small helpers, the `AppM env` newtype. |

Pattern: phases mirror each other. `Phase/Ingest/`, `Phase/Preparing/`, and
`Phase/Following/` each carry their own `Run.hs`, `Resolver.hs` (where
relevant), `Writer.hs`, and tuning module. When working on one phase, the
other two are useful as parallel references.

## `dbsync-db/` — the schema layer

Three sub-trees under `dbsync-db/src/DbSync/Db/`:

| Directory | What's there |
|---|---|
| `Schema/` | One module per feature group (`Core`, `UTxO`, `Pool`, `Governance`, …) exposing the `TableDef`s for the tables that group owns, plus DDL generation (`Generate.hs`, `Init.hs`, `Migration.hs`, `Version.hs`) and the run-state table (`SyncState.hs`). |
| `Statement/` | hasql statements for the Follow path and the Preparing pass — one module per table, plus cross-cutting ones for the Loader primitives, Backfill, Resolve, Rollback, Indexes, Tuning, and Sequences. |
| `Loader/` | Binary COPY-format encoders. The single biggest determinant of Ingest throughput. |

Also at the top level: `Db/Types.hs` (`DbLovelace` etc.), `Db/Sql.hs` and
`Db/Sql/Refs.hs` (hand-written SQL fragments), `Util/Bech32.hs`,
`Util/DedupHash.hs`.

## `tests/` — the test workspace

Two cabal targets sharing the package:

- **`dbsync-testlib`** under `tests/lib/` — shared helpers: mock-chain
  harness, mock-node server, PG fixtures, hasql helpers, hspec generators,
  invariant properties.
- **`dbsync-test`** under `tests/main/` — the runner.

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

## `profiles/`, `scripts/`, `docs/`

Not Haskell. `profiles/` is the canonical home for shipped profile JSON,
`scripts/` for operator-facing helpers, `docs/` for this site.
