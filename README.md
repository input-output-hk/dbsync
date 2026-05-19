# DbSync

A Haskell ground-up reimplementation of `cardano-db-sync` targeting a ~4× faster
genesis-to-tip sync via decoupled COPY-based parallel extraction.


## Modularity & profiles

dbsync is built around a **projection system**. Each feature — UTxO, metadata,
multi-asset, stake delegation, pool, scripts/datums, governance, CBOR, ... — is
an independent extractor that maps `GenericBlock → [TableRows]`. A **profile**
is a single JSON file (passed via `--profile`) that toggles which extractors
run, controlling both the work done *and* the schema created: disabling a
projection actually skips that work and those tables, not just a runtime flag.

The `core` extractor (block / tx / slot_leader) is always on; every other
extractor opts in via `"db_options"`. A profile is **immutable once the DB is
created** — changing options means re-syncing from scratch.

```jsonc
// profiles/spo-profile.json (excerpt)
"db_options": {
  "utxo":             true,
  "metadata":         true,
  "stake_delegation": true,
  "pool":             true
}
```

Six presets ship in `profiles/`, ordered roughly by data volume:

| Profile                   | Use case                                | Adds (on top of `core`)                                            | COPY conns |
|---------------------------|-----------------------------------------|--------------------------------------------------------------------|------------|
| `minimal`                 | Block / tx index only                   | —                                                                  | 4          |
| `utxo-only`               | Wallets, balances                       | `utxo`                                                             | 6          |
| `spo`                     | Stake-pool operators                    | `utxo`, `metadata`, `stake_delegation`, `pool`                     | 8          |
| `dapp`                    | DApps / explorers                       | `utxo`, `multi_asset`, `metadata`, `scripts_datums`                | 8          |
| `everything-no-ledger`    | Full block-derived data, no ledger      | all block-extractable (`+ governance`, `+ cbor`)                   | 12         |
| `everything`              | Original cardano-db-sync parity         | all of the above + ledger-derived (rewards, `ada_pots`, ...)       | 12         |

Copy `profiles/everything-profile.json` and trim it to build your own.



## Prerequisites

GHC 9.8.4, cabal ≥ 3.6, PostgreSQL ≥ 16, snappy, pkg-config. Linux benefits from
`liburing-dev` (LSM uses `io_uring` when available; macOS falls back to a serial
implementation automatically via the `+serialblockio` flag).

```bash
# Linux (Debian/Ubuntu)
sudo apt install postgresql-16 libsnappy-dev liburing-dev pkg-config
# Haskell toolchain via ghcup: https://www.haskell.org/ghcup/
```

```bash
# macOS (Apple Silicon)
brew install postgresql@16 snappy pkg-config
# Haskell toolchain: brew install ghcup
```

## Setup

### 1. Build `cardano-node` from the `early-n2c` fork

Stock cardano-node only opens its node-to-client (n2c) Unix socket after the
full LedgerDB replay finishes — on mainnet that's hours of dbsync sitting idle.
The `early-n2c` fork binds the socket within seconds of startup so dbsync can
begin consuming immutable blocks in parallel with the node's ledger replay.

```bash
git clone --branch early-n2c https://github.com/Cmdv/cardano-node.git
cd cardano-node
cabal install cardano-node \
  --installdir=$HOME/.local/bin \
  --overwrite-policy=always
```

### 2. Bootstrap the node DB with Mithril

The script downloads a verified ImmutableDB+VolatileDB snapshot so the node
skips network sync. It will install `mithril-client` for you on first run
(pass `-y` to skip the prompt).

```bash
./scripts/mithril-bootstrap.sh --db-dir ~/cardano/mainnet
```

### 3. Build dbsync

```bash
cabal build all
```

### 4. Create the Postgres database

```bash
createdb cexplorer   # name matches profiles/everything-profile.json
```

## Running

With zellij installed, one command gives you cardano-node and dbsync side by
side in a split pane:

```bash
scripts/run-everything-zellij.sh
```

Without zellij, run these two commands in two separate terminals — this is
exactly what the script above generates inside its layout heredoc:

```bash
# Terminal 1 — cardano-node
cd $CARDANO_NODE_DIR
cardano-node run \
  --config        $TESTNET_DIR/config.json \
  --database-path $TESTNET_DIR/db/ \
  --socket-path   $TESTNET_DIR/db/node.socket \
  --host-addr     0.0.0.0 \
  --port          1337 \
  --topology      $TESTNET_DIR/topology.json \
  +RTS -N -A64m -RTS
```

```bash
# Terminal 2 — dbsync
cabal run dbsync -- \
  --db-sync-config   $TESTNET_DIR/db-sync-config.json \
  --socket-path      $TESTNET_DIR/db/node.socket \
  --ledger-state-dir $TESTNET_DIR \
  --profile          profiles/everything-profile.json
```

Env vars used by the helper script: `CARDANO_NODE_DIR`, `TESTNET_DIR`, `PROFILE`
(defaults: `$HOME/Code/IOG/cardano-node`, `$HOME/Code/IOG/testnet`,
`profiles/everything-profile.json`).


## Architecture

```
                ┌─────────────────────┐
                │    cardano-node     │
                │   (early-n2c fork)  │
                │   ImmutableDB +     │
                │   VolatileDB +      │
                │   LedgerDB (LSM)    │
                └──────────┬──────────┘
                           │ Unix socket (n2c ChainSync)
                           ▼
┌──────────────────────────────────────────────────────────────────────┐
│                              dbsync                                  │
│                                                                      │
│  Pipeline — concurrent stages, TBQueue-bounded, same in all phases:  │
│                                                                      │
│   ┌─────────┐   ┌────────┐   ┌──────────────┐                        │
│   │Receiver │══▶│ Parser │══▶│ Extractors   │                        │
│   │ChainSync│   │HFC eras│   │ Core, UTxO,  │                        │
│   │ client  │   │        │   │ MultiAsset,  │                        │
│   └─────────┘   └────────┘   │ Pool, Gov, … │                        │
│                              └──────┬───────┘                        │
│                                     │                                │
│                  ┌──────────────────┴─────────────┐                  │
│                  ▼                                ▼                  │
│         ┌─────────────────┐            ┌─────────────────┐           │
│         │  COPY Writers   │            │  hasql Writer   │           │
│         │ ║ 12× parallel  │            │ INSERT + SELECT │           │
│         │  conns (1/tbl), │            │ per-block txn,  │           │
│         │  UNLOGGED tbls; │            │ single-threaded;│           │
│         │  IDs pre-       │            │  IDs returned   │           │
│         │  assigned       │            │   by the DB     │           │
│         └────────┬────────┘            └────────┬────────┘           │
│                  │                              │                    │
│         IngestChainHistory         PreparingForChainTip +            │
│                                    FollowingChainTip                 │
│                                                                      │
│  Side channels (parallel with the pipeline):                         │
│   • Ledger Worker     — optional, off critical path                  │
│   • OffChain Fetcher  — HTTP pool / vote metadata                    │
└──────────────────┬──────────────────────────────┬────────────────────┘
                   ▼                              ▼
           ┌──────────────────────────────────────────────┐
           │                 PostgreSQL                   │
           │    UNLOGGED → LOGGED at phase transition     │
           └──────────────────────────────────────────────┘

Lifecycle:
  IngestChainHistory  →  PreparingForChainTip  →  FollowingChainTip
  COPY, UNLOGGED,        One-time DDL: build       hasql INSERT/SELECT
  epoch-aligned          indexes, ALTER LOGGED,    per block, DB IDs,
  commits, in-mem IDs    ANALYZE                   rollback-safe
```

The same pipeline runs in every phase — `Receiver → Parser → Extractors →
Writer`. Only the **Writer** swaps between phases, and with it the strategy for
obtaining row IDs:

- **`IngestChainHistory`** writes through 12 parallel `COPY` streams (one libpq
  connection per table) into UNLOGGED tables with epoch-aligned commits.
  Because `COPY` has no return channel for generated IDs, monotonic IDs are
  pre-assigned in memory by `Phase/Ingest/DedupMap` + `Phase/Ingest/Counter`
  *before* the row is written.
- **`PreparingForChainTip` and `FollowingChainTip`** share a single `hasql`
  writer doing `INSERT … RETURNING` for new rows and `SELECT` for existing
  parent IDs — IDs come back as part of the query/insert response itself, not
  from a separate resolver step. Single-threaded so rollbacks under volatile-
  block churn stay correct.

Beyond the main pipeline, two side channels run concurrently throughout: the
optional **Ledger Worker** (kept off the critical path during Ingest) and the
**OffChain Fetcher** (HTTP fetches for pool / vote metadata).

## Repository layout

```
.
├── dbsync-db/        # Shared DB layer: schema types, DDL generation, COPY encoders, hasql statements
├── dbsync-smash/     # SMASH stake-pool metadata server (stub)
├── profiles/         # Projection profiles — which extractors run; immutable per DB
├── scripts/          # mithril-bootstrap.sh + run-everything-zellij.sh
├── cabal.project     # Workspace + CHaP + cardano-node fork pin
│
├── tests/            # All tests live here
│   ├── dbsync-tests.cabal   # Test-suite package (library dbsync-testlib + test-suite dbsync-test)
│   ├── lib/                 # Shared test helpers (dbsync-testlib)
│   ├── main/                # hspec test runner + *Spec.hs modules
│   ├── fixtures/            # Sample configs / golden files
│   ├── data/                # Larger test data (Conway mock-chain config)
│   └── dbsync-mock/         # Vendored Cardano.Mock forging primitives (separate cabal package)
│
└── dbsync/           # The sync engine
    ├── app/          # Executable entrypoint (Main.hs)
    └── src/DbSync/
        ├── App.hs / AppM.hs / Cli.hs / Env.hs / Error.hs / Util.hs   # Top-level wiring & CLI
        ├── App/                          # Orchestrator: boot decision, run loop, AppArgs
        ├── Phase/                        # Phase state machine — SyncPhase type, live Current carrier,
        │                                 #   Ingest/ (incl. Counter, DedupMap) / Preparing / Following
        ├── Block/                        # ChainSync receiver + HFC-era-dispatching block/tx parser
        ├── Extractor.hs + Extractor/     # Pure GenericBlock → [Row] projections (Core, UTxO, Pool, …)
        ├── Db/                           # Loader-stream COPY writer + hasql pool + transaction bracket
        ├── Writer.hs                     # hasql INSERT/SELECT writer interface (Preparing/Following path)
        ├── Address/                      # Async address-resolution worker (fills tx_out.address_id)
        ├── Resolver.hs                   # IdResolver interface shared by Ingest and Following extractors
        ├── StateQuery.hs + StateQuery/   # LocalStateQuery driver + observed-summary cache
        ├── Checkpoint/                   # Epoch-aligned checkpoint manager + resume logic
        ├── Ledger/                       # Optional ledger-state worker (off the critical path) +
        │                                 #   era-collapsed projections (Rewards, EpochUpdate,
        │                                 #   ProtoParams, StakeDist)
        ├── OffChain/                     # Off-chain metadata fetchers (pool / vote)
        ├── Node/                         # Unix-socket node connection
        ├── Config.hs + Config/           # Profile + node-config parsing & validation
        ├── Trace.hs + Trace/             # contra-tracer setup and structured logging
        └── Metrics.hs                    # Prometheus metric definitions
```
