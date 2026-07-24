# dbsync

[![Docs](https://github.com/input-output-hk/dbsync/actions/workflows/docs.yml/badge.svg)](https://github.com/input-output-hk/dbsync/actions/workflows/docs.yml)
[![GHC](https://img.shields.io/badge/GHC-9.14.1-purple)](https://www.haskell.org/ghc/)
[![Cabal](https://img.shields.io/badge/cabal--install-3.16.1.0-purple)](https://www.haskell.org/cabal/)
[![License: Apache-2.0](https://img.shields.io/badge/License-Apache--2.0-blue.svg)](LICENSE)

A fast, modular indexer for the Cardano blockchain. dbsync follows a Cardano
node and projects on-chain data into a PostgreSQL schema you control table by
table via **profiles**.

## Documentation

| | |
|---|---|
| **[User documentation](https://input-output-hk.github.io/dbsync/users/intro)** | Install, configure, run, and operate dbsync. |
| **[Developer documentation](https://input-output-hk.github.io/dbsync/developers/intro)** | Architecture, extractors, contribution guide. |

## Quick start

Requires **GHC 9.14.1** and **cabal-install 3.16.1.0** — install both via
[ghcup](https://www.haskell.org/ghcup/). Other combinations may fail to solve
against the pinned `index-state`.

```bash
git clone https://github.com/input-output-hk/dbsync.git
cd dbsync
cabal build all
```

You'll also need PostgreSQL ≥ 16 and a running `cardano-node`. See the
[user docs](https://input-output-hk.github.io/dbsync/users/intro) for the full
setup, prerequisites by platform, and how to choose a profile.

## Repository layout

- `dbsync/` — the sync engine
- `dbsync-db/` — schema types, DDL, COPY encoders, hasql statements
- `dbsync-smash/` — SMASH stake-pool metadata server
- `tests/` — test suites and `dbsync-mock` (mock-chain forging primitives)
- `profiles/` — preset profile JSON files
- `docs/` — Docusaurus source for the documentation site

## License

Apache-2.0. See [LICENSE](LICENSE).
