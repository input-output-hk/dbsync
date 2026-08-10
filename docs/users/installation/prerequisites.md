---
id: prerequisites
title: Prerequisites
sidebar_position: 1
---

# Prerequisites

What you need installed before building or running dbsync.

## Toolchain

| Tool | Version |
|---|---|
| GHC | 9.14.1 |
| cabal-install | 3.16.1.0 |
| PostgreSQL | ≥ 16 (18 recommended) |

These GHC and cabal versions are the known-good pair CI builds
against; other combinations may fail to solve against the pinned
`index-state`. Install both via [`ghcup`](https://www.haskell.org/ghcup/);
the platform pages cover the per-OS specifics.

## System libraries

| Library | Required? | Used by |
|---|---|---|
| `snappy` | yes | Compression in the LSM-tree stores. Required even with `ledger.enabled = false`, because the Ingest dedup and UTxO stores use them too. |
| `pkg-config` | yes | Build-time discovery of `snappy` and `libpq`. |
| `libpq` | yes | Bundled with PostgreSQL on most distros. |
| `liburing` | Linux only | The asynchronous block-I/O backend for the LSM-tree stores and the LedgerDB. |

On macOS, and on any non-Linux target, `liburing` does not exist.
dbsync detects this at build time and selects a synchronous fallback.
You need no manual flag. See the [macOS page](macos).

## PostgreSQL

Use **PostgreSQL 18**. CI builds and tests against it, and the
installation pages below target it.

dbsync performs no version check, so an older server may work. Nothing
in the codebase establishes a hard floor.

**dbsync does not create the database.** You create it, then dbsync
creates the tables inside it. The user therefore needs `CONNECT` on
the database plus `CREATE` and `USAGE` on the `public` schema. It does
not need `CREATEDB`.

For development, a local install with peer authentication for your OS
user works with no extra setup.

:::tip Read the tuning config first
`scripts/postgres-tuning.conf` in the repository documents the
server-side settings that materially affect bulk-sync performance
(`wal_level = minimal`, `max_wal_size`, `shared_buffers`,
`maintenance_work_mem`, parallelism). Worth the few minutes before a
multi-day mainnet sync.
:::

## Cardano node

A running `cardano-node` reachable over its Unix socket. Any
n2c-supporting node works — see [Cardano node setup](../node-setup)
for how to get one, including pre-built binaries, Docker, Nix, and
source builds via the official Cardano docs.

## Disk space

:::caution These are observations, not guarantees
The figures below come from one `everything` sync against mainnet.
The other rows are derived from its per-table breakdown. Your result
will differ with the network, the chain's growth since these were
taken, and PostgreSQL's autovacuum behaviour.

Size the disk from the ordering, not from the absolute numbers, and
leave generous headroom.
:::

Approximate `pg_database_size` at mainnet tip:

| Preset | Size |
|---|---|
| `minimal` | ~30 GB |
| `utxo-only` | ~160 GB |
| `spo` | ~185 GB |
| `dapp` | ~265 GB |
| `everything-no-ledger` | ~475 GB |
| `everything` | ~480 GB |

Budget for three more things on top:

- **The cardano-node database**, roughly 220 GB at mainnet tip, mostly
  the ImmutableDB chain history.
- **Working space for `PreparingForVolatileTail`**, which builds
  indexes and rewrites every table for the LOGGED flip.
- **The on-disk ledger state**, if `ledger.enabled = true`. It lives
  under `--ledger-state-dir` and runs about 11 GB at mainnet tip.

:::tip `cbor` dominates the total
The `cbor` extractor stores raw transaction CBOR. It accounts for
roughly 218 GB on its own, about half of an `everything` database.

Disable `cbor` unless your consumers re-serialise or replay
transactions. It nearly halves the database. See
[Preset configs](../config/presets).
:::
