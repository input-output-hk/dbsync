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
| GHC | 9.8.4 |
| cabal-install | ≥ 3.6 |
| PostgreSQL | ≥ 16 |

GHC and cabal are easiest to install via [`ghcup`](https://www.haskell.org/ghcup/);
the platform pages cover the per-OS specifics.

## System libraries

| Library | Required? | Used by |
|---|---|---|
| `snappy` | yes | Compression for the on-disk ledger snapshots. |
| `pkg-config` | yes | Build-time discovery of `snappy` and `libpq`. |
| `libpq` | yes | Bundled with PostgreSQL on most distros. |
| `liburing` | Linux only | The asynchronous block-I/O backend used by the LSM-tree dedup stores and the LedgerDB. |

On macOS (and Windows, and any non-Linux target), `liburing` isn't
available. dbsync detects this at build time and selects a synchronous
fallback automatically — no manual flag required. See the
[macOS page](macos) for the details.

## PostgreSQL

PostgreSQL 16 or newer is required. dbsync uses 16-only features in
the COPY path and the LOGGED/UNLOGGED machinery.

You don't need a separate database account — the connection string is
read from the profile JSON, and a local installation with peer
authentication for the current OS user works for development. For a
production deployment, give dbsync a dedicated user with `CREATEDB`
the first time, then narrow the grants afterwards.

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

## Disk

Rough figures for a mainnet sync. The `everything` row is measured;
the smaller profiles are approximations derived from the
`everything`-profile per-table breakdown. Actual usage depends on
the profile and on PostgreSQL's autovacuum behaviour:

| Profile | Approximate `pg_database_size` at mainnet tip |
|---|---|
| `minimal` | ~30 GB |
| `utxo-only` | ~160 GB |
| `spo` | ~185 GB |
| `dapp` | ~265 GB |
| `everything-no-ledger` | ~475 GB |
| `everything` | ~480 GB |

Plus ~220 GB for the cardano-node database (mostly the ImmutableDB
chain history at mainnet tip). Allow comfortable headroom over these
figures — the bulk-load phase needs working space for index builds
and the LOGGED-flip table rewrite.

The on-disk ledger state (if `ledger.enabled = true`) lives under
whatever directory you pass to `--ledger-state-dir` and runs ~11 GB
at mainnet tip with the LSM backend.

:::tip Big single contributor: `tx_cbor`
The `cbor` extractor stores raw transaction CBOR bytes and accounts
for ~218 GB on its own — roughly half of an `everything`-profile
database. If your consumers don't need to re-serialise or replay
transactions, disabling `cbor` in a custom profile derived from one
of the bigger presets cuts the database nearly in half. See
[Preset profiles](../profiles/presets) for the per-extractor
breakdown.
:::
