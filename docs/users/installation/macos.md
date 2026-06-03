---
id: macos
title: macOS
sidebar_position: 3
---

# Installing on macOS

Tested on Apple Silicon. Intel Macs work the same way; one
mithril-bootstrap caveat below.

## Homebrew packages

```bash
brew install \
  pkg-config \
  snappy \
  postgresql@16 \
  ghcup
```

`liburing` is **not** required on macOS. dbsync's I/O backend
(`blockio`) automatically selects a portable synchronous
implementation on non-Linux targets; the `cabal.project` toggles this
via the `+serialblockio` flag on `blockio` when `!os(linux)`. No
operator action needed.

Start PostgreSQL via brew services:

```bash
brew services start postgresql@16
```

The `postgresql@16` formula is keg-only — you may need to put its
binaries on PATH (`brew info postgresql@16` shows the exact line) so
`psql`, `createdb`, etc. resolve.

## Haskell toolchain

```bash
ghcup install ghc 9.8.4
ghcup set ghc 9.8.4
ghcup install cabal latest
```

`ghcup tui` if you want the menu interface.

## PostgreSQL setup

```bash
createuser --createdb dbsync
createdb -O dbsync cexplorer
```

The default macOS Postgres install uses the current OS user as the
superuser; you don't need `sudo`.

## Why no `liburing`

The LSM-tree-backed dedup stores and on-disk LedgerDB use a small
`blockio` shim for asynchronous block I/O. On Linux that wraps
`io_uring`; on macOS the kernel doesn't expose an equivalent, so
`blockio` falls back to a synchronous, syscall-per-operation
implementation.

The fallback is slower than `io_uring` but correct, and the difference
only shows up on the LedgerDB's `lsmRead`/`lsmDuplicate` hot path
during initial sync. On developer machines and small testnets it's
not noticeable.

The flag selection lives in `cabal.project`:

```
if !os(linux)
  package blockio
    flags: +serialblockio
```

so a `cabal build all` Just Works on macOS.

## Mithril bootstrap on Intel Macs

The `scripts/mithril-bootstrap.sh` helper downloads pre-built
`mithril-client` binaries by default. The official Mithril releases
don't ship an Intel-macOS binary; on Intel Macs install via cargo
instead:

```bash
cargo install mithril-client-cli
```

The bootstrap script will then use the cargo-installed binary. On
Apple Silicon the default download path works.

## Next

[Building from source](building).
