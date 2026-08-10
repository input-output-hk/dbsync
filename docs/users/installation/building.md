---
id: building
title: Building from source
sidebar_position: 4
---

# Building from source

dbsync uses a stock cabal workspace. There's no nix flake yet — see
the [Linux](linux) and [macOS](macos) pages for the prerequisites.

## Fetch and build

```bash
git clone https://github.com/input-output-hk/dbsync.git
cd dbsync

cabal update
cabal build all
```

:::info Cold build is slow
The first build pulls the entire cardano-node + cardano-ledger
dependency tree from CHaP (Cardano Haskell Packages) plus a couple
of source-repository-package pins, and compiles GHC's worth of
Haskell. On modern hardware figure **30–60 minutes** for a cold
build; a warm rebuild after a code change is seconds.
:::

## Where the binary lands

After `cabal build all`:

```bash
$ cabal list-bin dbsync
/path/to/dbsync/dist-newstyle/build/aarch64-osx/ghc-9.14.1/dbsync-0.1.0.0/x/dbsync/build/dbsync/dbsync
```

You can run it directly from `cabal run dbsync -- <args>` while
iterating, or symlink the binary into `~/.local/bin/` for production
use.

## Faster rebuilds

Put local overrides in `cabal.project.local`. It is git-ignored, so
your changes stay out of the repository.

```cabal
-- cabal.project.local
program-options
  ghc-options:
    -j

-- Skip -Werror locally (CI still enforces it)
package dbsync
  ghc-options: -Wwarn
package dbsync-db
  ghc-options: -Wwarn
```

## Common build issues

**`Setup: Missing dependency on a foreign library: snappy`**

`snappy` headers aren't on the include path. Install the development
package (`libsnappy-dev` on Debian/Ubuntu, `snappy` on Homebrew) and
ensure `pkg-config --libs snappy` returns successfully.

**`Setup: 'liburing' was not found`** (Linux only)

`liburing-dev` (Debian/Ubuntu) or `liburing-devel` (Fedora) is missing.

:::caution Old kernels
Kernels older than 5.1 do not support `io_uring`. `cabal.project`
selects the `+serialblockio` fallback automatically on non-Linux
targets only, so on Linux you must set it yourself in
`cabal.project.local`:

```cabal
package blockio
  flags: +serialblockio
```

This builds and runs correctly, but the LSM stores and the LedgerDB
are slower.
:::

**`unknown package: cardano-ledger-conway-1.17.3`**

Re-run `cabal update`. CHaP releases sometimes lag a few hours behind
the index-state in `cabal.project`.

## Profiled build

A separate cabal project file enables GHC profiling:

```bash
cabal --project-file=cabal.project.profiling build dbsync
```

The `scripts/profile-dbsync.sh` helper runs the resulting binary with
the right RTS flags for heap and cost-centre profiles. See the script
header for the per-mode walkthrough — useful when investigating
memory or performance regressions.

## Next

[Cardano node setup](../node-setup) is the next step — dbsync needs a
running node to connect to.
