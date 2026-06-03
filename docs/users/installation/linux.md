---
id: linux
title: Linux
sidebar_position: 2
---

# Installing on Linux

Tested on Ubuntu 22.04+ and Debian 12+. Other distributions work; the
package names below will differ on Fedora/Arch/NixOS.

## System packages (Debian/Ubuntu)

```bash
sudo apt update
sudo apt install \
  build-essential \
  pkg-config \
  libpq-dev \
  libsnappy-dev \
  liburing-dev \
  postgresql-16 \
  zlib1g-dev \
  libtinfo-dev \
  libgmp-dev \
  curl
```

:::info Why liburing
`liburing-dev` is the asynchronous-I/O backend used by the LSM-based
dedup stores and the on-disk LedgerDB. It's available on every
kernel ≥ 5.1, which is every Debian/Ubuntu release currently in
support. The build falls back to `+serialblockio` if it's missing,
but the LedgerDB is noticeably slower without it.
:::

`postgresql-16` is the server package; the client tools (`psql`) come
with it. If your distro defaults to an older PostgreSQL, see the
[PostgreSQL APT repository](https://wiki.postgresql.org/wiki/Apt) for
the official PG 16 packages.

## Fedora / RHEL

```bash
sudo dnf install \
  gcc gcc-c++ make \
  pkgconf-pkg-config \
  postgresql-server postgresql-devel \
  snappy-devel \
  liburing-devel \
  zlib-devel ncurses-devel gmp-devel \
  curl
```

Initialise PostgreSQL with `sudo postgresql-setup --initdb` and start
the service.

## Haskell toolchain

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Then install the required versions:

```bash
ghcup install ghc 9.8.4
ghcup set ghc 9.8.4
ghcup install cabal latest
```

`ghcup tui` is the easier way to manage versions if you have several
GHC installs side by side.

## PostgreSQL setup

Create a database role and a database for dbsync to use. The profile
JSON's `database` block will reference them:

```bash
sudo -u postgres createuser --createdb --pwprompt dbsync
sudo -u postgres createdb -O dbsync cexplorer
```

`cexplorer` is the conventional database name (matching upstream
cardano-db-sync); change it freely.

## NixOS

The repository targets a recent CHaP snapshot and a pinned
cardano-node fork, so `nix develop` is not currently wired up in this
repo. Build via the standard `cabal` flow inside a shell that provides
the system libraries:

```nix
{ pkgs }: pkgs.mkShell {
  buildInputs = with pkgs; [
    pkg-config snappy liburing zlib gmp
    postgresql_16
    ghc cabal-install
  ];
}
```

GHC 9.8.4 specifically is required; nixpkgs may default to a newer
version. Pin via `haskell.compiler.ghc984` once it's available, or
fall back to `ghcup` inside the shell.

## Next

Once the toolchain and PostgreSQL are installed, move on to
[Building from source](building).
