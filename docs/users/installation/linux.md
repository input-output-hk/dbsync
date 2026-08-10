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
  zlib1g-dev \
  libtinfo-dev \
  libgmp-dev \
  curl
```

:::info Why liburing
`liburing-dev` is the asynchronous-I/O backend for the LSM dedup
stores and the on-disk LedgerDB. Every kernel from 5.1 onward has it,
which covers every supported Debian and Ubuntu release.

**The build does not fall back automatically on Linux.** If
`liburing` is missing, the build fails. Set the `+serialblockio` flag
yourself — see [Old kernels](building#common-build-issues).
:::

PostgreSQL isn't in the list above because most distributions don't
ship version 18 yet. Install the server (`postgresql-18`, which brings
the `psql` / `pg_dump` client tools with it) from the official
[PostgreSQL APT repository](https://wiki.postgresql.org/wiki/Apt):

```bash
sudo apt install -y postgresql-common
sudo /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh -y
sudo apt install -y postgresql-18 postgresql-client-18
```

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
ghcup install ghc 9.14.1
ghcup set ghc 9.14.1
ghcup install cabal 3.16.1.0
ghcup set cabal 3.16.1.0
```

`ghcup tui` is the easier way to manage versions if you have several
GHC installs side by side.

## PostgreSQL setup

Create a role and a database for dbsync. **dbsync does not create the
database itself.** Your `--pg-config` file names both:

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
    postgresql_18
    ghc cabal-install
  ];
}
```

GHC 9.14.1 specifically is required; nixpkgs may default to a different
version. Pin via `haskell.compiler.ghc9141` once it's available, or
fall back to `ghcup` inside the shell.

## Next

Once the toolchain and PostgreSQL are installed, move on to
[Building from source](building).
