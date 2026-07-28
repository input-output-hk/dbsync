---
id: node-setup
title: Cardano node setup
sidebar_position: 3
---

# Cardano node setup

dbsync follows a `cardano-node` over its node-to-client (n2c) Unix
socket. Any node that supports the n2c protocol works — dbsync doesn't
require a specific build or fork.

## Installing cardano-node

Follow the official Cardano installation docs at
[**developers.cardano.org**](https://developers.cardano.org/docs/get-started/infrastructure/node/installing-cardano-node/).
They cover four ways to get a node:

- **Pre-built release binaries** — fastest, statically-linked Linux
  amd64/arm64 tarballs published with every
  [GitHub release](https://github.com/IntersectMBO/cardano-node/releases).
- **Docker / GHCR images** — `ghcr.io/intersectmbo/cardano-node:<version>`.
  See the [packages page](https://github.com/IntersectMBO/cardano-node/pkgs/container/cardano-node)
  for available tags.
- **Nix** — reproducible build from the upstream flake. The recommended
  path if you want a verified binary.
- **Build with GHCup + cabal** — for systems without Nix.

Pick whichever fits your environment. dbsync doesn't care.

## What dbsync needs from the node

Three things:

1. **A running node** producing blocks on the network you target
   (mainnet, preprod, preview, custom testnet).
2. **A reachable Unix socket** — wherever you pointed
   `--socket-path` on the `cardano-node` command line.
3. **The node's config files**: `config.json` and the per-era
   genesis files it references. dbsync reads the same `config.json`
   you start the node with (pass it via `--node-config`) and resolves
   the genesis files relative to it.

The config and genesis bundle for each network is published in the
[Cardano book](https://book.world.dev.cardano.org/environments.html).

## Socket path

You set the socket path when starting the node:

```bash
cardano-node run \
  --config        ~/cardano/mainnet/config.json \
  --database-path ~/cardano/mainnet/db/ \
  --socket-path   ~/cardano/mainnet/db/node.socket \
  --host-addr     0.0.0.0 \
  --port          1337 \
  --topology      ~/cardano/mainnet/topology.json
```

That same path is what you pass to dbsync's `--socket-path` flag.

:::note Permissions
The socket needs to be readable by the user running dbsync. The
easiest way is to run both processes as the same OS user.
:::

## Bootstrapping the node database (mainnet)

:::tip Skip the multi-day genesis sync
A fresh mainnet sync at the network layer takes a long time. The
repository ships a `scripts/mithril-bootstrap.sh` helper that
downloads a verified ImmutableDB + VolatileDB snapshot from the
release-mainnet Mithril aggregator. The node then replays the
LedgerDB locally from that snapshot, which is much faster than
fetching every block from peers.

```bash
./scripts/mithril-bootstrap.sh --db-dir ~/cardano/mainnet
```

The script refuses to overwrite an existing `db/` directory — remove
or rename it first if you're re-bootstrapping. See the script header
for flags (`--digest` to pin a snapshot, `--dry-run` to list
available ones, `-y` to auto-install `mithril-client`).

A preview-network variant lives at
`scripts/mithril-bootstrap-preview.sh`.
:::

## Starting both together

`scripts/run-everything-zellij.sh` opens a `zellij` layout that runs
`cardano-node` and `dbsync` side by side. Useful while developing or
running against a local testnet. For production, run each under your
init system of choice (systemd, supervisord, runit, ...).

## Verifying the node is ready

dbsync waits for the socket to become bindable, so order doesn't
matter. To confirm the node is producing blocks before launching
dbsync, watch its stdout for `Chain extended` lines, or query the
tip:

```bash
cardano-cli query tip --mainnet --socket-path ~/cardano/mainnet/db/node.socket
```

## Next

[The config file](profiles/overview) — pick which tables you want
populated.
