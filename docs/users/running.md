---
id: running
title: Running dbsync
sidebar_position: 5
---

# Running dbsync

How to invoke dbsync against a running Cardano node and a fresh
PostgreSQL database.

## Prerequisites checklist

Before launching dbsync:

- [ ] `cardano-node` is running and producing blocks.
- [ ] The node's Unix socket is readable by the user running dbsync.
- [ ] PostgreSQL is running and the user / database referenced in the
  [pg-config file](#the-postgresql-connection-file) exist.
- [ ] The `dbsync-ledger/` directory's parent (the `--ledger-state-dir`
  argument) is writable.
- [ ] You've picked a [config](profiles/overview).

## CLI

dbsync takes five required flags:

```bash
dbsync \
  --config           ./config-examples/everything-no-ledger.json \
  --pg-config        ~/cardano/pg-config.json \
  --node-config      ~/cardano/mainnet/config.json \
  --socket-path      ~/cardano/mainnet/db/node.socket \
  --ledger-state-dir ~/cardano/mainnet
```

| Flag | Required | Description |
|---|---|---|
| `--config` | yes | Path to your [dbsync config JSON](profiles/overview) — sync mode, ledger, `db_profile`, logging. |
| `--pg-config` | yes | Path to the [PostgreSQL connection file](#the-postgresql-connection-file). |
| `--node-config` | yes | Path to the `cardano-node` `config.json` (the one from the Cardano book). Genesis files are resolved relative to it. |
| `--socket-path` | yes | Path to the `cardano-node` Unix socket. Same value you pass to the node's `--socket-path`. |
| `--ledger-state-dir` | yes | Parent directory under which the `dbsync-ledger/` sub-directory is created. Holds the LSM session, on-disk LedgerDB snapshots (if enabled), and the per-network fingerprint file. |

Plus two optional flags for recovery:

| Flag | Description |
|---|---|
| `--resync-from-genesis` | Wipe the database schema and ledger state, then re-sync from genesis. Destructive. |
| `--rollback-to-slot SLOT` | Roll the database back to the nearest block at or after `SLOT` before resuming. Pure recovery hatch — no migration semantics. See [Recovery](operations/recovery). |

## The PostgreSQL connection file

The file passed via `--pg-config` holds the connection settings and
nothing else, so it is the one file that carries credentials:

```json
{
  "host": "localhost",
  "port": 5432,
  "name": "cexplorer",
  "user": "dbsync",
  "password_file": "/run/secrets/dbsync-pg-password"
}
```

| Field | Required | Default | Notes |
|---|---|---|---|
| `host` | yes | — | Host name, IP, or socket directory. |
| `port` | no | `5432` | |
| `name` | yes | — | Database name. |
| `user` | no | `postgres` | `""` falls back to the OS user (peer authentication on a local install). |
| `password_file` | no | — | Path to a file whose contents are the password. Relative paths resolve against the pg-config file's own directory. |

There is deliberately no inline `password` field. When `password_file`
is absent the password is empty — fine for local peer / trust auth.
For anything else, point `password_file` at a file readable only by
the user running dbsync (`chmod 0400`), a Docker secret
(`/run/secrets/...`), or a mounted Kubernetes `Secret`. Trailing
newlines are stripped, so `echo`-created files and Kubernetes Secrets
work as-is.

`config-examples/pg-config.example.json` is a working starting point
for a local install.

## Two-terminal invocation

The simplest way to run things during development is two terminals:

```bash
# terminal 1 — node
cardano-node run \
  --config        ~/cardano/mainnet/config.json \
  --database-path ~/cardano/mainnet/db/ \
  --socket-path   ~/cardano/mainnet/db/node.socket \
  --topology      ~/cardano/mainnet/topology.json \
  --host-addr     0.0.0.0 \
  --port          1337
```

```bash
# terminal 2 — dbsync
dbsync \
  --config           ./config-examples/dapp.json \
  --pg-config        ~/cardano/pg-config.json \
  --node-config      ~/cardano/mainnet/config.json \
  --socket-path      ~/cardano/mainnet/db/node.socket \
  --ledger-state-dir ~/cardano/mainnet
```

:::tip
`scripts/run-everything-zellij.sh` does the same thing in a single
zellij layout — useful if you have zellij installed and don't want
to manage two windows manually.
:::

## What happens on first run

A fresh sync goes through three phases (see [Phases overview](/developers/phases/overview)
on the developer side for the gory details):

```
IngestChainHistory  ─►  PreparingForVolatileTail  ─►  FollowingVolatileTail / FollowingChainTip
   (bulk-load)            (post-load setup)              (steady state, per-block)
```

What you'll see in the log:

1. **Startup** — config validated, schema created, extractors listed,
   and a `Network: mainnet (magic 764824073)` line naming the network
   the genesis describes. The network is recorded in the database on
   first run; later boots
   [refuse to start](operations/troubleshooting#cannot-resume-the-database-was-synced-against-a-different-network)
   if `--node-config` points at a different network.
2. **`IngestChainHistory`** — per-epoch summary lines as bulk-load
   progresses. On mainnet this is the longest phase (hours, not
   minutes).
3. **`PreparingForVolatileTail`** — a few minutes of CTAS rebuilds,
   index builds, the LOGGED flip, and `ANALYZE`. Outer markers only
   at `info`; raise to `debug` for per-step timing.
4. **`FollowingVolatileTail` → `FollowingChainTip`** — per-block log
   lines. Once the consumer catches up with the receiver, the phase
   tag flips to `FollowingChainTip` and the loop emits a `"still at
   tip"` heartbeat every 30 seconds when no new block has arrived.

Catching up against mainnet on a 4-core / 16 GB target with
`everything-no-ledger` is typically under a day; the smaller profiles
are proportionally faster. With ledger enabled (`everything`) expect
the ledger replay to add several hours on top.

## Stopping

`SIGINT` (Ctrl-C) is safe — the orchestrator's shutdown bracket
cancels the receiver, drains the writer, writes a final ledger
snapshot if enabled, and exits cleanly. The on-disk state is left in
a resumable shape regardless of which phase you stop in.

`SIGTERM` is also handled.

:::caution `SIGKILL`
`SIGKILL` doesn't give the shutdown bracket a chance to run. The
next boot will discover any stale state (rows past
`last_committed_slot`, partial snapshots) and clean it up
automatically — but you'll lose any work in the last in-flight epoch
or block. Use `SIGTERM` or Ctrl-C unless the process is genuinely
stuck.
:::

## Environment

dbsync reads no environment variables of its own. Standard PostgreSQL
environment variables (`PGHOST`, `PGUSER`, ...) are *not* consulted —
the connection comes exclusively from the `--pg-config` file.

## Logs

Logs go to stderr. Format follows the config's `logging.format`:

- `text` — human-readable, one line per event, `[severity] component:
  message` shape.
- `json` — one JSON object per event, suitable for piping into
  Loki / fluent-bit / etc.

At `info` you get phase transitions, epoch summaries, restart-relevant
events, and warnings. At `debug` you also get per-step Prep timings,
per-block progress, and verbose receiver / tx-out-worker traces.

:::tip
`logging.level = "trace"` adds per-row diagnostics. It's invaluable
for investigating a specific extractor's behaviour, but the volume
is large — switch back to `info` or `debug` once you've gathered
what you need.
:::

## Next

[Operations / Metrics](operations/metrics) and
[Operations / Troubleshooting](operations/troubleshooting).
