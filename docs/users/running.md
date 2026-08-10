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
- [ ] You've picked a [config](config/overview).

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
| `--config` | yes | Path to your [dbsync config JSON](config/overview) — sync mode, ledger, `extractors`, logging. |
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
3. **`PreparingForVolatileTail`** — CTAS rebuilds, index builds,
   foreign-key creation, the LOGGED flip, and `ANALYZE`. Every step
   logs its start, its completion, and its duration at `info`. A long
   step also logs a progress line each minute.
4. **`FollowingVolatileTail` → `FollowingChainTip`** — per-block log
   lines. Once the consumer catches up with the receiver, the phase
   tag flips to `FollowingChainTip` and the loop emits a `"still at
   tip"` heartbeat every 30 seconds when no new block has arrived.

How long the catch-up takes depends on the chain, the hardware, and
the extractors you enable. Ingest is the longest phase. Enabling
`ledger` adds a full ledger replay on top.

## Stopping

**Stop dbsync with Ctrl-C (`SIGINT`).** The GHC runtime turns `SIGINT`
into an exception. The shutdown bracket then runs: it cancels the
receiver, drains the writer, writes a final ledger snapshot if the
ledger is enabled, and exits. The on-disk state stays resumable in
every phase.

:::danger `SIGTERM` and `SIGKILL` are not safe
dbsync installs no signal handlers. `SIGTERM` therefore keeps its
default behaviour and kills the process immediately. The shutdown
bracket does not run, so `SIGTERM` behaves exactly like `SIGKILL`.

Do not use `kill` or `systemctl stop` without configuring the unit to
send `SIGINT`. For systemd, set `KillSignal=SIGINT`.
:::

If dbsync dies without running the shutdown bracket, you do not lose
the database. The next boot finds the stale state — rows past
`last_committed_slot`, and partial snapshots — and cleans it up. You
lose only the work in the epoch or block that was in flight.

## Environment

dbsync itself reads no environment variables.

Its `psql` sub-processes do. dbsync shells out to `psql` to create and
drop the schema, and those calls inherit `PGHOST`, `PGPORT`, `PGUSER`,
and `PGPASSWORD` from the environment. The `host` and `port` in the
`--pg-config` file do **not** apply to them.

:::caution
Set the libpq environment variables to match your `--pg-config` file,
or unset them. If they disagree, dbsync creates the schema in one
database and writes rows to another.
:::

## Logs

dbsync writes logs to stderr, one line per event, in the shape
`[severity] component: message`.

`logging.level` selects the volume. At `info` you get phase
transitions, per-step Prep progress, epoch summaries, and warnings. At
`debug` you also get per-block progress and receiver / tx-out-worker
traces.

:::note
`logging.format` accepts `text` and `json`, but dbsync only emits
text today. Setting `json` changes nothing.

`logging.level` accepts `debug`, `info`, `warning`, and `error`. Any
other value falls back to `info` without an error, so a typo makes
your logs quieter, not louder.
:::

## Next

[Operations / Metrics](operations/metrics) and
[Operations / Troubleshooting](operations/troubleshooting).
