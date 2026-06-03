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
  profile JSON exist.
- [ ] The `dbsync-ledger/` directory's parent (the `--ledger-state-dir`
  argument) is writable.
- [ ] You've picked a [profile](profiles/overview).

## CLI

dbsync takes four required flags:

```bash
dbsync \
  --db-sync-config   ~/cardano/mainnet/db-sync-config.json \
  --socket-path      ~/cardano/mainnet/db/node.socket \
  --ledger-state-dir ~/cardano/mainnet \
  --profile          ./profiles/everything-no-ledger-profile.json
```

| Flag | Required | Description |
|---|---|---|
| `--db-sync-config` | yes | Path to `db-sync-config.json` (the small file from the Cardano book that points at the node config). |
| `--socket-path` | yes | Path to the `cardano-node` Unix socket. Same value you pass to the node's `--socket-path`. |
| `--ledger-state-dir` | yes | Parent directory under which the `dbsync-ledger/` sub-directory is created. Holds the LSM session, on-disk LedgerDB snapshots (if enabled), and the per-network fingerprint file. |
| `--profile` | yes | Path to your [profile JSON](profiles/overview). |

Plus two optional flags for recovery:

| Flag | Description |
|---|---|
| `--resync-from-genesis` | Wipe the database schema and ledger state, then re-sync from genesis. Destructive. |
| `--rollback-to-slot SLOT` | Roll the database back to the nearest block at or after `SLOT` before resuming. Pure recovery hatch — no migration semantics. See [Recovery](operations/recovery). |

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
  --db-sync-config   ~/cardano/mainnet/db-sync-config.json \
  --socket-path      ~/cardano/mainnet/db/node.socket \
  --ledger-state-dir ~/cardano/mainnet \
  --profile          ./profiles/dapp-profile.json
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

1. **Startup** — config validated, schema created, extractors listed.
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
the connection comes from the profile JSON.

`DBSYNC_TEST_DB` is a test-only variable; production runs ignore it.

## Logs

Logs go to stderr. Format follows the profile's `logging.format`:

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
