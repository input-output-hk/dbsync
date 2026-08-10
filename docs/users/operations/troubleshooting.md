---
id: troubleshooting
title: Troubleshooting
sidebar_position: 2
---

# Troubleshooting

Common failures and how to resolve them.

## "Cardano-node socket file not yet present; retrying in 5s"

dbsync cannot reach the `cardano-node` Unix socket. It does not fail.
It retries every 5 seconds until the socket answers, so this message
repeats until you fix the cause.

A second message means the socket file exists but the node is not
answering on it yet:

```
Cardano-node socket present but not accepting yet; retrying in 5s
```

That usually means the node is still starting. Its LedgerDB replay
can take minutes on a populated chain. Wait for `Chain extended` in
the node's log.

If the first message repeats instead, check these in order:

- Confirm `cardano-node` is running.
- Check that `--socket-path` matches the path the node was started
  with.
- Check the file permissions. dbsync needs read access. The simplest
  fix is to run both programs as the same OS user.

## "could not connect to server: Connection refused" (PostgreSQL)

PG isn't reachable on the configured host/port.

- On a local install: `systemctl status postgresql` (Linux) or
  `brew services list | grep postgres` (macOS).
- Check the host / port in your `--pg-config` file.
- For remote PG, confirm `pg_hba.conf` allows the dbsync host.

## "FATAL: role "..." does not exist"

The PG user named in the pg-config file doesn't exist. Create it:

```bash
createuser --createdb dbsync
```

Or set `user: ""` in the pg-config file to fall back to the OS user
(peer authentication on a local install).

## "permission denied for database "cexplorer""

The PG user exists but has no grants on the database.

**dbsync never creates the database.** You create it, then dbsync
creates the tables inside it. Grant the user `CONNECT` on the
database, plus `CREATE` and `USAGE` on the `public` schema.

```bash
createdb -O dbsync cexplorer
```

## "… extractor requires … to be enabled"

Config validation rejected the combination and named the missing
dependency. Add it to `extractors`, or enable `ledger`. The enforced
rules:

- `multi_asset` → needs `utxo`.
- `off_chain_pools` → needs `pool`.
- `off_chain_votes` → needs `governance`.
- `epoch_boundary`, `pool_stats`, `stake_delegation_ledger`,
  `current_state` → need `ledger.enabled = true`.

See [Custom configs](../config/custom) for the dependency table.

## "Schema mismatch — refusing to start"

You changed the config against an existing database. This is the
[`extractors` is fixed per database](../config/overview#extractors-is-fixed-per-database)
guard. Your options:

- Revert the config to match what's in the database (resume the
  existing sync).
- Re-sync against a fresh database (drop the existing one).
- Pass `--resync-from-genesis` to wipe and start over (destructive).

## "Cannot resume: the database was synced against a different network"

On first run dbsync records the network in the database
(`dbsync_sync_state.network_magic` / `network_name`); every later boot
compares that against the genesis reachable through `--node-config`
and refuses to interleave two chains. The message names both sides:

```
  Database : preview (magic 2)
  This run : mainnet (magic 764824073)
```

- Wrong `--node-config` (the common case): point it at the
  `config.json` of the network this database was synced against.
- Actually switching networks: use a fresh database, or pass
  `--resync-from-genesis` to wipe this one (destructive).

## "Postgres wal_level is 'replica'. For fastest bulk-load, …"

dbsync emits this warning once, on the run that creates the schema.
It does not repeat on later boots.

```
Postgres wal_level is 'replica'. For fastest bulk-load,
set the following in postgresql.conf and restart the server:
  wal_level = minimal
  max_wal_senders = 0
  archive_mode = off
```

This is not an error and the sync works without any change. But on
`replica`, the `UNLOGGED → LOGGED` flip in
[`PreparingForVolatileTail`](/developers/phases/preparing) writes
every row to the WAL a second time. That costs wall-clock time.

:::tip One-time initial sync
If you run no replicas, set `wal_level = minimal` for the duration of
the sync, then revert it. `scripts/postgres-tuning.conf` holds the
full snippet.

Reverting to `wal_level = replica` afterwards forces a full re-base
of any replica. That is acceptable on a one-time fresh sync.
:::

## `FsTooManyOpenFiles` during Ingest

dbsync opens one libpq connection per extractor table, plus
connections for the control channel and the TxOut worker. With every
extractor enabled that is more than 70 connections, on top of the
files the LSM stores hold open.

At boot, `Phase.Ingest.FdLimit` raises the process's **soft** limit to
the hard limit (capped at 1048576). It does not abort if the limit is
low. If the OS refuses the raise, it logs a warning and continues.

You therefore only need to act if the **hard** limit is low. Raise it
in `/etc/security/limits.conf`:

```
your-user soft nofile 8192
your-user hard nofile 16384
```

`ulimit -n` in the launching shell has no effect, because dbsync
raises the soft limit itself.

## Disk full during Ingest

PG runs out of space partway through the bulk-load.

- UNLOGGED tables don't shrink — once allocated, the space is held.
  Recovery is a `DROP DATABASE` and a re-sync against a larger disk.
- Index builds during
  [`PreparingForVolatileTail`](/developers/phases/preparing)
  temporarily allocate roughly the size of the indexed table. If
  Ingest itself fit but Prep doesn't, dropping the smallest disabled
  extractor's tables and re-running can sometimes recover; usually
  it's cleaner to re-sync to a bigger disk.

The size table in [Prerequisites](../installation/prerequisites)
gives rough working figures; budget 50% headroom over the listed
sizes.

## The process is OOM-killed

`ledger.enabled = true` adds a full ledger replay to the sync. The
LedgerDB itself is on disk, but the replay holds caches and a
checkpoint buffer in RAM, and that is the largest memory consumer in
a dbsync run.

If the kernel kills the process:

- Give the machine more RAM. The ledger replay needs it, PG needs
  `shared_buffers`, and the sync needs its own working set.
- Or disable the ledger and use `everything-no-ledger.json`. You lose
  the rewards, deposits, and protocol-parameter tables. Every
  block-derived table still works.

## Ingest is slow

In order of likelihood:

1. **PG is not tuned.** Check `wal_level`, `shared_buffers`,
   `maintenance_work_mem`, and `max_parallel_maintenance_workers`.
   Start from `scripts/postgres-tuning.conf`.
2. **The disk is slow.** A network-mounted PG data directory costs a
   lot. Use local NVMe.
3. **Too many extractors for the machine.** Drop to a smaller
   `extractors` and re-sync.
4. **`liburing` is missing on Linux.** The build then uses
   `+serialblockio`, which is correct but slower for the LSM stores
   and the LedgerDB.

Compare epochs against each other to find where the time goes:

```sql
SELECT epoch_no, blocks_per_sec, elapsed_sec
FROM epoch_sync_stats
ORDER BY elapsed_sec DESC
LIMIT 10;
```

## "rollbackToPoint: target block N is more than k=2160 behind current tip"

The Follow loop received a `MsgRollback` deeper than the protocol
security parameter `k`. A node bug causes this, or a manual
`--rollback-to-slot` too far behind the tip. dbsync stops instead of
corrupting the database.

Recover with `--rollback-to-slot SLOT`, using a slot at or after the
new chain's intersection point. Use `--resync-from-genesis` if you
cannot find one. See [Recovery](recovery).

## "chain rollback to slot N crosses the k-safe rollback boundary"

The **ledger worker** raised this one. The rollback target is older
than the oldest state in its in-memory buffer.

dbsync records the target in
`dbsync_sync_state.pending_rollback_slot` before it stops. **Restart
dbsync and it replays the rollback from a disk snapshot.** You need
no flags and no manual step.
