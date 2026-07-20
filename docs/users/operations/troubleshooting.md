---
id: troubleshooting
title: Troubleshooting
sidebar_position: 2
---

# Troubleshooting

Common failures and how to resolve them.

## "Failed to connect to socket: …"

The `cardano-node` Unix socket isn't reachable.

- Confirm `cardano-node` is running.
- Check that the path you pass to `--socket-path` matches the path
  the node was started with.
- Check file permissions — dbsync needs read access. The easiest fix
  is to run both as the same OS user.

If the socket file exists but the connection still fails, the node
is probably still starting up — its LedgerDB replay can take minutes
on a populated chain. Wait until you see `Chain extended` in the
node's log, or query the tip via `cardano-cli`.

## "could not connect to server: Connection refused" (PostgreSQL)

PG isn't reachable on the configured host/port.

- On a local install: `systemctl status postgresql` (Linux) or
  `brew services list | grep postgres` (macOS).
- Check the host / port in your profile's `database` section.
- For remote PG, confirm `pg_hba.conf` allows the dbsync host.

## "FATAL: role "..." does not exist"

The PG user named in the profile doesn't exist. Create it:

```bash
createuser --createdb dbsync
```

Or set `user: ""` in the profile to fall back to the OS user (peer
authentication on a local install).

## "permission denied for database "cexplorer""

The PG user exists but lacks `CREATEDB` or grants on an existing
database. For a fresh sync, the simplest fix is to grant `CREATEDB`
and have dbsync create the database itself; for a constrained
production user, the operator should pre-create the database and
grant `CONNECT`, `CREATE`, and `USAGE` on the `public` schema.

## "… extractor requires … to be enabled"

Profile validation rejected the combination and named the missing
dependency. Add it to `db_options`, or enable `ledger`. The enforced
rules:

- `multi_asset` → needs `utxo`.
- `off_chain_pools` → needs `pool`.
- `off_chain_votes` → needs `governance`.
- `epoch_boundary`, `pool_stats`, `stake_delegation_ledger`,
  `current_state` → need `ledger.enabled = true`.

See [Custom profiles](../profiles/custom) for the dependency table.

## "Profile mismatch — database was synced with X, profile says Y"

You changed the profile against an existing database. This is the
[profile immutability](../profiles/overview#profile-immutability)
guard. Your options:

- Revert the profile to match what's in the database (resume the
  existing sync).
- Re-sync against a fresh database (drop the existing one).
- Pass `--resync-from-genesis` to wipe and start over (destructive).

## "WARN: wal_level is replica; consider 'minimal' during initial sync"

dbsync emitted this at boot because PG is configured with
`wal_level = replica` (the default). It's not an error — the sync
will work — but the `UNLOGGED → LOGGED` flip in
[`PreparingForVolatileTail`](/developers/phases/preparing) writes
every row to WAL on `replica`, materially increasing wall-clock time
for a large profile.

:::tip One-time initial sync
If you don't have replicas to worry about, set `wal_level = minimal`
in `postgresql.conf` for the sync duration. See
`scripts/postgres-tuning.conf` in the repo for a tuned starting
point — typically a 20–30% reduction in Prep wall-clock time on a
large profile.
:::

## "FATAL: too many open files" / "EMFILE"

Hit on Linux during Ingest. dbsync opens one libpq connection per
extractor table, plus connections for the control channel and the
TxOut worker. On a large profile this can exceed the default per-user
fd limit (1024 on most distros).

Raise the limit:

```bash
ulimit -n 8192
```

Add it to `/etc/security/limits.conf` for a permanent fix:

```
your-user soft nofile 8192
your-user hard nofile 16384
```

dbsync also checks the limit at boot
([`Phase.Ingest.FdLimit`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/FdLimit.hs))
and aborts early if it's clearly too low.

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

## OOM during ledger replay

`ledger.enabled = true` puts an in-RAM ledger state on top of the
sync. On mainnet at tip that's around 8 GB of resident memory; less
on testnets.

If the process is OOM-killed:

- Confirm you have at least 16 GB of RAM total (8 for the ledger,
  plus PG's `shared_buffers`, plus the sync's working set).
- If memory is genuinely tight, disable the ledger and use
  `everything-no-ledger-profile.json` instead. You lose rewards /
  deposits / protocol-param tables; everything else still works.

## Slow Ingest — diagnostics

The expected throughput on mainnet is roughly:

- 100–500 blocks/sec early in Byron (small blocks, mostly UTxO).
- 30–80 blocks/sec late in Alonzo/Babbage (large blocks, Plutus
  scripts, lots of metadata).
- 10–30 blocks/sec late in Conway (governance + large NFT mints).

If you're consistently below these, in order of likelihood:

1. PG isn't tuned. Check `wal_level`, `shared_buffers`,
   `maintenance_work_mem`, `max_parallel_maintenance_workers`. The
   shipped `scripts/postgres-tuning.conf` is a reasonable starting
   point.
2. Disk is slow. Even on SSD, a heavily-fragmented filesystem or a
   network-mounted PG data directory hurts a lot. Local NVMe is the
   sweet spot.
3. Too many extractors enabled for your machine. Drop to a smaller
   profile and re-sync.
4. `liburing` isn't installed on Linux and the build fell back to
   `+serialblockio`. The fallback is correct but noticeably slower
   for the LSM dedup stores and (if enabled) the LedgerDB.

:::tip Diagnostic recipe
Set `logging.level = "debug"` and re-start; you'll get per-epoch
timing breakdowns showing exactly where the time goes (LSM
compaction vs COPY vs tx-out worker drain).
:::

## "rollback exceeds k blocks (depth N > 2160)"

The Follow loop received a `MsgRollback` deeper than the protocol
security parameter `k`. This is either a node bug or operator error
(rolling back manually to a slot more than 2160 blocks behind the
tip). dbsync panics rather than silently corrupting the database.

The recovery path is `--rollback-to-slot SLOT` with a slot at or
after the new chain's intersection point, or `--resync-from-genesis`
if you can't determine one. See [Recovery](recovery).

## "Ingest scratch state was wiped — refusing to resume"

The ingest LSM session at `<state-dir>/dbsync-ledger/ingest-lsm/` is
missing or corrupt while the database still shows Ingest as
incomplete. Recovery: pass `--resync-from-genesis` to start over.

This is unusual — the only way to hit it is to manually delete the
scratch directory mid-sync, which you shouldn't do.
