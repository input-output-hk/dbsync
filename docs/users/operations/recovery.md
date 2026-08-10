---
id: recovery
title: Recovery and restart
sidebar_position: 3
---

# Recovery and restart

How dbsync handles restarts, rollbacks, and the cases where a full
re-sync is unavoidable.

## Clean restart

A clean restart is Ctrl-C (`SIGINT`), then re-launching. The shutdown
bracket cancels the receiver, drains the writer, writes a final
snapshot if the ledger is enabled, and leaves all on-disk state
resumable.

:::danger
`SIGTERM` is **not** a clean stop. dbsync installs no signal
handlers, so `SIGTERM` kills the process without running the shutdown
bracket. See [Stopping](../running#stopping).
:::

On the next boot, dbsync:

1. Reads `dbsync_sync_state` to find the last committed slot.
2. Reconciles against the on-disk ledger snapshots (if applicable).
3. Picks one of three boot paths — `BootFresh`, `BootResume`, or
   `BootFollowRestart` — and continues.

You don't need to pass any special flags. Just re-run the same
command line.

## Crash recovery

Same path as clean restart. The database's `dbsync_sync_state.last_committed_slot`
is the truth source — anything past that slot is deleted at boot via
`deleteRowsPastSlot`. Because the data tables are UNLOGGED during
Ingest, the deletes are cheap and the cleanup is fast.

Crashes during the [`PreparingForVolatileTail`](/developers/phases/preparing)
pass restart at the appropriate Prep step (the pass is idempotent
where possible and uses `IF NOT EXISTS` for index DDL). If a crash
catches Prep mid-CTAS, the rebuilt table is missing and the next boot
re-runs the rebuild.

Crashes during Follow restart at the last per-block commit. Follow
transactions are atomic over both the row writes and the
`dbsync_sync_state.last_committed_slot` advance, so there's no partial-block
state to clean up.

## Rollbacks during Follow

`MsgRollback` from the node triggers the rollback cascade
([`Phase.Following.Rollback`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Following/Rollback.hs)).
It runs in a single transaction:

1. Walk the FK graph in dependency order.
2. Find the first surviving id for each family — the smallest `tx.id`,
   `tx_out.id`, and `pool_update.id` after the target block — then
   delete every row at or above it. Rows keyed by block delete on
   `block_id`, and rows keyed by epoch delete on `epoch_no`.
3. Clear `tx_out.consumed_by_tx_id` on any surviving row that pointed
   at a deleted transaction.
4. Update `dbsync_sync_state.last_committed_slot` to the target.

The cascade deletes by **id threshold**, not by slot. Slots are not
dense, but ids are, so an id range is one indexed predicate per table.

:::caution `k`-bounded
The protocol security parameter `k` (2160 on mainnet) bounds a
rollback. dbsync stops on anything deeper instead of corrupting the
database. Recover with a manual rollback to a slot at or above
`tip − k`, or with a fresh sync.
:::

If the ledger is enabled, the worker's in-memory buffer absorbs
rollbacks within roughly the last 100 blocks without touching the
disk snapshot. For a deeper rollback, the worker writes the target to
`dbsync_sync_state.pending_rollback_slot` and stops. **Restart dbsync
and it replays the rollback from a disk snapshot automatically.**

## Manual rollback (`--rollback-to-slot`)

`--rollback-to-slot SLOT` rolls the database back to the nearest
block at or after `SLOT` before resuming the normal boot flow. The
nearest-at-or-after part matters because slots can be empty — the
flag resolves to the smallest block whose `slot_no >= SLOT`.

When you'd use it:

- A node-side error caused dbsync to diverge from the canonical
  chain.
- You want to re-process a specific epoch boundary for diagnostics.

It's a pure recovery hatch — no migration semantics, no schema
changes. The database is otherwise as it was.

If the rollback exceeds `k` from the current tip, the same panic
applies — pick a slot at or above `tip − k`.

## Boot decisions

[`DbSync.App.Boot.decideBoot`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Boot.hs)
classifies the observed state into one of three boot paths. Most of
the time you don't need to think about which — the classification is
automatic.

A sync-state row must already exist by this point. An empty database
is handled earlier: schema init creates the schema and seeds the row.
If `decideBoot` finds no row, it aborts.

| Decision | When | What it does |
|---|---|---|
| `BootFresh` | The row exists but records no committed progress. | Start `IngestChainHistory` from genesis. |
| `BootResume` | The row records progress; `sync_complete = false`. | Restart Ingest at the last committed epoch. Rebuilds dedup maps from PG. |
| `BootFollowRestart` | The row records `sync_complete = true`. | Start directly in `FollowingVolatileTail`, skipping Ingest and Prep. |

Any mismatch between disk and PG state aborts the boot — a missing
sync-state row, ledger snapshots with no row to match them, a change
to `ledger.enabled`, or fingerprint drift. `renderBootError` writes
the reason. Read it: it names the recovery option to use.

## When re-sync is unavoidable

A handful of cases require dropping the database and starting over:

- **`extractors` change** — enabling or disabling an extractor. See
  [`extractors` is fixed per database](../config/overview#extractors-is-fixed-per-database).
- **Uncovered schema drift.** A routine schema change in an upgrade
  migrates in place at boot, so it is *not* a re-sync case. dbsync
  refuses to start in two other cases: the schema drifted with no
  migration to cover it, or a newer binary built this database.
  Running the matching binary fixes the second case.
- **Network change** — pointing dbsync at a different network's
  genesis. dbsync compares `dbsync_sync_state.network_magic` against
  the genesis and refuses to boot. With the ledger enabled, the
  on-disk fingerprint file catches it as well.
- **Corruption** — physical disk corruption in either the PG data
  directory or the on-disk LSM session.

:::danger Destructive
The recovery is irrecoverable: it drops the entire PostgreSQL
database and wipes the on-disk ledger state. Anything not committed
upstream — analytic views, custom indexes, materialised tables
built off the dbsync schema — disappears with it. Back up first if
that's a concern.

```bash
dropdb cexplorer
rm -rf /path/to/ledger-state-dir/dbsync-ledger
# re-run dbsync from scratch
```

Or use the `--resync-from-genesis` flag, which wraps the above into
a single boot.
:::

## Backup strategy

dbsync doesn't ship a backup tool — use PostgreSQL's standard ones.

For a populated database you want to keep:

- **Logical backups** (`pg_dump`) — portable, slow, large. Useful
  for archival.
- **Physical backups** (`pg_basebackup` + WAL archiving) — fast to
  restore, locked to the same PG major version.
- **Snapshots** — filesystem-level (ZFS, LVM) or storage-level (EBS
  snapshots, ...). The fastest restore, but coordinate with the
  on-disk ledger snapshots at `<ledger-state-dir>/dbsync-ledger/` —
  snapshot both PG and the ledger directory together for a coherent
  restore.

:::tip
The on-disk ledger snapshots are rebuildable from the database (a
fresh boot will replay them), so they aren't strictly required in a
backup — but including them saves the replay time on restore.
:::
