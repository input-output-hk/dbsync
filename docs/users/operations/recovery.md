---
id: recovery
title: Recovery and restart
sidebar_position: 3
---

# Recovery and restart

How dbsync handles restarts, rollbacks, and the cases where a full
re-sync is unavoidable.

## Clean restart

A clean restart — `SIGINT` or `SIGTERM`, then re-launching — is
always safe. The orchestrator's shutdown bracket cancels the
receiver, drains the writer, writes a final snapshot if the ledger
is enabled, and exits with all state on disk in a resumable shape.

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
`sync_state.last_committed_slot` advance, so there's no partial-block
state to clean up.

## Rollbacks during Follow

`MsgRollback` from the node triggers the rollback cascade
([`Phase.Following.Rollback`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Following/Rollback.hs)).
It runs in a single transaction:

1. Walk the FK graph in dependency order.
2. `DELETE FROM <table> WHERE <fk> > <target_slot>` per table per
   family.
3. Update `sync_state.last_committed_slot` to the target.

:::caution `k`-bounded
Rollbacks are bounded by the protocol security parameter `k` (2160
on mainnet). A deeper rollback panics rather than silently
corrupting the database. The recovery path is a manual rollback to
a slot at or above `tip − k`, or a fresh sync.
:::

If the ledger is enabled, rollbacks within the last ~100 blocks are
absorbed by the worker's in-memory volatile buffer without touching
the disk snapshot. Deeper than that, the worker panics and the
recovery path is a restart that reloads the disk snapshot.

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

| Decision | When | What it does |
|---|---|---|
| `BootFresh` | Empty database; no sync-state row. | Start `IngestChainHistory` from genesis. |
| `BootResume` | Sync-state row exists; `sync_complete = false`. | Restart Ingest at the last committed epoch. Rebuilds dedup maps from PG. |
| `BootFollowRestart` | Sync-state row exists; `sync_complete = true`. | Start directly in `FollowingVolatileTail` (skipping Ingest + Prep). |

Mismatches between disk and PG state (snapshots without a sync-state
row, ledger-enabled flip, fingerprint drift) abort boot early with
an operator-facing message rendered by `renderBootError`. Read the
message — it usually tells you exactly which recovery option to use.

## When re-sync is unavoidable

A handful of cases require dropping the database and starting over:

- **Profile change** — adding, removing, or changing the structure
  of an extractor. See
  [profile immutability](../profiles/overview#profile-immutability).
- **Schema change** in a dbsync upgrade. Migrations aren't
  implemented yet; until the migration framework lands, any
  table-shape change means a full re-sync.
- **Network change** — pointing dbsync at a different network's
  genesis. The fingerprint check catches this and refuses to boot.
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
