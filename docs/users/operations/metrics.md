---
id: metrics
title: Metrics
sidebar_position: 1
---

# Metrics

:::warning Not implemented
dbsync exposes no metrics endpoint. The config parser accepts
`metrics.prometheus_port`, but no server listens on that port, and
dbsync does not collect the underlying counters.

Monitor a running sync through the logs and the two tables below.
:::

## Where the sync is now

`dbsync_sync_state` holds one row describing the current position.

```sql
SELECT last_committed_slot, last_committed_block_no, sync_complete
FROM dbsync_sync_state;
```

The same row also holds the per-table id counters, the recorded
network magic, the schema fingerprint, and the extractor list.

## How fast each epoch was

The `epoch_sync_stats` table records one row per finalised epoch.
Every preset enables it.

| Column | Meaning |
|---|---|
| `epoch_no` | The epoch this row describes. |
| `blocks_processed` | Blocks written for the epoch. |
| `blocks_per_sec` | Throughput over the epoch. |
| `elapsed_sec` | Wall-clock time for the epoch. |
| `phase` | The phase that wrote the row. |
| `synced_at` | When dbsync finalised the epoch. |

Use it to find where a slow sync lost time:

```sql
SELECT epoch_no, blocks_per_sec, elapsed_sec
FROM epoch_sync_stats
ORDER BY elapsed_sec DESC
LIMIT 10;
```

## Logs

At `logging.level = "info"` dbsync logs each phase transition, each
Prep step with its duration, and one summary line per epoch during
Ingest. In `FollowingChainTip` it logs a `"still at tip"` heartbeat
every 30 seconds while no new block arrives.

If that heartbeat stops and no block lines appear, dbsync is no longer
following the chain. See
[Troubleshooting](troubleshooting).
