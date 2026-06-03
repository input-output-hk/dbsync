---
id: metrics
title: Metrics
sidebar_position: 1
---

# Metrics

:::warning Not yet implemented
The Prometheus endpoint is reserved but not wired up. The
`metrics.prometheus_port` field in the profile is honoured by the
config parser, but no server listens on the port today.

This page describes the planned design so operators know what to
expect. If you need observability today, parse the structured
(`logging.format = "json"`) logs — they contain the same data the
endpoint will expose.
:::

## Planned design

When wired up, dbsync will expose a Prometheus-format endpoint on
the port set by `metrics.prometheus_port` (default `8080`). One
process, one endpoint at `GET /metrics`.

The metric definitions live in
[`DbSync.Metrics`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Metrics.hs);
the underlying values are already tracked through the sync, only the
HTTP server hasn't been wired.

## Planned metrics

| Metric | Type | Meaning |
|---|---|---|
| `dbsync_blocks_processed_total` | counter | Blocks the consumer has finished writing. |
| `dbsync_current_epoch` | gauge | Epoch number of the most recently committed block. |
| `dbsync_current_block` | gauge | Block number of the most recently committed block. |
| `dbsync_current_slot` | gauge | Slot of the most recently committed block. |
| `dbsync_blocks_per_sec` | gauge | Recent throughput. Resets across phase transitions. |
| `dbsync_copy_rows_written_total` | counter | Rows written by the Ingest COPY path. |
| `dbsync_phase` | gauge | 0 = `IngestChainHistory`, 1 = `PreparingForVolatileTail`, 2 = `FollowingVolatileTail` / `FollowingChainTip`. |
| `dbsync_dedup_store_size` | gauge | Combined size of the Ingest LSM dedup stores. |
| `dbsync_queue_depth` | gauge | Depth of the receiver → consumer block queue. |

Plus the standard GHC runtime metrics (heap size, GC time, capability
utilisation) exposed via `-T` in the executable's baked-in RTS opts.

## Until the endpoint lands

The same data is available three other ways:

- **Logs.** With `logging.level = "info"` you get per-epoch summary
  lines during Ingest and per-block progress during Follow. With
  `logging.format = "json"` they're parseable directly.
- **`epoch_sync_stats` table.** Always-on (enabled by every preset),
  one row per finalised epoch with timing, block count, and tx count.
  Useful for retrospective analysis.
- **PostgreSQL.** The `dbsync_sync_state` row tracks
  `last_committed_slot`, `last_committed_block_no`, and the per-table
  id counters. `SELECT * FROM dbsync_sync_state` shows where the
  sync is up to.

## What to alert on (eventually)

Once the endpoint is live, the alerting cases that matter most:

- **`dbsync_phase` stays at `0` indefinitely.** Ingest isn't
  finishing. Either the node is behind or dbsync isn't making
  progress.
- **`dbsync_blocks_per_sec` drops to near zero in Follow.** The node
  socket has stalled or PG is unreachable.
- **`dbsync_queue_depth` saturates.** The consumer can't keep up with
  the receiver — usually a PG-side bottleneck.
- **Process death** (via Prometheus's `up` metric on this target).
  The sync stopped; check logs for the cause.
