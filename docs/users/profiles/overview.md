---
id: overview
title: Profiles overview
sidebar_position: 1
---

# Profiles

A **profile** is a JSON file that decides which projections dbsync
runs against your database. Pass it via `--profile <path>` when you
start dbsync.

Each projection — UTxO, multi-asset, metadata, governance, etc. — is
an independent extractor. Enabling a projection populates its tables
on every block; disabling it skips the work *and* the tables, so the
database is exactly the shape you ask for.

## What a profile contains

Six top-level sections:

```json
{
  "database":   { "host": "localhost", "port": 5432,
                  "name": "cexplorer", "user": "", "password": "" },
  "sync":       { "mode": "auto" },
  "ledger":     { "enabled": false },
  "db_options": { "utxo": true, "epoch_sync_stats": true },
  "metrics":    { "prometheus_port": 8080 },
  "logging":    { "level": "info", "format": "text" }
}
```

| Section | Purpose |
|---|---|
| `database` | PostgreSQL connection. The schema is shared, no user separation. |
| `sync` | Sync mode. `auto` is the only mode you should normally use; `ingest` and `follow` exist for diagnostics. |
| `ledger` | Whether to apply blocks to an in-RAM ledger state and capture rewards, deposits, protocol params. Costs roughly 8 GB RAM and ~11 GB disk at mainnet tip. |
| `db_options` | Which projections are enabled. The heart of the profile. |
| `metrics` | Prometheus port. The endpoint isn't wired up yet (see [Metrics](../operations/metrics)). |
| `logging` | `info` / `debug` and `text` / `json`. |

The `core` extractor (block, tx, slot_leader) is always on and isn't
represented in `db_options`. Every other projection is opt-in — omit
the key or set it to `false` to disable.

## Profile immutability

:::warning Pick the profile before you sync
A profile is **fixed once the database is created**. The set of
enabled projections, the `ledger.enabled` flag, and the `utxo`
strategy are baked into the resulting schema. Changing them after
the fact requires a fresh sync. Schema migrations upgrade an existing
database between dbsync versions, but they do not turn projections on
or off — that is a different operation.
:::

dbsync detects mismatches at boot: if the database was synced with
the `utxo` projection enabled and you start it with a profile that
disables `utxo`, boot aborts with an operator-readable message. The
same applies in reverse.

The settings that *can* change between runs against the same database
are limited to the operational knobs: database connection, logging
level/format, metrics port. These don't affect the schema.

In-place schema migrations cover the database's *shape* as it evolves
across dbsync versions. They do not back-fill a newly enabled
projection's tables from genesis, so adding a projection to an existing
database still means a fresh sync. Treat profile choice as a one-time
decision per database.

## Which profile to pick

Six presets ship in `profiles/` covering the common cases — see
[Preset profiles](presets). Roughly:

| If you want | Use |
|---|---|
| Block-level index, lookups by hash / number | `minimal` |
| Wallets and balance queries | `utxo-only` |
| Stake-pool dashboards | `spo` |
| DApp / explorer queries | `dapp` |
| Full block-derived data without ledger state | `everything-no-ledger` |
| Everything including rewards, deposits, protocol params | `everything` |

If none of those fits, [write a custom profile](custom).

## Where profile JSON keys come from

Each `db_options` key corresponds to one extractor. The current set
of recognised keys (from
[`DbSync.App.Config.Types`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Config/Types.hs)
on the developer side):

`utxo`, `multi_asset`, `metadata`, `stake_delegation`, `pool`,
`scripts_datums`, `governance`, `cbor`, `epoch_sync_stats`,
`epoch_boundary`, `epoch`, `current_state`.

Three of these (`scripts_datums`, `governance`, `current_state`) are
reserved — their option key is accepted in the profile and the
validator won't reject them, but the underlying extractor is a no-op
stub today. The remaining options drive real projections. See
[Custom profiles](custom) for what each one writes and
[Existing extractors](/developers/extractors/existing) on the
developer side for the per-table breakdown.

## Validation

At boot dbsync:

1. Parses the profile JSON. Malformed JSON, unknown enum values, or
   missing required fields abort with a clear error.
2. Validates extractor dependencies. Enabling `pool` without
   `stake_delegation` is rejected with the message
   `Extractor 'pool' is enabled but its dependency 'stake_delegation' is not enabled.`
3. Cross-checks against the existing database schema (on a resume).
   Mismatches abort with a message explaining what changed.

:::note
If validation fails, dbsync exits before connecting to the node — no
partial state to clean up.
:::
