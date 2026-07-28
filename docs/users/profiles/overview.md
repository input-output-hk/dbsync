---
id: overview
title: The config file
sidebar_position: 1
---

# The config file

The JSON file you pass via `--config` decides how dbsync behaves:
which projections run, whether ledger state is applied, how it logs.
It contains no connection details and no credentials — the PostgreSQL
connection lives in a separate [pg-config
file](../running#the-postgresql-connection-file), so the config can be
committed, shared, and reused across environments as-is.

The heart of the config is the **profile** — the `db_profile` section.
Each projection — UTxO, multi-asset, metadata, governance, etc. — is
an independent extractor. Enabling a projection populates its tables
on every block; disabling it skips the work *and* the tables, so the
database is exactly the shape you ask for.

## What a config contains

Five top-level sections, all optional (an empty `{}` is a valid
config that syncs just the core tables):

```json
{
  "sync":       { "mode": "auto" },
  "ledger":     { "enabled": false },
  "db_profile": { "utxo": true, "epoch_sync_stats": true },
  "metrics":    { "prometheus_port": 8080 },
  "logging":    { "level": "info", "format": "text" }
}
```

| Section | Purpose |
|---|---|
| `sync` | Sync mode. `auto` is the only mode you should normally use; `ingest` and `follow` exist for diagnostics. |
| `ledger` | Whether to apply blocks to an in-RAM ledger state and capture rewards, deposits, protocol params. Costs roughly 8 GB RAM and ~11 GB disk at mainnet tip. |
| `db_profile` | Which projections are enabled. The heart of the config. |
| `metrics` | Prometheus port. The endpoint isn't wired up yet (see [Metrics](../operations/metrics)). |
| `logging` | `info` / `debug` and `text` / `json`. |

The `core` extractor (block, tx, slot_leader) is always on and isn't
represented in `db_profile`. Every other projection is opt-in — omit
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
the `utxo` projection enabled and you start it with a config that
disables `utxo`, boot aborts with an operator-readable message. The
same applies in reverse.

The settings that *can* change between runs against the same database
are limited to the operational knobs: the pg-config connection file,
logging level/format, metrics port. These don't affect the schema.

In-place schema migrations cover the database's *shape* as it evolves
across dbsync versions. They do not back-fill a newly enabled
projection's tables from genesis, so adding a projection to an existing
database still means a fresh sync. Treat the profile choice as a
one-time decision per database.

## Which profile to pick

Six presets ship in `config-examples/` covering the common cases —
see [Preset configs](presets). Roughly:

| If you want | Use |
|---|---|
| Block-level index, lookups by hash / number | `minimal` |
| Wallets and balance queries | `utxo-only` |
| Stake-pool dashboards | `spo` |
| DApp / explorer queries | `dapp` |
| Full block-derived data without ledger state | `everything-no-ledger` |
| Everything including rewards, deposits, protocol params | `everything` |

If none of those fits, [write a custom config](custom).

## Where profile keys come from

Each `db_profile` key corresponds to one extractor. The current set
of recognised keys (from
[`DbSync.App.Config.Types`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Config/Types.hs)
on the developer side):

`utxo`, `multi_asset`, `metadata`, `stake_delegation`,
`stake_delegation_ledger`, `pool`, `pool_stats`, `scripts_datums`,
`governance`, `cbor`, `epoch_sync_stats`, `epoch_boundary`, `epoch`,
`off_chain_pools`, `off_chain_votes`, `current_state`.

All of these drive real projections except `current_state`, which is
reserved — its key is accepted in the config and the validator won't
reject it, but the underlying extractor is a no-op stub today.
Several keys need `ledger.enabled = true`
(`stake_delegation_ledger`, `pool_stats`, `epoch_boundary`) or another
extractor (`multi_asset` needs `utxo`; `off_chain_pools` needs `pool`;
`off_chain_votes` needs `governance`); the validator rejects a config
that breaks these rules. See [Custom configs](custom) for what each
one writes and [Existing extractors](/developers/extractors/existing)
on the developer side for the per-table breakdown.

## Validation

At boot dbsync:

1. Parses the config JSON. Malformed JSON, unknown enum values, or
   invalid field values abort with a clear error.
2. Validates extractor dependencies. Enabling `multi_asset` without
   `utxo`, for example, is rejected with a message like
   `multi_asset extractor requires utxo extractor to be enabled.`
3. Cross-checks against the existing database schema (on a resume).
   Mismatches abort with a message explaining what changed.

:::note
If validation fails, dbsync exits before connecting to the node — no
partial state to clean up.
:::
