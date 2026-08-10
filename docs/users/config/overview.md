---
id: overview
title: The config file
sidebar_position: 1
---

# The config file

The JSON file you pass via `--config` decides how dbsync behaves:
which extractors run, whether ledger state is applied, how it logs.
It contains no connection details and no credentials — the PostgreSQL
connection lives in a separate [pg-config
file](../running#the-postgresql-connection-file), so the config can be
committed, shared, and reused across environments as-is.

The `extractors` section is the heart of the config. It lists the
**extractors** — UTxO, multi-asset, metadata, governance, and the
rest. Each extractor is independent. Enabling one populates its
tables on every block. Disabling one skips both the work and the
tables, so the database holds exactly the shape you ask for.

## What a config contains

Five top-level sections, all optional (an empty `{}` is a valid
config that syncs just the core tables):

```json
{
  "sync":       { "mode": "auto" },
  "ledger":     { "enabled": false },
  "extractors": { "utxo": true, "epoch_sync_stats": true },
  "metrics":    { "prometheus_port": 8080 },
  "logging":    { "level": "info", "format": "text" }
}
```

| Section | Purpose |
|---|---|
| `sync` | Reserved. Leave it at `auto`. dbsync ignores this section today and always decides the phase from the database state. |
| `ledger` | Whether to replay blocks through a ledger state and capture rewards, deposits, and protocol parameters. The LedgerDB is on disk, but the replay is the largest RAM consumer in a dbsync run. |
| `extractors` | Which extractors are enabled. |
| `metrics` | Reserved. dbsync exposes no endpoint today (see [Metrics](../operations/metrics)). |
| `logging` | `level` is `debug`, `info`, `warning`, or `error`. `format` is accepted but ignored; dbsync always writes text. |

The `core` extractor is always on and has no `extractors` key. It owns
`block`, `tx`, `slot_leader`, `stake_address`, and `pool_hash`. Every
other extractor is opt-in — omit the key or set it to `false` to
disable.

`epoch` is the one key that defaults to **enabled**. An empty `{}`
config therefore builds the `epoch` tables and views as well as the
core tables. Set `"epoch": false` to turn it off.

## `extractors` is fixed per database

:::warning Choose the extractors before you sync
`extractors` is **fixed once the database is created**. The set of
enabled extractors, the `ledger.enabled` flag, and the `utxo` strategy
are baked into the schema. Changing any of them needs a fresh sync.

Schema migrations upgrade an existing database across dbsync versions.
They do not turn an extractor on or off.
:::

dbsync compares the two sets at boot and **aborts on any difference,
in either direction**. Enabling an extractor the database was not
built with aborts the boot. So does disabling one it *was* built with.
There is no "ignore the extra tables" mode.

Migrations change the database's *shape* as it evolves across dbsync
versions. They never back-fill a newly enabled extractor's tables from
genesis. Adding an extractor therefore means a fresh sync.

The settings you *can* change between runs against the same database
are the operational ones: the pg-config connection file, the logging
level, and the metrics port. None of them affect the schema.

## Which preset to pick

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

## The `extractors` keys

Each key names one extractor. The recognised keys come from
[`DbSync.App.Config.Types`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Config/Types.hs):

`utxo`, `multi_asset`, `metadata`, `stake_delegation`,
`stake_delegation_ledger`, `pool`, `pool_stats`, `scripts_datums`,
`governance`, `cbor`, `epoch_sync_stats`, `epoch_boundary`, `epoch`,
`off_chain_pools`, `off_chain_votes`, `current_state`.

Every key drives a real extractor except `current_state`, which is a
no-op stub today.

Four keys need `ledger.enabled = true`: `stake_delegation_ledger`,
`pool_stats`, `epoch_boundary`, and `current_state`. Three need
another extractor: `multi_asset` needs `utxo`, `off_chain_pools` needs
`pool`, and `off_chain_votes` needs `governance`. The validator
rejects a config that breaks any of these rules.

See [Custom configs](custom) for what each extractor writes, and
[Existing extractors](/developers/extractors/existing) for the
per-table breakdown.

## Validation

At boot dbsync:

1. Parses the config JSON. Malformed JSON and invalid values abort
   with an error.
2. Checks the extractor dependencies. Enabling `multi_asset` without
   `utxo` is rejected with
   `multi_asset extractor requires utxo extractor to be enabled.`
3. Compares the config against the existing database on a resume. Any
   mismatch aborts with a message naming what changed.

:::caution `logging.level` is not validated
The parser rejects an unknown value for `logging.format`,
`ledger.backend`, and `utxo.strategy`. It does **not** reject an
unknown `logging.level`. A typo there silently selects `info`.
:::

:::note
If validation fails, dbsync exits before it connects to the node.
There is no partial state to clean up.
:::
