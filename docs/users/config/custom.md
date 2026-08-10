---
id: custom
title: Writing a custom config
sidebar_position: 3
---

# Writing a custom config

If none of the [presets](presets) fits exactly, build your own. The
easiest path is to copy the closest preset and trim it down.

## The `extractors` reference

Every key is opt-in. Omitting a key means disabled.

```json
{
  "extractors": {
    "utxo": {
      "enabled": true,
      "consumed_by_tx_id": true,
      "tx_in": true,
      "strategy": "archive"
    },
    "multi_asset":             true,
    "metadata":                true,
    "stake_delegation":        true,
    "stake_delegation_ledger": true,
    "pool":                    true,
    "pool_stats":              true,
    "scripts_datums":          true,
    "governance":              true,
    "off_chain_pools":         true,
    "off_chain_votes":         true,
    "cbor":                    true,
    "epoch_sync_stats":        true,
    "epoch_boundary":          true,
    "epoch":                   true,
    "current_state":           true
  }
}
```

The `utxo` key is the one structured option — it has its own knobs.
The rest are plain booleans.

### `utxo`

Outputs, inputs, addresses, collateral, reference inputs.

| Field | Default | Notes |
|---|---|---|
| `enabled` | `false` | Master switch. `false` leaves `tx_out`, `tx_in`, and `ma_tx_out` empty. |
| `consumed_by_tx_id` | `true` | Write `tx_out.consumed_by_tx_id`, the back-pointer from a UTxO to the transaction that spent it. |
| `tx_in` | `true` | Write `tx_in` rows. The parser rejects `false`. |
| `strategy` | `"archive"` | `archive` keeps every output ever produced. The parser rejects `prune` and `from_ledger`. |

The parser, not the validator, rejects the reserved values. dbsync
therefore fails while reading the file, before any dependency check
runs.

Either `"utxo": true` (shorthand for the defaults with `enabled: true`)
or the full object form is accepted.

### `multi_asset`

Native tokens. Adds `multi_asset`, `ma_tx_mint`, `ma_tx_out`.
Requires `utxo`.

```json
"multi_asset": true
```

### `metadata`

Transaction metadata. Adds `tx_metadata` — one row per top-level
metadata key per tx. Stores the JSON rendering and the singleton-CBOR
encoding. No dependencies.

### `stake_delegation`

Stake-address lifecycle. Adds `stake_registration`,
`stake_deregistration`, `delegation`, `withdrawal`, `pot_transfer`,
`reserve`, `treasury`. No dependencies (the `stake_address` table it
populates is owned by `core`).

### `stake_delegation_ledger`

Ledger-derived stake and reward data. Adds `reward`, `pot_reward`,
`epoch_stake`, and `epoch_stake_progress`. Requires `ledger.enabled =
true` — the validator rejects the config otherwise.

### `pool`

Stake-pool registrations and retirements. Adds `pool_update`,
`pool_metadata_ref`, `pool_owner`, `pool_relay`, `pool_retire`. No
dependencies beyond `core` (the `pool_hash` table it populates is
core-owned).

### `pool_stats`

Per-epoch pool distribution in `pool_stat` (one row per pool per
epoch). Requires `ledger.enabled = true`.

### `scripts_datums`

Plutus and native-script witness data. Adds `script`, `datum`,
`redeemer`, `redeemer_data`, and `extra_key_witness`; the `script`,
`datum`, and `redeemer_data` rows are deduped on their hash.
`redeemer.fee` needs `ledger.enabled = true` — the fee is priced from
the block's protocol parameters. No dependencies beyond `core`.

### `governance`

Conway-era governance: DRep and committee certificates, governance
proposals, voting procedures, and the constitution. Adds sixteen
tables including `drep_hash`, `drep_registration`, `drep_distr`,
`delegation_vote`, `gov_action_proposal`, `voting_procedure`,
`voting_anchor`, `constitution`, and the `committee*` family. No
dependencies beyond `core`; `drep_distr` is only populated when
`ledger.enabled = true`.

### `off_chain_pools`

Pool off-chain metadata, fetched over HTTP by a background worker.
Adds `off_chain_pool_data`, `off_chain_pool_fetch_error`,
`delisted_pool`, and `reserved_pool_ticker`. Requires `pool`.

### `off_chain_votes`

Governance off-chain metadata (Conway vote anchors), fetched over HTTP
by a background worker. Adds the seven `off_chain_vote_*` tables.
Requires `governance`.

### `cbor`

Raw transaction CBOR bytes in `tx_cbor`. Enable it only if your
consumers re-serialise or replay transactions.

This is the largest single extractor by a wide margin — it stores the
whole transaction body for every transaction on the chain. No
dependencies.

### `epoch_sync_stats`

Per-epoch sync performance metrics in `epoch_sync_stats`. Cheap and
useful for diagnostics. The shipped presets all enable this. No
dependencies.

### `epoch_boundary`

Adds `ada_pots`, `epoch_param`, `epoch_state`, `cost_model`.
Populated by `runEpochBoundary` when the ledger worker produces the
matching boundary output — i.e. these tables stay empty unless
`ledger.enabled = true`. The schema is created regardless so flipping
ledger on later requires only a re-sync, not a code change.

### `epoch`

Adds the `epoch_finalized` table plus the `epoch` and `epoch_current`
views that join finalized state with per-epoch ledger data.
Populated by SQL hooks at Ingest end / Follow boundary / Follow
rollback. Default: enabled.

### `current_state`

:::caution Reserved
This will cover per-pool and per-account snapshot tables. It is a stub
today and writes nothing.

It still requires `ledger.enabled = true`. The validator rejects the
config if you enable it without the ledger.
:::

## The other sections

The PostgreSQL connection is *not* part of the config — it lives in
the separate file passed via `--pg-config`. See [the PostgreSQL
connection file](../running#the-postgresql-connection-file).

### `sync`

```json
"sync": { "mode": "auto" }
```

:::caution Reserved
dbsync parses this section and then ignores it. No production code
reads `sync.mode`. The boot path always decides the phase from the
database state.

Leave it at `auto`, or omit the section.
:::

### `ledger`

```json
"ledger": { "enabled": true }
```

When enabled, dbsync replays every block through a ledger state to
produce reward, deposit, and protocol-parameter data.

The LedgerDB is an LSM-tree **on disk**, under `--ledger-state-dir`,
not an in-memory structure. It still holds caches and a checkpoint
buffer in RAM, and the replay is the largest RAM consumer in a dbsync
run.

Without the ledger, dbsync creates the schema but `ada_pots`,
`epoch_param`, `epoch_state`, `reward`, `pot_reward`, and
`epoch_stake` stay empty.

| Field | Default | Notes |
|---|---|---|
| `enabled` | `false` | |
| `backend` | `"lsm"` | The only accepted value. |
| `snapshot_near_tip_epoch` | `580` | Past this epoch, the ledger writes a snapshot at every epoch boundary. The default suits mainnet. |

### `metrics`

```json
"metrics": { "prometheus_port": 8080 }
```

:::caution Reserved
dbsync parses this section and then ignores it. No server listens on
the port. See [Metrics](../operations/metrics).
:::

### `logging`

```json
"logging": { "level": "info" }
```

| Field | Values | Notes |
|---|---|---|
| `level` | `error` / `warning` / `info` / `debug` | `info` is the production default. `debug` adds per-block progress and receiver traces. |
| `format` | `text` / `json` | Parsed but ignored. dbsync always writes text. |

:::caution A bad `level` is not an error
`level` accepts only the four values above. dbsync silently falls back
to `info` on anything else, so a typo makes your logs quieter, not
louder.

There is no `trace` level.
:::

## Tips

:::tip Start from the closest preset
Copy `everything-no-ledger.json` and remove what you don't need
rather than building up from `minimal`. The result is closer
to what you actually want, and you won't accidentally drop a
shared-dedup dependency the validator would otherwise reject.
:::

- Leave `epoch_sync_stats` enabled. Every preset turns it on, the cost
  is negligible, and it is the only per-epoch record you get.
- Do not enable a reserved option and expect tables to appear.
  `current_state` writes nothing today.
- The same config travels across environments. Connection details come
  from the pg-config file and operational paths come from the CLI, so
  one JSON file works against any node setup.
