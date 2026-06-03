---
id: custom
title: Writing a custom profile
sidebar_position: 3
---

# Writing a custom profile

If none of the [presets](presets) fits exactly, build your own. The
easiest path is to copy the closest preset and trim it down.

## The `db_options` reference

Every key is opt-in. Omitting a key means disabled.

```json
{
  "db_options": {
    "utxo": {
      "enabled": true,
      "consumed_by_tx_id": true,
      "tx_in": true,
      "strategy": "archive"
    },
    "multi_asset":      true,
    "metadata":         true,
    "stake_delegation": true,
    "pool":             true,
    "scripts_datums":   true,
    "governance":       true,
    "cbor":             true,
    "epoch_sync_stats": true,
    "epoch_boundary":   true,
    "epoch":            true,
    "current_state":    true
  }
}
```

The `utxo` key is the one structured option — it has its own knobs.
The rest are plain booleans.

### `utxo`

Outputs, inputs, addresses, collateral, reference inputs.

| Field | Default | Notes |
|---|---|---|
| `enabled` | `false` | Master switch. `false` leaves `tx_out`, `tx_in`, `ma_tx_out` empty. |
| `consumed_by_tx_id` | `true` | Populate `tx_out.consumed_by_tx_id` — the back-pointer from a UTxO to the tx that spent it. |
| `tx_in` | `true` | Write `tx_in` rows. `false` is reserved (an alternate backfill path via `consumed_by_tx_id` is planned but not landed); currently the validator rejects `false`. |
| `strategy` | `"archive"` | What `tx_out` ultimately contains. `archive` keeps every output ever produced; `prune` and `from_ledger` are reserved and currently rejected by the validator. |

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

Stake-address lifecycle. Adds `stake_address`, `stake_registration`,
`stake_deregistration`, `delegation`, `withdrawal`, `pot_transfer`,
`reserve`, `treasury`. No dependencies.

### `pool`

Stake-pool registrations and retirements. Adds `pool_hash`,
`pool_update`, `pool_metadata_ref`, `pool_owner`, `pool_relay`,
`pool_retire`. Requires `stake_delegation`.

### `scripts_datums`

:::caution Reserved
Will cover Plutus scripts, datums, redeemers, and redeemer-data.
Currently a stub — enabling it is accepted but nothing gets
populated.
:::

### `governance`

:::caution Reserved
Will cover Conway-era governance: proposals, votes, voters, vote
anchors. Currently a stub.
:::

### `cbor`

Raw transaction CBOR bytes in `tx_cbor`. Useful for consumers that
need to re-serialise or replay txs. Large (~tx-body bytes per tx) —
opt in only when you need it. No dependencies.

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
Will cover per-pool / per-account snapshot tables. Currently a stub.
:::

## The other sections

### `database`

```json
"database": {
  "host":     "localhost",
  "port":     5432,
  "name":     "cexplorer",
  "user":     "",
  "password": ""
}
```

Standard PostgreSQL connection fields. Empty `user` falls back to the
OS user (peer authentication on a local install). Empty `password` is
fine for local; production deployments should use `.pgpass` or set
the field.

### `sync`

```json
"sync": { "mode": "auto" }
```

Three values:

- `auto` — detect the boot mode from the database state and ledger
  snapshots. Use this in production.
- `ingest` — force `IngestChainHistory`. Diagnostic; refuses to run
  if `sync_complete` is true.
- `follow` — force `FollowingVolatileTail`. Diagnostic; assumes the
  database is already populated.

### `ledger`

```json
"ledger": { "enabled": true }
```

When enabled, dbsync applies blocks to an in-RAM ledger state to
produce reward, deposit, and protocol-parameter data. Adds roughly
8 GB to the steady-state RAM footprint and ~11 GB to disk at mainnet
tip (the LSM-backed on-disk UTxO set).

Without ledger, the schema is created but `ada_pots`, `epoch_param`,
`epoch_state`, `rewards`, `epoch_stake` etc. stay empty.

`backend` accepts only `"lsm"` (the on-disk backend); an `"inmemory"`
backend was considered but isn't supported. `snapshot_near_tip_epoch`
defaults to 580 — past that epoch the ledger writes a snapshot every
epoch boundary. The default works for mainnet and you shouldn't need
to change it.

### `metrics`

```json
"metrics": { "prometheus_port": 8080 }
```

The Prometheus endpoint isn't wired up yet — see
[Metrics](../operations/metrics). The field is honoured by the config
parser but no server listens on the port today.

### `logging`

```json
"logging": { "level": "info", "format": "text" }
```

| Field | Values | Notes |
|---|---|---|
| `level` | `error` / `warn` / `info` / `debug` / `trace` | `info` is the production default; `debug` adds per-phase timing and step-by-step Prep logs. |
| `format` | `text` / `json` | `text` is human-readable; `json` produces structured one-line-per-event log records. |

## Tips

:::tip Start from the closest preset
Copy `everything-no-ledger-profile.json` and remove what you don't
need rather than building up from `minimal`. The result is closer
to what you actually want, and you won't accidentally drop a
shared-dedup dependency the validator would otherwise reject.
:::

- Leave `epoch_sync_stats` enabled unless you have a specific
  reason not to — every preset turns it on and the cost is
  negligible.
- Don't enable a reserved option expecting tables to appear. The
  validator accepts them so future releases can wire them up without
  a config-shape break, but today they're no-ops.
- The profile travels across environments: same JSON in dev,
  staging, prod. Operational paths (socket, ledger state) come from
  the CLI, not the profile, so the same JSON works against any node
  setup.
