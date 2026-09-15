---
id: converting
title: Converting from cardano-db-sync
sidebar_position: 4
---

# Converting from cardano-db-sync

The original cardano-db-sync configures behaviour through
`insert_options` in its config file. dbsync replaces that block with
the [`extractors`](custom#the-extractors-reference) and
[`ledger`](custom#ledger) sections. This page maps every old option to
its new spelling, and
lists what is always on, what changed shape, and what did not carry
over.

Remember that `extractors` is
[fixed per database](overview#extractors-is-fixed-per-database):
convert your config before you start the sync, not after.

## Always on — no option needed

Some old options disappeared because the behaviour is now
unconditional:

- **`tx_out.use_address_table`** — the `address` table always exists
  and `tx_out` always references it. There is no inline-address
  variant.
- **`tx_out.value: "consumed"`** — `tx_out.consumed_by_tx_id` is
  populated by default whenever `utxo` is enabled. Turn it off with
  `utxo.consumed_by_tx_id: false`.
- **The `core` extractor** — `block`, `tx`, `slot_leader`,
  `stake_address`, and `pool_hash` are always written. `disable_all`
  behaviour is the default config.

## Option mapping

| Old (`insert_options`) | New | Notes |
|---|---|---|
| `tx_cbor: "enable"` | `extractors.cbor: true` | Same table (`tx_cbor`). |
| `tx_out.value: "enable"` | `extractors.utxo: true` | |
| `tx_out.value: "disable"` | `extractors.utxo: false` | The default. |
| `tx_out.value: "consumed"` | `utxo: true` | `consumed_by_tx_id` is on by default. |
| `tx_out.value: "prune"` | `utxo.strategy: "prune"` | Not yet implemented — the parser rejects it. |
| `tx_out.value: "bootstrap"` | `utxo.strategy: "from_ledger"` | Not yet implemented — the parser rejects it. |
| `tx_out.force_tx_in: true` | `utxo.tx_in: true` | See [`tx_in` below](#tx_in-is-independent). |
| `tx_out.use_address_table` | — | Always on. |
| `ledger: "enable"` | `ledger.enabled: true` | |
| `ledger: "disable"` | `ledger.enabled: false` | The default. |
| `ledger: "ignore"` | — | Dissolves: ledger-derived writes are per-extractor opt-ins, so "ledger on but don't write its data" is just leaving those extractors off. |
| `shelley.enable: true` | `extractors.stake_delegation: true` + `extractors.pool: true` | Split in two. Param proposals moved under `governance`. |
| `multi_asset.enable: true` | `extractors.multi_asset: true` | Requires `utxo`. |
| `metadata.enable: true` | `extractors.metadata: true` | |
| `plutus.enable: true` | `extractors.scripts_datums: true` | `datum`, `script`, `redeemer`, `redeemer_data`, `extra_key_witness`. |
| `governance: "enable"` | `extractors.governance: true` | Works without ledger; only `drep_distr` needs it. |
| `offchain_pool_data: "enable"` | `extractors.off_chain_pools: true` | Requires `pool`. |
| `offchain_vote_data: "enable"` | `extractors.off_chain_votes: true` | Requires `governance`. |
| `pool_stat: "enable"` | `extractors.pool_stats: true` | Requires `ledger.enabled`. |
| `disable_epoch: true` | `extractors.epoch: false` | `epoch` defaults to on. |
| `snapshot_interval.near_tip_epoch` | `ledger.snapshot_near_tip_epoch` | Same meaning, default 580. |
| rewards / epoch stake (implied by `ledger`) | `extractors.stake_delegation_ledger`, `extractors.epoch_boundary` | Ledger-derived tables are explicit opt-ins now. Both require `ledger.enabled`. |

## `tx_in` is independent

In the original, `tx_out.value: "consumed"` stopped populating `tx_in`
unless you set `force_tx_in: true`. In dbsync the two are independent
booleans, both on by default:

- `utxo.tx_in` — the `tx_in` table: which output each input spends,
  plus the redeemer for script-locked spends.
- `utxo.consumed_by_tx_id` — the back-pointer on `tx_out` naming the
  consuming transaction.

All four combinations are valid, except that a `prune` strategy will
require `consumed_by_tx_id` (pruning deletes rows marked consumed).

Converting `"value": "consumed"` without `force_tx_in` therefore means
`tx_in: false`, which keeps fees and deposits intact but leaves
`redeemer.script_hash` `NULL` for spend redeemers. See
[`utxo`](custom#utxo).

## Presets

The old `preset` enum is gone. Copy the closest
[example config](presets) and adjust:

| Old preset | Closest example | Adjust |
|---|---|---|
| `"full"` | `everything.json` | Old `full` excluded `tx_cbor` and the off-chain fetchers; set `cbor`, `off_chain_pools`, `off_chain_votes` to `false` to match. |
| `"only_utxo"` | `utxo-only.json` | Old `only_utxo` also enabled `ma_tx_out`; add `multi_asset: true` to match. |
| `"only_governance"` | — | No direct preset. Start from `minimal.json` and enable `governance` (plus `ledger` if you need `drep_distr`). |
| `"disable_all"` | `minimal.json` | |

## Not carried over

No replacement exists for these; if you depend on one, stay on the
original for now:

- **Whitelists** — `metadata.keys`, `multi_asset.policies`,
  `shelley.stake_addresses`. Extractors are all-or-nothing.
- **`remove_jsonb_from_schema` / `json_type`** — metadata and
  off-chain payload columns are always `jsonb`.
- **`stop_at_block`** — no equivalent; dbsync follows the tip.

## Sticky settings

Like the original's `prune` ("if set once, it must always be set"),
some settings are recorded in the database on first boot and enforced
after that. dbsync refuses to start, with a message naming both
values, if the config no longer matches:

- the enabled extractor set,
- `ledger.enabled`,
- `utxo.consumed_by_tx_id` and `utxo.strategy`,
- the network the database was synced against.

Changing any of these means a fresh database (or
`--resync-from-genesis`).
