---
id: presets
title: Preset configs
sidebar_position: 2
---

# Preset configs

Six ready-to-use configs ship in
[`config-examples/`](https://github.com/input-output-hk/dbsync/tree/main/config-examples).
Each is a small JSON file you can copy and adjust if needed.

Pick one based on what you'll query, not on what looks comprehensive
— disabling projections you don't need is the biggest single lever
on sync time and disk usage.

| Preset | Projections enabled | Ledger | Approximate size (mainnet) |
|---|---|---|---|
| `minimal.json` | (core only) | off | ~30 GB |
| `utxo-only.json` | `utxo` | off | ~160 GB |
| `spo.json` | `utxo`, `metadata`, `stake_delegation`, `pool` | off | ~185 GB |
| `dapp.json` | `utxo`, `multi_asset`, `metadata`, `scripts_datums` | off | ~265 GB |
| `everything-no-ledger.json` | all non-ledger projections | off | ~475 GB |
| `everything.json` | every projection, ledger on | on | ~480 GB |

The `everything` figure is measured at mainnet tip; the
smaller-preset figures are approximations derived from its
per-table breakdown.

Every preset also enables `epoch_sync_stats` so the per-epoch sync
progress table gets populated. It's cheap and useful for diagnostics.

:::tip The `cbor` tax
`tx_cbor` (raw transaction CBOR bytes) accounts for ~218 GB on its
own at mainnet tip — nearly half of an `everything`-profile
database. Both `everything-no-ledger.json` and
`everything.json` enable `cbor` by default. If your
consumers don't need to re-serialise or replay transactions,
copying one of those presets and setting `"cbor": false` cuts the
database from ~475 GB to ~260 GB.
:::

## `minimal`

Block / tx index only. The `core` extractor writes `block`, `tx`, and
`slot_leader` rows; no UTxO, no certificates, no metadata. Suitable
for:

- Hash-to-height resolution.
- Per-block summary queries (block count per epoch, slot leader
  distribution).
- Verifying that your dbsync stack is wired up end-to-end before
  committing to a larger profile.

It's the fastest possible sync — useful as a sanity check on new
hardware. Not useful for any wallet, balance, or asset query.

## `utxo-only`

The `utxo` projection on top of `core`. Adds `tx_out`, `tx_in`,
`collateral_tx_in`, `collateral_tx_out`, `reference_tx_in`, and
`address`.

Suitable for:

- Wallet backends — resolve an address to its current UTxO set.
- Balance queries — sum unconsumed outputs for an address.
- Spend-tracing — `tx_in.tx_out_id` resolves to the producing output.

`consumed_by_tx_id` is enabled by default (populates a back-pointer
from each `tx_out` to the tx that spent it). `tx_in` is enabled by
default. The `archive` strategy stores every output ever produced;
the `prune` and `from_ledger` strategies are reserved but not yet
implemented.

## `spo`

Stake-pool operator view. Adds `metadata`, `stake_delegation`, and
`pool` on top of `utxo`.

Tables it gives you:

- `stake_address`, `stake_registration`, `stake_deregistration`,
  `delegation`, `withdrawal`.
- `pool_hash`, `pool_update`, `pool_metadata_ref`, `pool_owner`,
  `pool_relay`, `pool_retire`.
- `tx_metadata` for general tx metadata queries.

Suitable for:

- Pool dashboards — list of registered pools, current delegation,
  retirement schedule.
- Tracking a specific stake address's delegation history.
- Reward-address resolution.

Ledger is off, so reward amounts and deposit values aren't in the
database. If you need those, use the `everything` preset instead.

## `dapp`

DApp / explorer view. Adds `multi_asset`, `metadata`, and
`scripts_datums` on top of `utxo`.

Tables it gives you:

- `multi_asset`, `ma_tx_mint`, `ma_tx_out` for native tokens.
- `tx_metadata` for arbitrary on-chain metadata.
- `datum`, `script`, `redeemer`, `redeemer_data`, `extra_key_witness`
  for Plutus and native-script witness data.

Suitable for:

- NFT / token explorers.
- DApps that need on-chain metadata payloads.
- Plutus contract analytics.

## `everything-no-ledger`

The broad projection set that doesn't need ledger state. Adds
`stake_delegation`, `pool`, `governance`, and `cbor` on top of `dapp`.
It deliberately leaves out the ledger-derived projections
(`stake_delegation_ledger`, `pool_stats`, `epoch_boundary`) and the
off-chain metadata fetchers (`off_chain_pools`, `off_chain_votes`).

Suitable for:

- Block-explorer backends that need the full on-chain schema without
  reward / stake-distribution data.
- Downstream consumers that re-serialise transactions (the `cbor`
  extractor stores raw tx bytes).

`governance` records proposals, votes, and DRep / committee
certificates straight from transaction bodies, so it works here
without ledger. Only its `drep_distr` (per-epoch DRep voting power)
stays empty, since that's derived from ledger state.

## `everything`

`ledger.enabled = true` plus every projection: everything in
`everything-no-ledger`, and additionally the ledger-derived
`stake_delegation_ledger`, `pool_stats`, and `epoch_boundary`, the
`off_chain_pools` / `off_chain_votes` metadata fetchers, and the
reserved `current_state` stub.

With ledger on, the consumer applies blocks to an in-RAM ledger state
and the worker produces:

- Per-block deposit maps (tx-level deposit amounts).
- Per-epoch `ada_pots`, `epoch_param`, `epoch_state`, `reward`,
  `pot_reward`, `epoch_stake`, and `pool_stat`.

This is the largest, slowest profile, and the one that gives you the
full upstream-cardano-db-sync-equivalent schema.

## Switching profiles

You can't switch profiles in place — see [Profile immutability](overview#profile-immutability).
A switch means a fresh sync against a fresh database. Plan the
profile choice before you commit to a multi-day sync.
