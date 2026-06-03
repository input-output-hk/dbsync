---
id: presets
title: Preset profiles
sidebar_position: 2
---

# Preset profiles

Six ready-to-use profiles ship in
[`profiles/`](https://github.com/input-output-hk/dbsync/tree/main/profiles).
Each is a small JSON file you can copy and adjust if needed.

Pick one based on what you'll query, not on what looks comprehensive
— disabling projections you don't need is the biggest single lever
on sync time and disk usage.

| Preset | Projections enabled | Ledger | Approximate size (mainnet) |
|---|---|---|---|
| `minimal-profile.json` | (core only) | off | ~30 GB |
| `utxo-only-profile.json` | `utxo` | off | ~160 GB |
| `spo-profile.json` | `utxo`, `metadata`, `stake_delegation`, `pool` | off | ~185 GB |
| `dapp-profile.json` | `utxo`, `multi_asset`, `metadata`, `scripts_datums`† | off | ~265 GB |
| `everything-no-ledger-profile.json` | every implemented projection | off | ~475 GB |
| `everything-profile.json` | every implemented projection | on | ~480 GB |

The `everything-profile` figure is measured at mainnet tip; the
smaller-profile figures are approximations derived from that
profile's per-table breakdown.

† `scripts_datums` is currently a stub; enabling it doesn't yet
populate scripts/datums tables. See
[Existing extractors](/developers/extractors/existing) for the
current implementation status.

Every preset also enables `epoch_sync_stats` so the per-epoch sync
progress table gets populated. It's cheap and useful for diagnostics.

:::tip The `cbor` tax
`tx_cbor` (raw transaction CBOR bytes) accounts for ~218 GB on its
own at mainnet tip — nearly half of an `everything`-profile
database. Both `everything-no-ledger-profile.json` and
`everything-profile.json` enable `cbor` by default. If your
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
database. If you need those, use `everything-profile` instead.

## `dapp`

DApp / explorer view. Adds `multi_asset`, `metadata`, and
`scripts_datums` on top of `utxo`.

Tables it gives you:

- `multi_asset`, `ma_tx_mint`, `ma_tx_out` for native tokens.
- `tx_metadata` for arbitrary on-chain metadata.
- `scripts_datums` tables (reserved — Plutus scripts, datums,
  redeemers; not yet implemented).

Suitable for:

- NFT / token explorers.
- DApps that need on-chain metadata payloads.
- Plutus contract analytics (once `scripts_datums` lands).

## `everything-no-ledger`

Every implemented projection except the ledger-derived ones. Adds
`stake_delegation`, `pool`, `governance` (reserved), `cbor`, and
`epoch_boundary` on top of `dapp`.

Suitable for:

- Block-explorer backends that need the full schema.
- Downstream consumers that re-serialise transactions (the `cbor`
  extractor stores raw tx bytes).

The `epoch_boundary` and `governance` projections are enabled but
their per-block work is mostly a no-op without ledger state — the
tables get created but stay empty.

## `everything`

`everything-no-ledger` plus `ledger.enabled = true` and the
`epoch_boundary` and `current_state` (reserved) projections.

With ledger on, the consumer applies blocks to an in-RAM ledger
state and the worker produces:

- Per-block deposit maps (tx-level deposit amounts).
- Per-epoch `ada_pots`, `epoch_param`, `epoch_state`, `stake_dist`,
  `rewards`, `epoch_stake`.

This is the largest, slowest profile, and the one that gives you the
full upstream-dbsync-equivalent schema.

Mainnet sync time on a 4-core / 16 GB target is on the order of a
day for `everything`; less for the smaller profiles.

## Switching profiles

You can't switch profiles in place — see [Profile immutability](overview#profile-immutability).
A switch means a fresh sync against a fresh database. Plan the
profile choice before you commit to a multi-day sync.
