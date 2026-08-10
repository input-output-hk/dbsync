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
— disabling extractors you don't need is the biggest single lever
on sync time and disk usage.

| Preset | Extractors enabled | Ledger |
|---|---|---|
| `minimal.json` | none beyond the defaults | off |
| `utxo-only.json` | `utxo` | off |
| `spo.json` | `utxo`, `metadata`, `stake_delegation`, `pool` | off |
| `dapp.json` | `utxo`, `multi_asset`, `metadata`, `scripts_datums` | off |
| `everything-no-ledger.json` | all non-ledger extractors | off |
| `everything.json` | every extractor, ledger on | on |

Sizes for each preset are in
[Prerequisites](../installation/prerequisites#disk-space).

Two extractors run in more presets than the table shows:

- **`epoch_sync_stats`** — every preset enables it. It fills the
  per-epoch progress table, which is cheap and the best diagnostic you
  have. See [Metrics](../operations/metrics).
- **`epoch`** — enabled by default, so `minimal.json` and both
  `everything` presets build the `epoch` tables and views. `dapp`,
  `spo`, and `utxo-only` set `"epoch": false` explicitly.

:::tip The `cbor` tax
`cbor` stores raw transaction CBOR and is the single largest
contributor to the database, roughly half of an `everything` sync.
Both `everything` presets enable it.

Copy one of them and set `"cbor": false` unless your consumers
re-serialise or replay transactions.
:::

## `minimal`

Block and transaction index only. The `core` extractor writes `block`,
`tx`, `slot_leader`, `stake_address`, and `pool_hash`. No UTxO, no
certificates, no metadata. Suitable for:

- Hash-to-height resolution.
- Per-block summary queries (block count per epoch, slot leader
  distribution).
- Verifying that your dbsync stack works end-to-end before you commit
  to a larger extractor set.

This is the fastest sync available, which makes it a good sanity check
on new hardware. It answers no wallet, balance, or asset query.

## `utxo-only`

The `utxo` extractor on top of `core`. Adds `tx_out`, `tx_in`,
`collateral_tx_in`, `collateral_tx_out`, `reference_tx_in`, and
`address`.

Suitable for:

- Wallet backends — resolve an address to its current UTxO set.
- Balance queries — sum unconsumed outputs for an address.
- Spend-tracing — `tx_in.tx_out_id` resolves to the producing output.

`consumed_by_tx_id` is on by default. It writes a back-pointer from
each `tx_out` to the transaction that spent it. `tx_in` is on by
default too.

The `archive` strategy stores every output ever produced. It is the
only strategy that works: the parser rejects `prune` and
`from_ledger`.

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

The broad extractor set that doesn't need ledger state. Adds
`stake_delegation`, `pool`, `governance`, and `cbor` on top of `dapp`.
It deliberately leaves out the ledger-derived extractors
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

`ledger.enabled = true` plus every extractor: everything in
`everything-no-ledger`, and additionally the ledger-derived
`stake_delegation_ledger`, `pool_stats`, and `epoch_boundary`, the
`off_chain_pools` / `off_chain_votes` metadata fetchers, and the
reserved `current_state` stub.

With the ledger on, the ledger worker replays every block through a
ledger state held in an on-disk LSM-tree, and produces:

- Per-block deposit maps, which fill the transaction-level deposit
  amounts.
- Per-epoch `ada_pots`, `epoch_param`, `epoch_state`, `reward`,
  `pot_reward`, `epoch_stake`, and `pool_stat`.

This is the largest and slowest preset. It is also the one that gives
you the schema closest to the original cardano-db-sync.

## Switching presets

You cannot switch in place. See
[`extractors` is fixed per database](overview#extractors-is-fixed-per-database).

A switch means a fresh sync against a fresh database. Choose before
you commit to a long sync.
