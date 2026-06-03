---
id: existing
title: Existing extractors
sidebar_position: 3
---

# Existing extractors

The projections that ship with dbsync. Names match the keys in a
profile's `db_options` block. Every extractor lives under
`DbSync.Extractor.*` in the [`dbsync` package](https://github.com/input-output-hk/dbsync/tree/main/dbsync/src/DbSync/Extractor);
each is registered in
[`DbSync.App.Setup.buildExtractors`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Setup.hs).

| Extractor | Status | Always on? | Tables owned |
|---|---|---|---|
| `core` | Implemented | yes | `block`, `tx`, `slot_leader`, `meta`, `reverse_index` |
| `utxo` | Implemented | no | `tx_out`, `tx_in`, `collateral_tx_in`, `collateral_tx_out`, `reference_tx_in`, `address` |
| `multi_asset` | Implemented | no | `multi_asset`, `ma_tx_mint`, `ma_tx_out` |
| `metadata` | Implemented | no | `tx_metadata` |
| `stake_delegation` | Implemented | no | `stake_address`, `stake_registration`, `stake_deregistration`, `delegation`, `withdrawal`, `pot_transfer`, `reserve`, `treasury` |
| `pool` | Implemented | no | `pool_hash`, `pool_update`, `pool_metadata_ref`, `pool_owner`, `pool_retire`, `pool_relay` |
| `cbor` | Implemented | no | `tx_cbor` |
| `epoch` | Implemented | no | `epoch_finalized` + the `epoch` / `epoch_current` views |
| `epoch_boundary` | Implemented | no | `ada_pots`, `epoch_param`, `epoch_state`, `cost_model` |
| `epoch_sync_stats` | Implemented | no | `epoch_sync_stats` |
| `scripts_datums` | Stub (not wired up yet) | no | — |
| `governance` | Stub (not wired up yet) | no | — |
| `current_state` | Stub (not wired up yet) | no | — |

:::caution Reserved stubs
The three stubs (`scripts_datums`, `governance`, `current_state`)
come from `resolveExtractor` returning a no-op `ExtractorDef`.
Enabling them in a profile is allowed (the validator doesn't reject
them) but produces nothing. Their schema and processing logic land
in subsequent passes.
:::

## `core`

The unconditional extractor. Every other extractor's tables FK into
its rows — there's no profile where `core` is off.

What it writes per block:

- `slot_leader` — once per VRF key, deduped through the resolver.
- `block` — one row, using the pre-assigned `BlockId`.
- `tx` — one per transaction, with phase-aware fee and deposit fields.

The phase-aware bit is real but small. In Ingest, `core` writes the
phase-1 fee straight from the tx body and leaves the deposit column
NULL where it depends on ledger state; the
[`PreparingForVolatileTail`](../phases/preparing) backfill fills the
deposit column from `epoch_param_pending`. In Follow with ledger on,
both values come straight from the ledger worker's `BlockLedgerData`.

Owns `slot_leader.pool_hash_id` via a lookup the pipeline performs
before any extractor runs — this avoids a circular `core` → `pool`
dependency.

## `utxo`

Outputs, inputs, addresses. Depends on `core` and `stake_delegation`
(addresses with inline stake credentials FK into `stake_address`).

Writes per tx:

- `tx_out` — one per output, with the pre-assigned `TxOutId`.
- `tx_in` — one per consumed input. `tx_out_id` (the FK to the
  producing output) is filled inline during Ingest when the producer
  is in the per-block UTxO cache, otherwise it's NULL and resolved at
  Prep time.
- `collateral_tx_in`, `collateral_tx_out`, `reference_tx_in` — the
  Babbage+ shapes.
- `address` — deduped by raw bytes, populated lazily through the
  TxOut worker during Ingest and synchronously during Follow.

Phase-2-failed transactions don't write `tx_out`, `tx_in`, or
collateral; only their collateral outputs and inputs that consume
collateral show up (matching the original cardano-db-sync semantics).

Three knobs on the `utxo` option:

- `consumed_by_tx_id` — populate `tx_out.consumed_by_tx_id`. Backed
  by the [TxOut worker](../workers#txout-worker)'s
  consumed-by buffer.
- `tx_in` — write `tx_in` rows at all. Today the option must be
  `true`; `false` is reserved for an alternate backfill path that
  hasn't landed.
- `strategy: archive | prune | from_ledger` — what `tx_out` ultimately
  contains. Only `archive` is implemented; the other two are stubs.

## `multi_asset`

Native tokens. Depends on `core` and `utxo`.

Per tx (skipped for phase-2 failures):

- `multi_asset` — deduped by `(policy, name)`. Stable
  `MultiAssetId` for the lifetime of the asset, assigned through the
  resolver.
- `ma_tx_mint` — one row per `(policy, name, quantity)` mint or burn.
- `ma_tx_out` — one row per `(asset, quantity)` carried on a `tx_out`.

The dedup table is one of the LSM-backed Ingest stores; in Follow
the dedup goes through a `SELECT … WHERE policy = ? AND name = ?`
followed by `INSERT … RETURNING id` if it's new.

## `metadata`

One `tx_metadata` row per metadata key in the transaction (keyed by
the JSON-encodable `Word64` label). Stores both the no-schema JSON
rendering of the value and the CBOR encoding of the single-key
singleton map, matching what upstream dbsync stores.

Phase-2 failures are skipped (no on-chain metadata for them).

## `stake_delegation`

Stake-address lifecycle and reward withdrawals. Depends on `core`.

Per tx (skipped for phase-2 failures):

- `stake_address` — deduped by raw stake credential; resolves the
  Bech32 view at write time.
- `stake_registration`, `stake_deregistration`, `delegation` — one row
  per certificate of that kind.
- `withdrawal` — one row per reward withdrawal.
- `pot_transfer`, `reserve`, `treasury` — Shelley MIR certificates
  fan out into these three depending on which pot is involved.

Owns the cross-extractor `stake_address` table because it's the
extractor most likely to be enabled when stake data is needed; other
extractors (`utxo`, `pool`) share its dedup helper.

## `pool`

Stake-pool lifecycle. Depends on `core` and `stake_delegation` (pool
owners and reward addresses resolve as stake addresses).

Per tx (skipped for phase-2 failures):

- `pool_hash` — deduped by 28-byte key hash.
- `pool_update` — one row per registration certificate (incl.
  re-registrations).
- `pool_metadata_ref` — when the registration carries metadata.
- `pool_owner` — one row per declared owner.
- `pool_relay` — one row per declared relay.
- `pool_retire` — one row per retirement certificate.

`pool_update.deposit` is populated when ledger is on (from the
worker-supplied protocol-param pool deposit) and NULL otherwise,
matching the original schema for ledger-disabled runs.

## `cbor`

Stores the raw CBOR-encoded transaction bytes in `tx_cbor`. Useful
for downstream consumers that need to re-serialise or replay txs.

:::warning Big single contributor
`tx_cbor` accounts for ~218 GB on its own at mainnet tip — roughly
half of an `everything`-profile database. Both
`everything-no-ledger-profile.json` and `everything-profile.json`
enable `cbor` by default. If your consumers don't need raw CBOR,
disabling this one option nearly halves the database.
:::

Byron txs don't carry CBOR through the parser; `cbor` skips them.

## `epoch`

Owns `epoch_finalized` plus the `epoch` and `epoch_current` views
that join the finalized state with the per-epoch ledger data.

`pdProcess` is a no-op. The table is populated by three SQL hooks
rather than per-block COPY:

- A backfill statement at the end of Ingest writes one row per epoch
  fully consumed by the bulk-load pass.
- An append statement runs at every Follow epoch boundary.
- A delete-past-slot statement runs as part of the Follow rollback
  cascade.

Registering the extractor is the way to get the table and views
created.

## `epoch_boundary`

Owns the boundary-triggered ledger tables: `ada_pots`, `epoch_param`,
`epoch_state`, `cost_model`.

`pdProcess` is a no-op. The consumer calls `runEpochBoundary`
([`DbSync.Extractor.EpochBoundary`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Extractor/EpochBoundary.hs))
when it detects an epoch transition and the ledger worker has the
matching `ApplyResult`. With ledger off the consumer never calls it
and these tables stay empty — but the DDL still runs so operators can
flip ledger on without re-syncing.

## `epoch_sync_stats`

Per-epoch sync performance metrics. The table is `epoch_sync_stats`;
`pdProcess` is a no-op (the consumer writes rows directly at the
epoch boundary commit). Defining it as an extractor is the lightest
way to get the schema created.

## `scripts_datums`, `governance`, `current_state`

Reserved. Their option keys are accepted in the profile so a future
release can wire them up without a configuration-schema change, but
today they map to a stub `ExtractorDef` with no tables and a no-op
process function.

When implemented they'll own:

- `scripts_datums` — Plutus scripts, datums, redeemers, and the
  redeemer-data tables.
- `governance` — Conway-era proposals, votes, voters, and vote
  anchors.
- `current_state` — the per-pool / per-account snapshot tables that
  upstream dbsync produces.
