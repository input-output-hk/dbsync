---
id: existing
title: Existing extractors
sidebar_position: 3
---

# Existing extractors

The projections that ship with dbsync. Names match the keys in a
config's `db_profile` block. Every extractor lives under
`DbSync.Extractor.*` in the [`dbsync` package](https://github.com/input-output-hk/dbsync/tree/main/dbsync/src/DbSync/Extractor);
each is registered in
[`DbSync.App.Setup.buildExtractors`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Setup.hs).

| Extractor | Always on? | Tables owned |
|---|---|---|
| `core` | yes | `block`, `tx`, `slot_leader`, `stake_address`, `pool_hash` |
| `utxo` | no | `tx_out`, `tx_in`, `collateral_tx_in`, `collateral_tx_out`, `reference_tx_in`, `address` |
| `multi_asset` | no | `multi_asset`, `ma_tx_mint`, `ma_tx_out` |
| `metadata` | no | `tx_metadata` |
| `stake_delegation` | no | `stake_registration`, `stake_deregistration`, `delegation`, `withdrawal`, `pot_transfer`, `reserve`, `treasury` |
| `stake_delegation_ledger` | no | `reward`, `pot_reward`, `epoch_stake`, `epoch_stake_progress` |
| `pool` | no | `pool_update`, `pool_metadata_ref`, `pool_owner`, `pool_retire`, `pool_relay` |
| `scripts_datums` | no | `datum`, `script`, `redeemer`, `redeemer_data`, `extra_key_witness` |
| `governance` | no | `drep_hash`, `drep_registration`, `drep_distr`, `delegation_vote`, `gov_action_proposal`, `voting_procedure`, `voting_anchor`, `constitution`, `committee`, `committee_hash`, `committee_member`, `committee_registration`, `committee_de_registration`, `param_proposal`, `treasury_withdrawal`, `event_info` |
| `cbor` | no | `tx_cbor` |
| `epoch_sync_stats` | no | `epoch_sync_stats` |
| `epoch_boundary` | no | `ada_pots`, `epoch_param`, `epoch_state`, `cost_model` |
| `pool_stats` | no | `pool_stat` |
| `epoch` | no | `epoch_finalized` + the `epoch` / `epoch_current` views |
| `off_chain_pools` | no | `off_chain_pool_data`, `off_chain_pool_fetch_error`, `delisted_pool`, `reserved_pool_ticker` |
| `off_chain_votes` | no | the seven `off_chain_vote_*` tables |
| `current_state` | no | — (reserved stub) |

Every extractor except `core` is opt-in through its `db_profile` key.
Several also need `ledger.enabled = true` to produce anything:
`stake_delegation_ledger`, `pool_stats`, and `epoch_boundary` are
driven by the ledger worker's per-epoch output, and `core` / `pool`
fill their ledger-derived columns (deposits, refunds) only when ledger
is on.

:::caution `current_state` is a reserved stub
`current_state` has no implementation yet — it's intentionally absent
from `allKnownExtractors`, so `resolveExtractor` maps it to a no-op
`ExtractorDef`. Enabling it in a profile is accepted (the validator
doesn't reject it) but produces nothing and creates no tables.
:::

## `core`

The unconditional extractor. Every other extractor's tables FK into
its rows — there's no profile where `core` is off. It also owns the
shared `stake_address` and `pool_hash` tables, so they are always
present; the pipeline's dedup helper fills them on behalf of whichever
extractors are enabled.

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

Outputs, inputs, addresses. Depends on `core`; addresses with inline
stake credentials FK into the core-owned `stake_address` table.

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

The `stake_address` table is owned by `core`, so it is always present;
this extractor and others (`utxo`, `pool`) populate it through the
shared dedup helper.

## `stake_delegation_ledger`

Ledger-derived stake and reward data. Requires `ledger.enabled =
true`; with ledger off its boundary pass never runs and all four
tables stay empty. Every row is an IDENTITY leaf — PostgreSQL
allocates the ids.

- `epoch_stake` — per-`(stake, pool, epoch)` active stake, emitted in
  per-block slices of the ledger's "mark" snapshot so no single block
  has to materialise the whole set.
- `epoch_stake_progress` — one row per epoch recording that its
  `epoch_stake` slicing completed.
- `reward` — block-production rewards (leader and member) and
  pool-deposit refunds, written at the epoch boundary from the ledger
  worker's event stream.
- `pot_reward` — pot-sourced payouts: MIR distributions
  (Shelley→Babbage) and Conway-era enacted treasury withdrawals.

Two known divergences from upstream cardano-db-sync: rewards for
stake-deregistered credentials are kept rather than deleted, and
governance deposit refunds are not written (only enacted treasury
withdrawals reach `pot_reward`).

## `pool`

Stake-pool lifecycle. Depends on `core`; pool owners and reward
addresses resolve as core-owned stake addresses.

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
matching upstream's schema for ledger-disabled runs.

## `scripts_datums`

Plutus and native-script witness data. Depends on `core`.

- `script` — one row per script witness, deduped on script hash.
- `datum` — one row per datum, deduped on hash.
- `redeemer` — one row per redeemer in the transaction witness set.
- `redeemer_data` — the redeemer's datum payload, deduped on hash.
- `extra_key_witness` — required-signer key hashes.

Deduping `script`, `datum`, and `redeemer_data` on their hash means a
payload referenced by many transactions yields a single row.

Two `redeemer` columns are declared but currently always NULL:
`redeemer.fee` (needs the per-block execution-unit prices) and
`redeemer.script_hash` (needs the redeemer pointer resolved against
the tx body's inputs, certs, withdrawals, votes, and proposals).

## `governance`

Conway-era governance. Depends on `core`. Owns sixteen tables in three
groups:

- **Certificate-driven** — `drep_hash`, `drep_registration`,
  `delegation_vote`, `committee_hash`, `committee_registration`,
  `committee_de_registration`.
- **Proposal-driven** — `voting_anchor`, `gov_action_proposal`,
  `param_proposal`, `voting_procedure`, `treasury_withdrawal`,
  `constitution`, `committee`, `committee_member`.
- **Ledger-derived** (epoch boundary) — `drep_distr` (DRep voting
  power per epoch) and `event_info`.

A `(tx_hash, proposal_index) → gov_action_proposal.id` cache carries
proposal ids across blocks so later votes and boundary status updates
can resolve their `GovActionId` references.

`event_info` is declared but not yet populated — it exists as an FK
target only, and `voting_procedure.invalid` (which would reference it)
is always NULL for now.

## `cbor`

Stores the raw CBOR-encoded transaction bytes in `tx_cbor`. Useful
for downstream consumers that need to re-serialise or replay txs.

:::warning Big single contributor
`tx_cbor` accounts for ~218 GB on its own at mainnet tip — roughly
half of an `everything`-profile database. Both
`everything-no-ledger.json` and `everything.json`
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

## `pool_stats`

Per-epoch pool distribution. Owns `pool_stat` — one row per `(pool,
epoch)` written from the post-epoch ledger state at each boundary
crossing. Like `epoch_boundary`, `pdProcess` is a no-op and the work
happens in a boundary hook, so the table stays empty when ledger is
off.

`pool_stat.voting_power` is written as 0 for now; its derivation isn't
wired yet.

## `epoch_sync_stats`

Per-epoch sync performance metrics. The table is `epoch_sync_stats`;
`pdProcess` is a no-op (the consumer writes rows directly at the
epoch boundary commit). Defining it as an extractor is the lightest
way to get the schema created.

## `off_chain_pools`

Pool off-chain metadata. The per-block pass watches pool registrations
that carry a metadata URL and enqueues a fetch; the [OffChain pool
worker](../workers) does the HTTP work and writes the result rows, so
a slow or failing endpoint never blocks ingest.

- `off_chain_pool_data` — successfully fetched and validated metadata.
- `off_chain_pool_fetch_error` — one row per failed attempt.
- `delisted_pool`, `reserved_pool_ticker` — SMASH-style moderation
  lists.

## `off_chain_votes`

Governance off-chain metadata — the voting analogue of
`off_chain_pools`. The per-block pass watches the anchors carried by
proposals, votes, DRep registrations, committee resignations, and
constitution updates, and enqueues a fetch; the [OffChain vote
worker](../workers) writes the result rows.

Owns the seven `off_chain_vote_*` tables: `off_chain_vote_data` plus
its `gov_action_data`, `drep_data`, `author`, `reference`,
`external_update`, and `fetch_error` companions.

## `current_state`

Reserved. The option key is accepted in a profile so a future release
can wire it up without a configuration-schema change, but today it
maps to a stub `ExtractorDef` with no tables and a no-op process
function. When implemented it will own the per-pool / per-account
snapshot tables that upstream cardano-db-sync produces.
