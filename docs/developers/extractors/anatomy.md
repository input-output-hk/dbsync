---
id: anatomy
title: Extractor anatomy
sidebar_position: 1
---

# Extractor anatomy

An extractor is the unit of modular extraction. Each one owns a set of
PostgreSQL tables and defines a single per-block processing function
that runs in both
[`IngestChainHistory`](../phases/ingest) and
[`FollowingVolatileTail`](../phases/following-volatile-tail). The same
body works in both phases because the env it consumes is
polymorphic — the phase decides what `IdResolver` and `Writer`
implementations the env carries.

## The contract

The definition lives in
[`DbSync.Extractor`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Extractor.hs):

```haskell
data ExtractorDef = ExtractorDef
  { pdName    :: !Text
  , pdTables  :: ![TableDef]
  , pdProcess :: ProcessBlockFn
  }

type ProcessBlockFn =
  forall env m.
  ( HasResolver env, HasWriter env, HasNetwork env
  , MonadReader env m, MonadIO m
  )
  => BlockContext -> m ()
```

Three fields, no surprises. `pdName` is the profile key that enables
the extractor; `pdTables` is the canonical schema for the tables it
owns; `pdProcess` is the per-block body.

## Why the polymorphism

:::tip This is the load-bearing piece
The rank-N quantification over `env` is what makes one extractor
body work in both Ingest and Follow. The body never picks a phase
— it asks the env for a resolver and a writer and calls methods on
the interface. The phase decides which implementations the env
carries.
:::

In Ingest, the env carries the COPY-backed
[`Phase.Ingest.Resolver`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/Resolver.hs)
and
[`Phase.Ingest.Writer`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/Writer.hs)
implementations. In Follow it carries the hasql-backed
[`Phase.Following.Resolver`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Following/Resolver.hs)
and
[`Phase.Following.Writer`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Following/Writer.hs).
The body never branches on phase — and never can, because it doesn't
have the env type to dispatch on.

The exception is the one place a body genuinely needs to know which
path it's on: `bcSyncPhase` on the context, used by `core` to write
phase-aware fee/deposit values and by `utxo` to decide whether to
fill `address_id` synchronously or queue it for the TxOut worker.

## BlockContext

```haskell
data BlockContext = BlockContext
  { bcBlockId      :: !BlockId
  , bcSlotLeaderId :: !SlotLeaderId
  , bcSlotLeaderNew :: !Bool
  , bcSlotLeaderPoolHashId :: !(Maybe PoolHashId)
  , bcPrevBlockId  :: !(Maybe BlockId)
  , bcGenBlock     :: !GenericBlock
  , bcTxs          :: ![TxContext]
  , bcNetwork      :: !Network
  , bcLedgerData   :: !BlockLedgerData
  , bcSyncPhase    :: !SyncPhase
  }
```

Every shared ID an extractor might need is pre-assigned and packaged
here before the first extractor runs. `BlockId`, `SlotLeaderId`, the
per-tx `TxId` and per-output `TxOutId` are all populated by
[`processBlock`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Extractor/Pipeline.hs)
in a single sweep that walks the block once before dispatching to
extractors.

That's what makes extractors textually independent. None of them have
to look at each other's output to discover the parent row's ID —
those IDs are already known. The dispatcher then iterates through
each enabled extractor in registration order.

## Ledger data

`bcLedgerData` is either:

```haskell
data BlockLedgerData
  = LedgerDataOff
  | LedgerDataOn !LedgerOutputs
```

The two-constructor shape rules out the impossible "ledger off but
populated" state at compile time. Extractors that care about
deposits, protocol-param deposits, or other ledger-derived data
pattern-match this; extractors that don't (`utxo`, `metadata`,
`cbor`, etc.) ignore it entirely.

## Tables and dependencies

`pdTables` is a list of `TableDef`s the extractor owns. The schema
layer ([`dbsync-db`](../schema-layer)) generates `CREATE TABLE` DDL
from each one at boot. Tables an extractor doesn't claim never get
created — disabling an extractor at the profile level skips both the
work and the schema.

Dependencies between extractors aren't declared on `ExtractorDef`.
They're enforced by the config validator in
[`DbSync.App.Config.Validation`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Config/Validation.hs),
which collects every violation in one pass and aborts boot with an
operator-readable message. The rules:

- `multi_asset` requires `utxo` (its rows FK into `tx_out`).
- `off_chain_pools` requires `pool`; `off_chain_votes` requires
  `governance` (each fetches metadata for rows the other writes).
- `stake_delegation_ledger`, `pool_stats`, `epoch_boundary`, and
  `current_state` require `ledger.enabled = true` — their rows are
  derived from ledger state.

Schema versioning is global rather than per-extractor: a fingerprint
over every declared table detects drift at boot. See
[Schema versioning](../schema-versioning).

## Shared dedup helpers

A handful of tables — `pool_hash`, `stake_address`, `multi_asset` —
can be touched by multiple extractors. The first sighting must insert,
subsequent sightings must reuse the same ID. The dedup helpers in
[`DbSync.Extractor.SharedDedup`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Extractor/SharedDedup.hs)
wrap that pattern: look up via the resolver, write the row if it's
new, return the ID.

The same helper works in both phases because the underlying
`resolvePoolHash` / `resolveStakeAddress` / `resolveMultiAsset`
methods on `IdResolver` are phase-implemented (LSM dedup map in
Ingest, `SELECT … WHERE hash = ?` in Follow).

## Registration

Extractors are wired up in
[`DbSync.App.Setup.buildExtractors`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Setup.hs)
by name. The `core` extractor is unconditional and leads the list;
the rest are resolved from a `(name, enabled?)` table built from the
config's `db_profile` against the registry in
[`DbSync.Extractor.Registry`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Extractor/Registry.hs).
A name with no implementation (today only `current_state`) resolves to
a no-op stub, so enabling it is accepted but writes nothing.

The list order is the fixed declaration order in `allKnownExtractors`,
arranged so shared-dedup producers (e.g. `stake_delegation`) precede
their consumers (`pool`). Dependency validation happens separately, in
the config validator.

## Non-block-driven extractors

Two extractors have a no-op `pdProcess`:

- **`epoch_boundary`** owns `ada_pots`, `epoch_param`, `epoch_state`,
  `cost_model`. These are populated by `runEpochBoundary`
  ([`DbSync.Extractor.EpochBoundary`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Extractor/EpochBoundary.hs))
  which the consumer calls at each epoch boundary with the matching
  ledger output. Per-block work is none.
- **`epoch`** owns `epoch_finalized` plus the `epoch` / `epoch_current`
  views. The table is filled by SQL hooks at three points (Ingest
  backfill, Follow boundary, Follow rollback) rather than from any
  per-block path. The extractor exists so the schema gets created
  when the option is on.

:::note
Registering them as extractors keeps schema creation uniform —
there's no second registration path to maintain. See
[Existing extractors](existing) for the catalogue.
:::
