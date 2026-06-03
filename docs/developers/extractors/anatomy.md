---
id: anatomy
title: Extractor anatomy
sidebar_position: 1
---

# Extractor anatomy

An extractor is the unit of modular extraction. Each one owns a set of
PostgreSQL tables, declares its dependencies, and defines a single
per-block processing function that runs in both
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
  { pdName         :: !Text
  , pdVersion      :: !Int
  , pdDependencies :: ![(Text, Int)]
  , pdTables       :: ![TableDef]
  , pdProcess      :: ProcessBlockFn
  }

type ProcessBlockFn =
  forall env m.
  ( HasResolver env, HasWriter env, HasNetwork env
  , MonadReader env m, MonadIO m
  )
  => BlockContext -> m ()
```

Five fields, no surprises. `pdTables` is the canonical schema for
this extractor's tables; `pdProcess` is the per-block body.

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
each enabled extractor in topological order.

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

## Tables, dependencies, versions

`pdTables` is a list of `TableDef`s the extractor owns. The schema
layer ([`dbsync-db`](../schema-layer)) generates `CREATE TABLE` DDL
from each one at boot. Tables an extractor doesn't claim never get
created — disabling an extractor at the profile level skips both the
work and the schema.

`pdDependencies` is a list of `(name, minVersion)` pairs. The Pool
extractor declares `[("core", 1), ("stake_delegation", 1)]` because
its rows FK into `stake_address` via pool reward addresses and
owners. The validator in
[`DbSync.App.Setup.validateExtractorDeps`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Setup.hs)
rejects a profile that enables Pool without StakeDelegation with an
operator-readable message.

`pdVersion` bumps when an extractor's table shapes change in a
breaking way. Lower-than-required versions in the dependency check
abort boot with the version mismatch in the message.

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
by name. The `core` extractor is unconditional; the rest are
resolved from a `(name, enabled?)` table built from the profile's
`db_options`. Unknown names get a no-op stub — useful when a
projection's schema is in place but its body hasn't landed yet.

`buildExtractors` also validates dependencies and topologically sorts
the result so producers run before consumers. The order is stable for
a given profile so logs and per-extractor traces stay readable.

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
