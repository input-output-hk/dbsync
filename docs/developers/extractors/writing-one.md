---
id: writing-one
title: Writing a new extractor
sidebar_position: 2
---

# Writing a new extractor

End-to-end walkthrough. Adding a new extractor means landing changes
in three packages — schema in `dbsync-db`, processing in `dbsync`,
tests in `tests` — but the seam between them is narrow.

Read [Extractor anatomy](anatomy) first if you haven't.

## The shape of the work

```mermaid
flowchart LR
    Schema["dbsync-db<br/>row types + TableDef<br/>+ COPY encoder<br/>+ hasql statements"]
    Extractor["dbsync<br/>Extractor.X<br/>(pure function)"]
    Wire["dbsync<br/>App/Setup.hs<br/>(register)"]
    Tests["tests/<br/>unit + e2e"]

    Schema --> Extractor
    Extractor --> Wire
    Wire --> Tests
```

Schema first, then the extractor body, then registration, then tests.
Nothing in the phase code, the resolver, or the writer needs to
change for a new extractor: the `Writer` and `IdResolver` typeclasses
gain `write*` / `assign*` methods, and the existing Ingest and Follow
implementations grow to cover them.

## 1. Define the row types

Pick a domain module under `dbsync-db/src/DbSync/Db/Schema/`. For an
example, say you're adding a `claim` extractor that records on-chain
reward-claim events. Create `DbSync.Db.Schema.Claim`:

```haskell
data Claim = Claim
  { claimTxId       :: !TxId
  , claimSlotNo     :: !Word64
  , claimAddress    :: !ByteString
  , claimAmount     :: !DbLovelace
  }
  deriving stock (Eq, Show)
```

Plus a `claimTableDef :: TableDef` declaring columns, primary key,
table mode (`TableUnlogged` for an extractor-owned table), and any
unique constraints that should be indexed at Prep time. See
[Schema layer](../schema-layer) for the vocabulary.

Add a `ClaimId` newtype to `DbSync.Db.Schema.Ids` and the table to
the `dbsync_sync_state` per-table counter list (every table backed
by a per-table sequence needs a counter column).

## 2. Add the COPY encoder

In the same domain module, write `encodeClaimCopy :: ClaimId -> Claim -> ByteString`
that produces a single COPY-text row using the `Builder`-based
helpers in
[`DbSync.Db.Loader.Encoder`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Loader/Encoder.hs):

```haskell
encodeClaimCopy :: ClaimId -> Claim -> ByteString
encodeClaimCopy cId c = buildCopyRow
  [ Just (bInt64 (getClaimId cId))
  , Just (bInt64 (getTxId (claimTxId c)))
  , Just (bWord64 (claimSlotNo c))
  , Just (bHex (claimAddress c))
  , Just (bInt64 (fromIntegral (unDbLovelace (claimAmount c))))
  ]
```

`Nothing` produces a SQL `NULL`; the helpers do the COPY escaping.

## 3. Add a hasql statement for Follow

In `dbsync-db/src/DbSync/Db/Statement/Claim.hs`, add the per-row
insert and any lookups Follow needs:

```haskell
insertClaimRowStmt :: Stmt.Statement (ClaimId, Claim) ()
insertClaimRowStmt = ...
```

The Follow writer batches inserts through a hasql `Pipeline`, so the
statement only needs to be preparable. No per-call connection gymnastics.
`Statement/` groups by domain, not by table — see
[`DbSync.Db.Statement.Core`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Core.hs)
for a representative shape.

If the extractor needs ID allocation in Follow (most do), add
`nextClaimIdStmt` using the
[`nextIdStmt`](https://github.com/input-output-hk/dbsync/blob/main/dbsync-db/src/DbSync/Db/Statement/Common.hs)
helper. The Follow `IdAllocator` will pre-allocate batches of these
at the start of every block.

## 4. Extend the Resolver and Writer

The `IdResolver` ([`DbSync.Resolver`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Resolver.hs))
and `Writer` ([`DbSync.Writer`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Writer.hs))
records gain one field each:

```haskell
-- Resolver
, assignClaimId :: !(m ClaimId)

-- Writer
, writeClaim :: !(ClaimId -> Claim -> m ())
```

Then update the four implementations:

- `Phase.Ingest.Resolver` — pull from the in-process counter.
- `Phase.Following.Resolver` — pull from the pre-allocated buffer.
- `Phase.Ingest.Writer.Claim` — new module; encodes via
  `encodeClaimCopy` and pushes to the `LoaderStream`.
- `Phase.Following.Writer.Claim` — new module; appends to the
  per-block `WriteBuffer` for flush via the hasql Pipeline.

The phase writer modules mirror each other directory-for-directory;
look at [`Phase.Ingest.Writer.Core`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Ingest/Writer/Core.hs)
and [`Phase.Following.Writer.Core`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Following/Writer/Core.hs)
together for the pattern.

Also extend `IdCounts` ([`DbSync.Phase.Following.IdCounts`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Phase/Following/IdCounts.hs))
with a counter for `claim` so the Follow loop knows how many IDs to
allocate up front from `claim_id_seq`.

## 5. Write the extractor module

Create `DbSync.Extractor.Claim`:

```haskell
module DbSync.Extractor.Claim
  ( claimExtractor
  ) where

claimExtractor :: ExtractorDef
claimExtractor = ExtractorDef
  { pdName    = "claim"
  , pdTables  = [claimTableDef]
  , pdProcess = processClaim
  }

processClaim :: ProcessBlockFn
processClaim ctx = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  forM_ (bcTxs ctx) $ \tc -> do
    let txId = tcTxId tc
        gtx  = tcGenTx tc
    -- ... walk certs / withdrawals / outputs, build Claim rows,
    -- assign IDs via resolver, write via writer.
    forM_ (claimsIn gtx) $ \c -> do
      cId <- liftIO $ assignClaimId resolver
      liftIO $ writeClaim writer cId c
```

Keep the body small. Anything more complex than a few `forM_`s is
usually a sign that the pure
`GenericBlock → [Claim]` function should live in its own
helper and be tested separately.

## 6. Register the extractor

First add `claimExtractor` to `allKnownExtractors` in
[`DbSync.Extractor.Registry`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/Extractor/Registry.hs).
That registry is the single source of truth for which names resolve to
a real extractor — anything not in it falls back to a no-op stub — and
it also feeds the schema fingerprint.

Then add the `extractors` key to `optionalExtractors` in
[`DbSync.App.Setup.buildExtractors`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Setup.hs):

```haskell
optionalExtractors =
  [ ...
  , ("claim", prEnabled (pcClaim pc))
  ]
```

Finally add `pcClaim :: !OptionFlag` to `Extractors` in
[`DbSync.App.Config.Types`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Config/Types.hs)
with a disabled default, and — if the extractor depends on another —
a rule in
[`DbSync.App.Config.Validation`](https://github.com/input-output-hk/dbsync/blob/main/dbsync/src/DbSync/App/Config/Validation.hs).
Configs opt in by setting `"claim": true` in `extractors`.

If your extractor warrants structured config (like `utxo` does with
its `consumed_by_tx_id` / `strategy` knobs), use the `UtxoOption`
pattern: a dedicated record with `FromJSON`, and a structured
default.

## 7. Tests

A unit test for the pure function is usually enough:

```haskell
-- tests/main/unit/DbSync/Extractor/ClaimSpec.hs
spec :: Spec
spec = describe "Extractor.Claim" $ do
  it "extracts claims from a withdrawal-bearing tx" $ do
    let block = mkBlockWithWithdrawals [...]
    written <- runPureExtract claimExtractor block
    written `shouldBe` ...
```

`runPureExtract` and `runPureExtractMany` come from
[`DbSync.Test.Property.Invariants`](https://github.com/input-output-hk/dbsync/blob/main/tests/lib/DbSync/Test/Property/Invariants.hs).
They wrap
[`mkTestPipelineEnv`](https://github.com/input-output-hk/dbsync/blob/main/tests/lib/DbSync/Test/PipelineEnv.hs),
which builds an in-memory env — an in-process `IdResolver` plus a
collecting `Writer` — so the test exercises the extractor body with no PG
and no mock chain.

For end-to-end coverage, add the new extractor to an existing e2e
spec or create one in `tests/main/e2e/DbSync/Phase/`. The
[`MockChain`](https://github.com/input-output-hk/dbsync/blob/main/tests/lib/DbSync/Test/MockChain.hs)
harness forges Conway-era blocks through a real ledger so derived
state (deposits, rewards) is correct.

See [Testing](../testing) for the per-tier conventions.

## What you don't have to touch

- The phase consumers (`Phase/Ingest/Consumer.hs`,
  `Phase/Following/Run.hs`). They iterate the extractor list; adding
  one is all they need.
- The boot logic. `decideBoot` doesn't care which extractors are
  enabled.
- The receiver, the parser, or the LSM session. They're all
  extractor-agnostic.

The same applies if you're disabling an extractor: drop it from the
config and the table disappears from `initSchema`'s output. No code
changes required.
