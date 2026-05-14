{-# LANGUAGE OverloadedStrings #-}

-- | Query-plan assertions for the four post-load backfill UPDATEs.
--
-- 'PreparingForChainTipSpec' already covers the /correctness/ of
-- each backfill: it drives a 3-block fixture through the real
-- pipeline and asserts on the @tx.fee@ / @tx.deposit@ values that
-- come out the other side. What it cannot catch is a query-plan
-- regression — the SQL produces the correct numbers on a tiny
-- fixture even when the plan is pathological enough to take hours
-- on a real chain.
--
-- This spec stops mid-'PreparingForChainTip.run' (after the
-- pre-resolve indexes, the FK resolves and a fresh @ANALYZE@, but
-- before the backfill UPDATEs themselves) and runs @EXPLAIN@
-- against each backfill statement. The assertions check that the
-- plan references the new pre-resolve indexes, not the full-table
-- aggregate-then-filter shape that previously hung at the rollback
-- boundary.
--
-- Substring matching on plain-text @EXPLAIN@ output is chosen over
-- structured JSON walking on purpose: substring assertions read
-- like a checklist of the index lookups the plan must use, and they
-- stay stable across PG point releases that occasionally rename
-- internal node types.
module DbSync.Db.Statement.BackfillSpec (spec) where

import Cardano.Prelude

import Data.IORef (newIORef)

import qualified Data.Text as T

import Test.Hspec
  ( Spec
  , afterAll_
  , beforeAll_
  , describe
  , it
  , shouldNotSatisfy
  , shouldSatisfy
  )

import DbSync.Copy.Writer (CopyWriter (..), closeCopyWriter, mkCopyWriter)
import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.CBOR (txCborTableDef)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init (analyzeSql, dropSchema, initSchema)
import DbSync.Db.Schema.Metadata (txMetadataTableDef)
import DbSync.Db.Schema.MultiAsset
  ( maTxMintTableDef
  , maTxOutTableDef
  , multiAssetTableDef
  )
import DbSync.Db.Schema.Pool
  ( poolHashTableDef
  , poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRelayTableDef
  , poolRetireTableDef
  , poolUpdateTableDef
  )
import DbSync.Db.Schema.StakeDelegation
  ( delegationTableDef
  , stakeAddressTableDef
  , stakeDeregistrationTableDef
  , stakeRegistrationTableDef
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Db.Statement.Backfill
  ( backfillByronFeeSql
  , backfillPhaseTwoDepositSql
  , backfillPhaseTwoFeeSql
  , backfillValidContractDepositSql
  )
import DbSync.Extractor (ExtractorDef, freshExtractState)
import DbSync.Extractor.Cbor (cborExtractor)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Metadata (metadataExtractor)
import DbSync.Extractor.MultiAsset (multiAssetExtractor)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.UTxO (utxoExtractor)
import DbSync.Id.DedupMap (newMaps)
import DbSync.Ingest.Pipeline (processBlock)
import qualified DbSync.Phase.PreparingForChainTip.PreResolveIndexes as PreResolveIndexes
import qualified DbSync.Phase.PreparingForChainTip.Resolve as Resolve
import DbSync.Resolver.AddressBuffer (newAddressBufferRef)
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.Test.Database
  ( execTestDb
  , queryTestDb
  , testConnBs
  , testConnStr
  )
import DbSync.Test.Fixtures (byronBlock, producerBlock, spendingBlock)
import DbSync.Test.Hasql (withTestConnection)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Trace.Backend (mkNullTracer)
import DbSync.Writer.CopyAdapter (mkCopyWriterAdapter)

-- ---------------------------------------------------------------------------
-- * Schema setup
-- ---------------------------------------------------------------------------

-- | Same table set as 'PreparingForChainTipSpec' so the fixtures
-- flow through the real COPY pipeline. The backfill spec only
-- needs a subset for assertions but the COPY writer expects every
-- target table to exist.
tables :: [TableDef]
tables =
  [ blockTableDef
  , txTableDef
  , slotLeaderTableDef
  , addressTableDef
  , txOutTableDef
  , txInTableDef
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txMetadataTableDef
  , multiAssetTableDef
  , maTxMintTableDef
  , maTxOutTableDef
  , stakeAddressTableDef
  , stakeRegistrationTableDef
  , stakeDeregistrationTableDef
  , delegationTableDef
  , withdrawalTableDef
  , poolHashTableDef
  , poolUpdateTableDef
  , poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRetireTableDef
  , poolRelayTableDef
  , txCborTableDef
  ]

versions :: [(Text, Int)]
versions =
  [ ("core", 1)
  , ("utxo", 1)
  , ("metadata", 1)
  , ("multi_asset", 1)
  , ("stake_delegation", 1)
  , ("pool", 1)
  , ("cbor", 1)
  ]

extractors :: [ExtractorDef]
extractors =
  [ coreExtractor
  , utxoExtractor
  , metadataExtractor
  , multiAssetExtractor
  , stakeDelegationExtractor
  , poolExtractor
  , cborExtractor
  ]

-- ---------------------------------------------------------------------------
-- * Setup / teardown
-- ---------------------------------------------------------------------------

-- | Tables the post-resolve ANALYZE in 'Phase.PreparingForChainTip.run'
-- refreshes. Duplicated here verbatim because exporting the binding
-- from the production module just for a test would push a non-test
-- name into the public surface.
analyzeTables :: [TableDef]
analyzeTables =
  [ blockTableDef
  , txTableDef
  , txInTableDef
  , txOutTableDef
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , withdrawalTableDef
  ]

-- | Drive the fixture blocks through the real COPY pipeline, then
-- prepare the DB up to but /not including/ the backfill UPDATEs.
-- The plan-shape assertions run against this state.
setUp :: IO ()
setUp = do
  dropSchema tables versions testConnStr
  initSchema tables versions testConnStr

  stRef <- newIORef freshExtractState
  dedupMaps <- newMaps
  addrBuf <- newAddressBufferRef
  cw <- mkCopyWriter testConnBs tables
  let env = mkTestPipelineEnv (mkIngestResolver stRef dedupMaps addrBuf)
                              (mkCopyWriterAdapter cw) extractors
  for_ [producerBlock, spendingBlock, byronBlock] $ \blk ->
    runReaderT (processBlock blk) env
  cwCommit cw
  closeCopyWriter cw

  withTestConnection $ \conn -> do
    PreResolveIndexes.createPreResolveIndexes mkNullTracer conn
    _ <- Resolve.resolveForeignKeys mkNullTracer conn
    pure ()

  -- Run ANALYZE the same way 'Phase.PreparingForChainTip.run'
  -- does — needed for the planner to pick non-trivial plans even
  -- on this tiny fixture.
  for_ analyzeTables $ \td -> execTestDb (analyzeSql (tdName td))

tearDown :: IO ()
tearDown = dropSchema tables [] testConnStr

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

-- | Capture @EXPLAIN (VERBOSE)@ output as a single text blob.
--
-- 'enable_seqscan = off' forces PG to use any index that matches the
-- query, regardless of size-based cost estimates. On the tiny test
-- fixture a seq scan over five rows is genuinely cheaper than an
-- index lookup, so the planner would pick seq scan even when the
-- right index exists. Disabling seqscan turns the assertion into
-- "the index is usable for this query shape", which is the
-- regression we want to catch — without it, a missing pre-resolve
-- index would silently take down a production sync while every
-- spec stayed green on a four-row fixture.
explainOf :: Text -> IO Text
explainOf sql = queryTestDb
  $ "SET enable_seqscan = off; EXPLAIN (VERBOSE) " <> sql

-- | Substring presence assertion that reads naturally at the call
-- site. Lifted out so the assertion lines stay short.
shouldMention :: HasCallStack => Text -> Text -> IO ()
shouldMention plan fragment =
  plan `shouldSatisfy` (fragment `T.isInfixOf`)

shouldNotMention :: HasCallStack => Text -> Text -> IO ()
shouldNotMention plan fragment =
  plan `shouldNotSatisfy` (fragment `T.isInfixOf`)

spec :: Spec
spec = describe "DbSync.Db.Statement.Backfill" $
  beforeAll_ setUp $
  afterAll_  tearDown $ do

    describe "preResolveIndexStatements drives the plan shape" $
      it "every new pre-resolve index exists in pg_indexes" $ do
        -- The five names that must exist after pre-resolve runs.
        -- The two original (tx-hash + tx_out tx_id/index) were
        -- already checked in 'PreparingForChainTipSpec.indexes';
        -- this case covers the four new perf indexes the rewritten
        -- backfills need.
        count <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE indexname IN ("
            , "  'collateral_tx_in_tx_in_id_idx',"
            , "  'collateral_tx_out_tx_id_idx',"
            , "  'tx_in_tx_in_id_idx',"
            , "  'withdrawal_tx_id_idx'"
            , ")"
            ])
        count `shouldSatisfy` (== "4")

    describe "backfillPhaseTwoFeeStmt plan" $ do
      it "drives off the tx WHERE clause, not the collateral aggregate" $ do
        plan <- explainOf backfillPhaseTwoFeeSql
        -- The rewritten shape filters tx first (selective predicate),
        -- then looks up collateral via index. The old shape was a
        -- HashAggregate over all collateral_tx_in.
        plan `shouldMention` "tx"
        plan `shouldNotMention` "HashAggregate"

      it "looks up collateral inputs via the pre-resolve index" $ do
        plan <- explainOf backfillPhaseTwoFeeSql
        plan `shouldMention` "collateral_tx_in_tx_in_id_idx"

      it "looks up collateral outputs via the pre-resolve index" $ do
        plan <- explainOf backfillPhaseTwoFeeSql
        plan `shouldMention` "collateral_tx_out_tx_id_idx"

    describe "backfillByronFeeStmt plan" $ do
      it "drives off the block.proto_major filter, not the tx_in aggregate" $ do
        plan <- explainOf backfillByronFeeSql
        plan `shouldMention` "proto_major"
        plan `shouldNotMention` "HashAggregate"

      it "looks up Byron tx inputs via tx_in_tx_in_id_idx" $ do
        plan <- explainOf backfillByronFeeSql
        plan `shouldMention` "tx_in_tx_in_id_idx"

    describe "backfillValidContractDepositStmt plan" $ do
      it "still aggregates inputs and withdrawals once (bulk shape)" $ do
        -- The valid-contract deposit fallback is the one place the
        -- aggregate-then-join shape stays — in ledger-disabled mode
        -- every valid tx needs the computation, so a one-pass
        -- aggregate is the right plan. PG may pick HashAggregate or
        -- GroupAggregate (the sorted variant) depending on input
        -- ordering and version; either qualifies. What we want to
        -- catch is a regression into per-row Subquery Scans, which
        -- is what caused the hang on the phase-2 path.
        plan <- explainOf backfillValidContractDepositSql
        plan `shouldSatisfy` \p ->
          "HashAggregate" `T.isInfixOf` p
            || "GroupAggregate" `T.isInfixOf` p
        -- And it must not have the bad shape — a SubPlan probing
        -- the same tables per outer row.
        plan `shouldNotMention` "SubPlan"

    describe "backfillPhaseTwoDepositStmt plan" $
      it "is a flat UPDATE on tx (no joins, no subqueries)" $ do
        -- The simplest of the four — sets deposit = 0 on phase-2
        -- fails. Plan should be a Seq Scan / Index Scan on tx and
        -- nothing else.
        plan <- explainOf backfillPhaseTwoDepositSql
        plan `shouldMention` "tx"
        plan `shouldNotMention` "Subquery"
        plan `shouldNotMention` "HashAggregate"

-- ---------------------------------------------------------------------------
-- Fixtures live in 'DbSync.Test.Fixtures'; shared with
-- 'PreparingForChainTipSpec'.
