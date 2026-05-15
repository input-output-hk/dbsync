{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests for 'DbSync.Phase.PreparingForChainTip.run'.
--
-- The whole pipeline is driven from a small set of 'GenericBlock'
-- fixtures: a Shelley producer, a Shelley spending block (one
-- valid-contract spender + one phase-2 failure with collateral),
-- and a Byron block whose single tx spends one of the producer's
-- outputs. The fixtures flow through the real extractors and
-- COPY writer so the rows the post-load pass operates on are
-- shaped exactly the way 'IngestChainHistory' would have shaped
-- them — no hand-rolled INSERTs.
--
-- After the pipeline drains, 'Phase.PreparingForChainTip.run'
-- runs against the same database and the spec asserts:
--
--   * @tx_in@ / @collateral_tx_in@ FK columns are populated.
--   * @tx_out.consumed_by_tx_id@ tracks the spending tx_in only
--     (collateral consumption is intentionally not tracked).
--   * @tx.fee@ on the phase-2 failure is collateral-in minus
--     collateral-out.
--   * @tx.fee@ on the Byron tx is inputs minus outputs.
--   * @tx.deposit@ for valid-contract tx is the inputs-minus-
--     outputs fallback; for the phase-2 failure it's @0@.
--   * Tables flipped from UNLOGGED to LOGGED.
--   * Declared unique indexes exist.
--   * @<table>_id_seq@ allocates @MAX(id) + 1@.
--
-- Requires a running PostgreSQL instance with a @dbsync_test@
-- database; runs in the "Database integration" test group.
module DbSync.Phase.PreparingForChainTipSpec (spec) where

import Cardano.Prelude

import Data.IORef (newIORef)

import qualified Data.Text as T

import Test.Hspec
  ( Spec
  , afterAll_
  , beforeAll_
  , describe
  , it
  , shouldBe
  )

import DbSync.Block.Types (GenericBlock)
import DbSync.Copy.Writer (CopyWriter (..), closeCopyWriter, mkCopyWriter)
import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.CBOR (txCborTableDef)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init (dropSchema, initSchema)
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
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
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
import qualified DbSync.Phase.PreparingForChainTip as Prep
import DbSync.Phase.PreparingForChainTip.Tuning (defaultPrepTuning)
import DbSync.Resolver.AddressBuffer (newAddressBufferRef)
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.Test.Database
  ( queryTestDb
  , testConnBs
  , testConnStr
  , testHasqlSettings
  )
import DbSync.Test.Fixtures (byronBlock, producerBlock, spendingBlock)
import DbSync.Test.Hasql (withTestConnection)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Trace.Backend (mkNullTracer)
import DbSync.Writer.CopyAdapter (mkCopyWriterAdapter)

-- ---------------------------------------------------------------------------
-- * Schema setup
-- ---------------------------------------------------------------------------

-- | Every table the active extractors write into. Must include
-- @collateral_tx_out@ (which 'utxoExtractor' writes via the COPY
-- adapter even though its 'pdTables' list omits it) and every
-- table the post-load UPDATEs read from.
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
-- * Pipeline runner
-- ---------------------------------------------------------------------------

-- | Drive the supplied blocks through extractor → resolver → COPY
-- writer → PostgreSQL, then run the post-load pass on the same DB.
runPipelineThenPrepare :: [GenericBlock] -> IO ()
runPipelineThenPrepare blocks = do
  stRef <- newIORef freshExtractState
  dedupMaps <- newMaps
  addrBuf <- newAddressBufferRef
  cw <- mkCopyWriter testConnBs tables
  let env = mkTestPipelineEnv (mkIngestResolver stRef dedupMaps addrBuf)
                              (mkCopyWriterAdapter cw) extractors
  for_ blocks $ \blk -> runReaderT (processBlock blk) env
  cwCommit cw
  closeCopyWriter cw
  withTestConnection $ \conn ->
    Prep.run mkNullTracer conn testHasqlSettings defaultPrepTuning tables

-- ---------------------------------------------------------------------------
-- * Setup / teardown
-- ---------------------------------------------------------------------------

setUp :: IO ()
setUp = do
  dropSchema tables versions testConnStr
  initSchema tables versions testConnStr
  runPipelineThenPrepare [producerBlock, spendingBlock, byronBlock]

tearDown :: IO ()
tearDown = dropSchema tables [] testConnStr

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DbSync.Phase.PreparingForChainTip" $
  beforeAll_ setUp $
  afterAll_  tearDown $ do

    describe "FK resolution" $ do
      it "tx_in.tx_out_id is populated from the producing tx's id" $ do
        result <- T.strip <$> queryTestDb
          "SELECT tx_out_id FROM tx_in WHERE id = 1"
        result `shouldBe` "1"

      it "collateral_tx_in.tx_out_id is populated the same way" $ do
        result <- T.strip <$> queryTestDb
          "SELECT tx_out_id FROM collateral_tx_in WHERE id = 1"
        result `shouldBe` "1"

      it "tx_out.consumed_by_tx_id is set on every spending tx_in" $ do
        result <- T.strip <$> queryTestDb
          "SELECT id, consumed_by_tx_id FROM tx_out ORDER BY id"
        let rows = T.lines result
        rows `shouldBe`
          [ "1|2"  -- producer.0: spent by the Shelley consumer (tx 2)
          , "2|"   -- producer.1: only collateral consumed it; not tracked
          , "3|4"  -- producer.2: spent by the Byron tx (tx 4)
          , "4|"   -- consumer's own output: never spent
          , "5|"   -- byron tx's own output: never spent
          ]

    describe "tx column backfill" $ do
      it "phase-2 failed tx.fee is collateral-in minus collateral-out" $ do
        result <- T.strip <$> queryTestDb
          "SELECT fee FROM tx WHERE valid_contract = FALSE"
        result `shouldBe` "3000000"

      it "phase-2 failed tx.deposit is set to 0" $ do
        result <- T.strip <$> queryTestDb
          "SELECT deposit FROM tx WHERE valid_contract = FALSE"
        result `shouldBe` "0"

      it "valid-contract tx.deposit is inputs - outputs - fee - donation" $ do
        -- 5_000_000 (input) - 4_500_000 (out_sum) - 200_000 (fee)
        --   - 0 (treasury_donation) = 300_000. The producer has no
        --   inputs so its deposit is left NULL by the fallback.
        result <- T.strip <$> queryTestDb
          "SELECT deposit FROM tx WHERE block_id = 2 AND block_index = 0"
        result `shouldBe` "300000"

      it "valid-contract tx.fee is left untouched" $ do
        -- The post-load pass replaces fee only when valid_contract is
        -- FALSE. The consumer's declared fee should be exactly what
        -- the parser wrote.
        result <- T.strip <$> queryTestDb
          "SELECT fee FROM tx WHERE block_id = 2 AND block_index = 0"
        result `shouldBe` "200000"

      it "Byron tx.fee is computed as inputs - outputs" $ do
        -- The Byron tx spends producer.2 (value 2_000_000) and
        -- writes one output of 1_500_000. Expected fee = 500_000.
        result <- T.strip <$> queryTestDb
          "SELECT fee FROM tx WHERE block_id = 3"
        result `shouldBe` "500000"

    describe "schema-mode flip" $
      it "every extractor table is now LOGGED" $ do
        -- pg_class.relpersistence: 'p' = permanent (LOGGED), 'u' = UNLOGGED
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_class"
            , "WHERE relkind = 'r'"
            , "AND relname IN ('block', 'tx', 'tx_out', 'tx_in',"
            , "  'collateral_tx_in', 'collateral_tx_out',"
            , "  'reference_tx_in', 'address', 'slot_leader',"
            , "  'withdrawal')"
            , "AND relpersistence = 'p'"
            ])
        result `shouldBe` "10"

    describe "indexes" $ do
      it "the address.raw unique index is created" $ do
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE tablename = 'address'"
            , "AND indexname = 'address_unique_1_idx'"
            ])
        result `shouldBe` "1"

      -- The next three assertions confirm the pre-resolve index build
      -- runs before the resolves: each index name must exist on disk
      -- after Prep.run returns. The unique tx.hash index is also
      -- emitted by the later concurrent pass under the same name; the
      -- IF NOT EXISTS guard makes the second emission a no-op.
      it "tx (hash) is indexed for the post-load join-on-hash UPDATEs" $ do
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE tablename = 'tx'"
            , "AND indexname = 'tx_unique_1_idx'"
            ])
        result `shouldBe` "1"

      it "tx_out (tx_id, index) is indexed for consumed-by + backfill JOINs" $ do
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE tablename = 'tx_out'"
            , "AND indexname = 'tx_out_tx_id_index_idx'"
            ])
        result `shouldBe` "1"

      it "tx_in (tx_out_id, tx_out_index) is indexed for the merge-join inner" $ do
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE tablename = 'tx_in'"
            , "AND indexname = 'tx_in_tx_out_idx'"
            ])
        result `shouldBe` "1"

      -- The four perf indexes that support the rewritten backfill
      -- UPDATEs. A missing entry here means a backfill regresses to
      -- aggregate-then-filter and the post-load pass hangs at scale.
      it "collateral_tx_in (tx_in_id) is indexed for phase-2 fee lookup" $ do
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE tablename = 'collateral_tx_in'"
            , "AND indexname = 'collateral_tx_in_tx_in_id_idx'"
            ])
        result `shouldBe` "1"

      it "collateral_tx_out (tx_id) is indexed for phase-2 fee lookup" $ do
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE tablename = 'collateral_tx_out'"
            , "AND indexname = 'collateral_tx_out_tx_id_idx'"
            ])
        result `shouldBe` "1"

      it "tx_in (tx_in_id) is indexed for Byron fee + deposit lookup" $ do
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE tablename = 'tx_in'"
            , "AND indexname = 'tx_in_tx_in_id_idx'"
            ])
        result `shouldBe` "1"

      it "withdrawal (tx_id) is indexed for deposit lookup" $ do
        result <- T.strip <$> queryTestDb
          (T.unwords
            [ "SELECT count(*) FROM pg_indexes"
            , "WHERE tablename = 'withdrawal'"
            , "AND indexname = 'withdrawal_tx_id_idx'"
            ])
        result `shouldBe` "1"

    describe "sequence reset" $ do
      it "tx_id_seq's next value is MAX(id) + 1" $ do
        -- Four txs landed: producer, valid-contract spender, phase-2
        -- failure, Byron spender. MAX(id) = 4, next allocation = 5.
        result <- T.strip <$> queryTestDb "SELECT nextval('tx_id_seq')"
        result `shouldBe` "5"

      it "tx_out_id_seq's next value is MAX(id) + 1" $ do
        -- Producer wrote three outputs; consumer wrote one; Byron
        -- wrote one; phase-2 wrote none. MAX(id) = 5, next = 6.
        result <- T.strip <$> queryTestDb "SELECT nextval('tx_out_id_seq')"
        result `shouldBe` "6"

      it "tx_in_id_seq's next value is MAX(id) + 1" $ do
        -- Two tx_in rows: consumer's spend + Byron's spend.
        result <- T.strip <$> queryTestDb "SELECT nextval('tx_in_id_seq')"
        result `shouldBe` "3"

      it "an empty table's sequence still starts at 1" $ do
        -- No reference_tx_in rows were produced; setval(seq, 0+1, false)
        -- leaves nextval at 1.
        result <- T.strip <$>
          queryTestDb "SELECT nextval('reference_tx_in_id_seq')"
        result `shouldBe` "1"

-- ---------------------------------------------------------------------------
-- Fixtures live in 'DbSync.Test.Fixtures'; shared with 'BackfillSpec'.
