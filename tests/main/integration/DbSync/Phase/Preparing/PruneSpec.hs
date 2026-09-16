{-# LANGUAGE OverloadedStrings #-}

-- | The post-load pass with @utxo.strategy = "prune"@.
--
-- Mirrors 'DbSync.Phase.Preparing.RunSpec' on the same fixture chain.
-- Prep fills @consumed_by_tx_id@ from @tx_in@ as usual, then the prune
-- pass deletes every consumed output.
--
-- The fixtures carry no multi-assets and no collateral return, so the
-- spec seeds an @ma_tx_out@ row on a consumed output, one on a live
-- output, and a @collateral_tx_out@ row. The @ma_tx_out@ seed is what
-- puts the delete ordering under test: the FK is created and validated
-- at the end of the same pass, so a surviving orphan fails 'setUp'.
module DbSync.Phase.Preparing.PruneSpec (spec) where

import Cardano.Prelude

import Data.IORef (newIORef)
import qualified Data.Text as T

import Test.Hspec (Spec, afterAll_, beforeAll_, describe, it, shouldBe)

import DbSync.App.Config.Types
  ( Extractors (..)
  , SyncConfig (..)
  , UtxoOption (..)
  , UtxoStrategy (..)
  )
import DbSync.App.Env (TracerWithConn (..))
import DbSync.AppM (runAppM)
import DbSync.Db.Loader (LoaderStream (..), closeLoaderStream, mkLoaderStream)
import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.CBOR (txCborTableDef)
import DbSync.Db.Schema.Core
  ( blockTableDef
  , poolHashTableDef
  , slotLeaderTableDef
  , stakeAddressTableDef
  , txTableDef
  )
import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Db.Schema.Metadata (txMetadataTableDef)
import DbSync.Db.Schema.MultiAsset
  ( maTxMintTableDef
  , maTxOutTableDef
  , multiAssetTableDef
  )
import DbSync.Db.Schema.Pool
  ( poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRelayTableDef
  , poolRetireTableDef
  , poolUpdateTableDef
  )
import DbSync.Db.Schema.StakeDelegation
  ( delegationTableDef
  , stakeDeregistrationTableDef
  , stakeRegistrationTableDef
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Ids (TxId (..))
import DbSync.Db.Schema.Types (TableDef (..))
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
import DbSync.Extractor.Pipeline (processBlock)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.UTxO (utxoExtractor)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import qualified DbSync.Phase.Ingest.Writer as IngestWriter
import qualified DbSync.Phase.Preparing.Run as Prep
import DbSync.Phase.Preparing.Tuning (defaultPrepTuning)
import DbSync.Test.AppHarness (defaultTestConfig)
import DbSync.Test.Database (execTestDb, queryTestDb, testConnBs, testConnStr, testHasqlSettings)
import DbSync.Test.Fixtures (byronBlock, producerBlock, spendingBlock, withdrawalBlock)
import DbSync.Db.Statement.Worker.Prune (queryPruneWatermarkStmt)
import DbSync.Test.Hasql (runStatement, withTestConnection)
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Trace.Backend (mkNullTracer)
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)
import qualified DbSync.Worker.TxOut.ConsumedByBuffer as ConsumedByBuffer

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

-- | 'defaultTestConfig' with @utxo.strategy = "prune"@.
pruneConfig :: SyncConfig
pruneConfig =
  defaultTestConfig
    { scExtractors = ex { exUtxo = (exUtxo ex) { uoStrategy = StrategyPrune } } }
  where
    ex = scExtractors defaultTestConfig

setUp :: IO ()
setUp = do
  dropSchema tables testConnStr
  initSchema tables testConnStr

  withTestIngestStores $ \utxoStore dedupStores -> do
    stRef       <- newIORef freshExtractState
    addrBuf     <- newAddressBufferRef
    consumedBuf <- ConsumedByBuffer.newConsumedByBufferRef
    bs          <- mkLoaderStream testConnBs tables
    let resolver = mkIngestResolver stRef dedupStores addrBuf utxoStore (Just consumedBuf)
        writer   = IngestWriter.mkWriter bs
        env      = mkTestPipelineEnv resolver writer extractors
    for_ [producerBlock, spendingBlock, byronBlock, withdrawalBlock] $ \blk ->
      runReaderT (processBlock blk) env
    lsCommit bs
    closeLoaderStream bs

  -- tx_out 1 is consumed (by tx 2), tx_out 5 is not. The orphan-to-be
  -- asset row must go with its parent, or the FK validation at the end
  -- of the pass rejects it.
  execTestDb $ mconcat
    [ "INSERT INTO ", tdName maTxOutTableDef, " (quantity, tx_out_id, ident)"
    , " VALUES (100, 1, 1), (200, 5, 1);"
    , "INSERT INTO ", tdName collateralTxOutTableDef
    , " (id, tx_id, index, value, multi_assets_descr) VALUES (1, 2, 0, 1000, '');"
    ]

  withTestConnection $ \conn ->
    runAppM (TracerWithConn mkNullTracer conn pruneConfig)
      (Prep.run testHasqlSettings defaultPrepTuning tables)

tearDown :: IO ()
tearDown = dropSchema tables testConnStr

spec :: Spec
spec = describe "DbSync.Phase.Preparing.Run (utxo.strategy = prune)" $
  beforeAll_ setUp $
  afterAll_  tearDown $ do

    it "keeps only the unconsumed outputs" $ do
      result <- T.strip <$> queryTestDb
        ("SELECT id FROM " <> tdName txOutTableDef <> " ORDER BY id")
      T.lines result `shouldBe` ["5", "6", "7", "8"]

    it "deletes assets of pruned outputs and keeps the rest" $ do
      result <- T.strip <$> queryTestDb
        ("SELECT tx_out_id FROM " <> tdName maTxOutTableDef <> " ORDER BY tx_out_id")
      T.lines result `shouldBe` ["5"]

    it "truncates collateral_tx_out" $ do
      result <- T.strip <$> queryTestDb
        ("SELECT count(*) FROM " <> tdName collateralTxOutTableDef)
      result `shouldBe` "0"

    -- Prune runs after the backfills, which read the values of the
    -- outputs it deletes; these are 'RunSpec's values unchanged.
    it "leaves the fee backfills intact" $ do
      phaseTwo <- T.strip <$> queryTestDb
        ("SELECT fee FROM " <> tdName txTableDef <> " WHERE valid_contract = FALSE")
      byron <- T.strip <$> queryTestDb
        ("SELECT fee FROM " <> tdName txTableDef <> " WHERE block_id = 3")
      (phaseTwo, byron) `shouldBe` ("3000000", "500000")

    it "leaves the deposit backfill intact" $ do
      result <- T.strip <$> queryTestDb
        ("SELECT deposit FROM " <> tdName txTableDef
           <> " WHERE block_id = 2 AND block_index = 0")
      result `shouldBe` "300000"

    -- The Follow-phase watermark. Offsetting past the start of the
    -- chain must yield nothing rather than a watermark that prunes
    -- inside the rollback window.
    it "watermarks the whole chain at offset 0" $ do
      result <- withTestConnection $ \conn ->
        runStatement conn 0 queryPruneWatermarkStmt
      result `shouldBe` Just (TxId 5)

    it "has no watermark when the window is longer than the chain" $ do
      result <- withTestConnection $ \conn ->
        runStatement conn (2 * 2160) queryPruneWatermarkStmt
      result `shouldBe` Nothing
