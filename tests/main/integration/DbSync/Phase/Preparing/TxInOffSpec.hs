{-# LANGUAGE OverloadedStrings #-}

-- | The post-load pass with @utxo.tx_in = false@.
--
-- Mirrors 'DbSync.Phase.Preparing.RunSpec' on the same fixture chain,
-- but with the tx_in writer gated off and @consumed_by_tx_id@ filled
-- through the worker's bulk UPDATE (the production path — the tx_in
-- residual cannot run). The fee/deposit assertions must produce the
-- same values as the tx_in-based backfills, via the consumed-by
-- alternates.
module DbSync.Phase.Preparing.TxInOffSpec (spec) where

import Cardano.Prelude

import Data.IORef (newIORef)
import qualified Data.Text as T

import Test.Hspec (Spec, afterAll_, beforeAll_, describe, it, shouldBe)

import DbSync.App.Config.Types
  ( Extractors (..)
  , SyncConfig (..)
  , UtxoOption (..)
  )
import DbSync.App.Env (TracerWithConn (..))
import DbSync.App.Setup (applyUtxoWriterOptions)
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
import DbSync.Db.Schema.Ids (getTxId, getTxOutId)
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
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , referenceTxInTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Db.Statement.Worker.ConsumedBy (bulkUpdateConsumedByTxIdStmt)
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
import DbSync.Test.Database (queryTestDb, testConnBs, testConnStr, testHasqlSettings)
import DbSync.Test.Fixtures (byronBlock, producerBlock, spendingBlock, withdrawalBlock)
import DbSync.Test.Hasql (runStatement, withTestConnection)
import DbSync.Test.Lsm (withTestIngestStores)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import DbSync.Trace.Backend (mkNullTracer)
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)
import qualified DbSync.Worker.TxOut.ConsumedByBuffer as ConsumedByBuffer

-- Same table set as 'Phase.Preparing.RunSpec' so the fixtures flow
-- through the real COPY pipeline.
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

-- | 'defaultTestConfig' with @utxo.tx_in = false@.
txInOffConfig :: SyncConfig
txInOffConfig =
  defaultTestConfig
    { scExtractors = ex { exUtxo = (exUtxo ex) { uoTxIn = False } } }
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
        writer   = applyUtxoWriterOptions (exUtxo (scExtractors txInOffConfig))
                     (IngestWriter.mkWriter bs)
        env      = mkTestPipelineEnv resolver writer extractors
    for_ [producerBlock, spendingBlock, byronBlock, withdrawalBlock] $ \blk ->
      runReaderT (processBlock blk) env
    lsCommit bs
    closeLoaderStream bs

    -- Mirror the TxOutWorker's consumed-by arm: with tx_in off, the
    -- bulk UPDATE is the only source of consumed_by_tx_id.
    cb <- ConsumedByBuffer.takeAndReset consumedBuf
    let producers = map getTxOutId (toList (ConsumedByBuffer.ecbProducerTxOutIds cb))
        consumers = map getTxId    (toList (ConsumedByBuffer.ecbConsumerTxIds cb))
    withTestConnection $ \conn ->
      runStatement conn (producers, consumers) bulkUpdateConsumedByTxIdStmt

  withTestConnection $ \conn ->
    runAppM (TracerWithConn mkNullTracer conn txInOffConfig)
      (Prep.run testHasqlSettings defaultPrepTuning tables)

tearDown :: IO ()
tearDown = dropSchema tables testConnStr

spec :: Spec
spec = describe "DbSync.Phase.Preparing.Run (utxo.tx_in = false)" $
  beforeAll_ setUp $
  afterAll_  tearDown $ do

    it "writes no tx_in rows" $ do
      result <- T.strip <$> queryTestDb
        ("SELECT count(*) FROM " <> tdName txInTableDef)
      result `shouldBe` "0"

    it "consumed_by_tx_id matches the tx_in-based run" $ do
      result <- T.strip <$> queryTestDb
        ("SELECT id, consumed_by_tx_id FROM " <> tdName txOutTableDef <> " ORDER BY id")
      T.lines result `shouldBe`
        [ "1|2", "2|3", "3|4", "4|5", "5|", "6|", "7|", "8|" ]

    -- Same expected values as 'RunSpec': the consumed-by alternates
    -- must be equivalent to the tx_in-based backfills.
    it "phase-2 failed tx.fee is folded-input value minus out_sum" $ do
      result <- T.strip <$> queryTestDb
        ("SELECT fee FROM " <> tdName txTableDef <> " WHERE valid_contract = FALSE")
      result `shouldBe` "3000000"

    it "Byron tx.fee is computed as inputs - outputs" $ do
      result <- T.strip <$> queryTestDb
        ("SELECT fee FROM " <> tdName txTableDef <> " WHERE block_id = 3")
      result `shouldBe` "500000"

    it "valid-contract tx.deposit is inputs - outputs - fee - donation" $ do
      result <- T.strip <$> queryTestDb
        ("SELECT deposit FROM " <> tdName txTableDef
           <> " WHERE block_id = 2 AND block_index = 0")
      result `shouldBe` "300000"

    it "drops the consumed-by scaffolding index before the flip" $ do
      result <- T.strip <$> queryTestDb
        "SELECT count(*) FROM pg_indexes WHERE indexname = 'tx_out_consumed_by_scaffold_idx'"
      result `shouldBe` "0"
