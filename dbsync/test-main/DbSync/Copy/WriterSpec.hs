{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the multi-threaded COPY writer.
--
-- Tests the full pipeline: extractor → IdResolver → CopyAdapter → CopyWriter
-- → real PostgreSQL. No hardcoded COPY strings — all rows are produced by
-- the real extractors and COPY encoders, ensuring type-safe round-trips.
--
-- Requires a running PostgreSQL instance with a @dbsync_test@ database.
module DbSync.Copy.WriterSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Test.Hspec (Spec, afterAll_, beforeAll_, describe, it, shouldBe)

import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  )
import DbSync.Copy.Writer (CopyWriter (..), closeCopyWriter, mkCopyWriter)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Extractor (ExtractState (..), ExtractorDef (..))
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Id.Counter (IdCounters (..), mkIdCounter)
import DbSync.Id.DedupMap (emptyMaps)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.Test.Database (queryTestDb, testConnBs, testConnStr, truncateAllTables)
import DbSync.Writer.CopyAdapter (mkCopyWriterAdapter)

coreTables :: [TableDef]
coreTables = [blockTableDef, txTableDef, slotLeaderTableDef]

coreVersions :: [(Text, Int)]
coreVersions = [("core", 1)]

coreTableNames :: [Text]
coreTableNames = ["tx", "block", "slot_leader"]

spec :: Spec
spec = describe "DbSync.Copy.Writer (multi-threaded, full pipeline)" $
  beforeAll_ setupTestSchema $
  afterAll_  teardownTestSchema $ do

    describe "single block through full pipeline" $ do
      it "writes 1 block, 0 txs, 1 slot_leader to PostgreSQL" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock]
        blockCount <- queryTestDb "SELECT count(*) FROM block;"
        txCount    <- queryTestDb "SELECT count(*) FROM tx;"
        slCount    <- queryTestDb "SELECT count(*) FROM slot_leader;"
        T.strip blockCount `shouldBe` "1"
        T.strip txCount    `shouldBe` "0"
        T.strip slCount    `shouldBe` "1"

      it "writes block with 3 transactions" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [blockWith3Txs]
        blockCount <- queryTestDb "SELECT count(*) FROM block;"
        txCount    <- queryTestDb "SELECT count(*) FROM tx;"
        T.strip blockCount `shouldBe` "1"
        T.strip txCount    `shouldBe` "3"

    describe "multiple blocks" $ do
      it "writes 2 blocks with correct IDs" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock, emptyBlock2]
        result <- queryTestDb "SELECT id FROM block ORDER BY id;"
        T.strip result `shouldBe` "1\n2"

      it "links previous_id correctly" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock, emptyBlock2]
        result <- queryTestDb
          "SELECT id, previous_id FROM block ORDER BY id;"
        let rows = T.lines (T.strip result)
        rows `shouldBe` ["1|", "2|1"]

      it "tx IDs are monotonic across blocks" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [blockWith3Txs, blockWith2Txs]
        result <- queryTestDb "SELECT id FROM tx ORDER BY id;"
        let txIds = T.lines (T.strip result)
        txIds `shouldBe` ["1", "2", "3", "4", "5"]

      it "tx block_id references the correct block" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [blockWith3Txs, blockWith2Txs]
        result <- queryTestDb "SELECT block_id FROM tx ORDER BY id;"
        let blockRefs = T.lines (T.strip result)
        -- First 3 txs belong to block 1, next 2 to block 2
        blockRefs `shouldBe` ["1", "1", "1", "2", "2"]

    describe "slot leader dedup" $ do
      it "same leader across 2 blocks produces 1 slot_leader row" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock, emptyBlock2]
        slCount <- queryTestDb "SELECT count(*) FROM slot_leader;"
        T.strip slCount `shouldBe` "1"

      it "different leaders produce separate rows" $ do
        truncateAllTables coreTableNames
        let diffLeader = emptyBlock2 { blkSlotLeader = BS.replicate 28 0xff }
        runPipelineToDb [emptyBlock, diffLeader]
        slCount <- queryTestDb "SELECT count(*) FROM slot_leader;"
        T.strip slCount `shouldBe` "2"

    describe "epoch boundary (commit + reopen)" $ do
      it "data survives commit + reopen cycle" $ do
        truncateAllTables coreTableNames
        -- Simulate 2 epochs: write block, commit, reopen, write another block, commit
        stRef <- newIORef mkInitState
        cw <- mkCopyWriter testConnBs coreTables
        let resolver = mkIngestResolver stRef
            writer   = mkCopyWriterAdapter cw

        -- Epoch 1
        processBlock resolver writer [coreExtractor] emptyBlock
        cwCommit cw
        -- Epoch 2
        cwReopen cw
        processBlock resolver writer [coreExtractor] emptyBlock2
        cwCommit cw
        closeCopyWriter cw

        blockCount <- queryTestDb "SELECT count(*) FROM block;"
        T.strip blockCount `shouldBe` "2"

    describe "data integrity" $ do
      it "NULL values round-trip correctly" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock]
        result <- queryTestDb
          "SELECT previous_id FROM block WHERE id = 1;"
        T.strip result `shouldBe` ""  -- NULL

      it "bytea hash round-trips correctly" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock]
        result <- queryTestDb
          "SELECT encode(hash, 'hex') FROM slot_leader WHERE id = 1;"
        -- blkSlotLeader is BS.replicate 28 0xab = 28 bytes = 56 hex chars
        T.strip result `shouldBe` T.replicate 28 "ab"

-- ---------------------------------------------------------------------------
-- Test runner helpers
-- ---------------------------------------------------------------------------

-- | Run the full pipeline: extractor → resolver → CopyAdapter → PostgreSQL.
runPipelineToDb :: [GenericBlock] -> IO ()
runPipelineToDb blocks = do
  stRef <- newIORef mkInitState
  cw <- mkCopyWriter testConnBs coreTables
  let resolver = mkIngestResolver stRef
      writer   = mkCopyWriterAdapter cw
  forM_ blocks $ \blk ->
    processBlock resolver writer [coreExtractor] blk
  cwCommit cw
  closeCopyWriter cw

-- ---------------------------------------------------------------------------
-- Initial state
-- ---------------------------------------------------------------------------

mkInitState :: ExtractState
mkInitState = ExtractState
  { esIdCounters = IdCounters
      { icBlockId        = mkIdCounter 1
      , icTxId           = mkIdCounter 1
      , icTxOutId        = mkIdCounter 1
      , icSlotLeaderId   = mkIdCounter 1
      , icStakeAddressId = mkIdCounter 1
      , icPoolHashId     = mkIdCounter 1
      , icMultiAssetId   = mkIdCounter 1
      , icScriptId       = mkIdCounter 1
      }
  , esDedupMaps   = emptyMaps
  , esLastBlockId = Nothing
  }

-- ---------------------------------------------------------------------------
-- Setup/teardown
-- ---------------------------------------------------------------------------

setupTestSchema :: IO ()
setupTestSchema = do
  dropSchema coreTables coreVersions testConnStr
  initSchema coreTables coreVersions testConnStr

teardownTestSchema :: IO ()
teardownTestSchema = dropSchema coreTables coreVersions testConnStr

-- ---------------------------------------------------------------------------
-- Test fixtures (same as CoreSpec/PipelineSpec)
-- ---------------------------------------------------------------------------

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

emptyBlock :: GenericBlock
emptyBlock = GenericBlock
  { blkEra          = Shelley
  , blkHash         = BS.replicate 32 0
  , blkPreviousHash = ""
  , blkSlotNo       = SlotNo 100
  , blkBlockNo      = BlockNo 1
  , blkEpochNo      = EpochNo 5
  , blkEpochSlotNo  = 100
  , blkSize         = 512
  , blkTime         = sampleTime
  , blkSlotLeader   = BS.replicate 28 0xab
  , blkProtoMajor   = 9
  , blkProtoMinor   = 0
  , blkVrfKey       = Just "vrf_vk1test"
  , blkOpCert       = Just (BS.replicate 32 0)
  , blkOpCertCounter = Just 0
  , blkTxs          = []
  }

emptyBlock2 :: GenericBlock
emptyBlock2 = emptyBlock
  { blkHash    = BS.replicate 32 1
  , blkBlockNo = BlockNo 2
  , blkSlotNo  = SlotNo 120
  }

blockWith3Txs :: GenericBlock
blockWith3Txs = emptyBlock
  { blkTxs = [mkTx 0 "tx0hash", mkTx 1 "tx1hash", mkTx 2 "tx2hash"]
  }

blockWith2Txs :: GenericBlock
blockWith2Txs = emptyBlock
  { blkHash    = BS.replicate 32 2
  , blkBlockNo = BlockNo 2
  , blkSlotNo  = SlotNo 120
  , blkTxs     = [mkTx 0 "tx3hash", mkTx 1 "tx4hash"]
  }

mkTx :: Word64 -> ByteString -> GenericTx
mkTx idx txH = GenericTx
  { txHash              = txH <> BS.replicate (max 0 (32 - BS.length txH)) 0
  , txBlockIndex        = idx
  , txSize              = 300
  , txFee               = 174000
  , txOutSum            = 5000000
  , txValidContract     = True
  , txScriptSize        = 0
  , txTreasuryDonation  = 0
  , txInvalidBefore     = Nothing
  , txInvalidHereafter  = Nothing
  , txInputs            = []
  , txOutputs           = []
  , txCollateralInputs  = []
  , txReferenceInputs   = []
  , txCollateralOutput  = Nothing
  , txCertificates      = []
  , txWithdrawals       = []
  , txMetadata          = Nothing
  , txMint              = []
  }
