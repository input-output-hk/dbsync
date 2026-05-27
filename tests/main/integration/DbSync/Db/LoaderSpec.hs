{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the multi-threaded loader-stream writer.
--
-- Tests the full pipeline: extractor → IdResolver → Ingest writer →
-- LoaderStream → real PostgreSQL. No hardcoded wire-format strings — all
-- rows are produced by the real extractors and encoders, ensuring
-- type-safe round-trips.
--
-- Requires a running PostgreSQL instance with a @dbsync_test@ database.
module DbSync.Db.LoaderSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef)
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
import DbSync.Db.Loader (LoaderStream (..), closeLoaderStream, mkLoaderStream)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Db.Schema.Pool (poolHashTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Extractor (freshExtractState)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Phase.Ingest.DedupMap (newMaps)
import DbSync.Block.Pipeline (processBlock)
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Test.Lsm (withTestUtxoStore)
import DbSync.Test.Database (queryTestDb, testConnBs, testConnStr, truncateAllTables)
import DbSync.Test.PipelineEnv (mkTestPipelineEnv)
import qualified DbSync.Phase.Ingest.Writer as IngestWriter

-- | The tables this spec exercises. @pool_hash@ is included because
-- the pipeline (not 'coreExtractor' itself) writes a @pool_hash@ row
-- for the slot leader on every Shelley+ block — see
-- 'DbSync.Block.Pipeline.resolveSlotLeaderPoolHash'. Omitting it
-- causes the loader stream to panic on the first non-Byron block.
coreTables :: [TableDef]
coreTables = [blockTableDef, txTableDef, slotLeaderTableDef, poolHashTableDef]

coreVersions :: [(Text, Int)]
coreVersions = [("core", 1)]

coreTableNames :: [Text]
coreTableNames = map tdName coreTables

spec :: Spec
spec = describe "DbSync.Db.Loader (multi-threaded, full pipeline)" $
  beforeAll_ setupTestSchema $
  afterAll_  teardownTestSchema $ do

    describe "single block through full pipeline" $ do
      it "writes 1 block, 0 txs, 1 slot_leader to PostgreSQL" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock]
        blockCount <- queryCount blockTableDef
        txCount    <- queryCount txTableDef
        slCount    <- queryCount slotLeaderTableDef
        blockCount `shouldBe` "1"
        txCount    `shouldBe` "0"
        slCount    `shouldBe` "1"

      it "writes block with 3 transactions" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [blockWith3Txs]
        blockCount <- queryCount blockTableDef
        txCount    <- queryCount txTableDef
        blockCount `shouldBe` "1"
        txCount    `shouldBe` "3"

    describe "multiple blocks" $ do
      it "writes 2 blocks with correct IDs" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock, emptyBlock2]
        result <- queryTestDb $
          "SELECT id FROM " <> tdName blockTableDef <> " ORDER BY id;"
        T.strip result `shouldBe` "1\n2"

      it "links previous_id correctly" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock, emptyBlock2]
        result <- queryTestDb $
          "SELECT id, previous_id FROM " <> tdName blockTableDef <> " ORDER BY id;"
        let rows = T.lines (T.strip result)
        rows `shouldBe` ["1|", "2|1"]

      it "tx IDs are monotonic across blocks" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [blockWith3Txs, blockWith2Txs]
        result <- queryTestDb $
          "SELECT id FROM " <> tdName txTableDef <> " ORDER BY id;"
        let txIds = T.lines (T.strip result)
        txIds `shouldBe` ["1", "2", "3", "4", "5"]

      it "tx block_id references the correct block" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [blockWith3Txs, blockWith2Txs]
        result <- queryTestDb $
          "SELECT block_id FROM " <> tdName txTableDef <> " ORDER BY id;"
        let blockRefs = T.lines (T.strip result)
        -- First 3 txs belong to block 1, next 2 to block 2
        blockRefs `shouldBe` ["1", "1", "1", "2", "2"]

    describe "slot leader dedup" $ do
      it "same leader across 2 blocks produces 1 slot_leader row" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock, emptyBlock2]
        slCount <- queryCount slotLeaderTableDef
        slCount `shouldBe` "1"

      it "different leaders produce separate rows" $ do
        truncateAllTables coreTableNames
        let diffLeader = emptyBlock2 { blkSlotLeader = BS.replicate 28 0xff }
        runPipelineToDb [emptyBlock, diffLeader]
        slCount <- queryCount slotLeaderTableDef
        slCount `shouldBe` "2"

    describe "epoch boundary (commit + reopen)" $ do
      it "data survives commit + reopen cycle" $ withTestUtxoStore $ \utxoStore -> do
        truncateAllTables coreTableNames
        -- Simulate 2 epochs: write block, commit, reopen, write another block, commit
        stRef <- newIORef freshExtractState
        dedupMaps <- newMaps
        addrBuf <- newAddressBufferRef
        bs <- mkLoaderStream testConnBs coreTables
        let env = mkTestPipelineEnv (mkIngestResolver stRef dedupMaps addrBuf utxoStore Nothing)
                                    (IngestWriter.mkWriter bs) [coreExtractor]

        -- Epoch 1
        runReaderT (processBlock emptyBlock) env
        lsCommit bs
        -- Epoch 2
        lsReopen bs
        runReaderT (processBlock emptyBlock2) env
        lsCommit bs
        closeLoaderStream bs

        blockCount <- queryCount blockTableDef
        blockCount `shouldBe` "2"

    describe "data integrity" $ do
      it "NULL values round-trip correctly" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock]
        result <- queryTestDb $
          "SELECT previous_id FROM " <> tdName blockTableDef <> " WHERE id = 1;"
        T.strip result `shouldBe` ""  -- NULL

      it "bytea hash round-trips correctly" $ do
        truncateAllTables coreTableNames
        runPipelineToDb [emptyBlock]
        result <- queryTestDb $
          "SELECT encode(hash, 'hex') FROM " <> tdName slotLeaderTableDef
            <> " WHERE id = 1;"
        -- blkSlotLeader is BS.replicate 28 0xab = 28 bytes = 56 hex chars
        T.strip result `shouldBe` T.replicate 28 "ab"

-- | Strip-and-count helper. Returns the bare count string so callers
-- can compare against the small numeric literals they already use.
queryCount :: TableDef -> IO Text
queryCount td = T.strip <$>
  queryTestDb ("SELECT count(*) FROM " <> tdName td <> ";")

-- ---------------------------------------------------------------------------
-- Test runner helpers
-- ---------------------------------------------------------------------------

-- | Run the full pipeline: extractor → resolver → Ingest writer → PostgreSQL.
runPipelineToDb :: [GenericBlock] -> IO ()
runPipelineToDb blocks = withTestUtxoStore $ \utxoStore -> do
  stRef <- newIORef freshExtractState
  dedupMaps <- newMaps
  addrBuf <- newAddressBufferRef
  bs <- mkLoaderStream testConnBs coreTables
  let env = mkTestPipelineEnv (mkIngestResolver stRef dedupMaps addrBuf utxoStore Nothing)
                              (IngestWriter.mkWriter bs) [coreExtractor]
  forM_ blocks $ \blk ->
    runReaderT (processBlock blk) env
  lsCommit bs
  closeLoaderStream bs

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
  , blkIsEBB        = False
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
  , txCborRaw           = Nothing
  }
