{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the block processing pipeline.
--
-- Verifies that 'processBlock' correctly composes multiple extractors
-- via 'IdResolver' + 'Writer'. Uses the real 'coreExtractor' plus
-- mock extractors to test composition behaviour.
module DbSync.Ingest.PipelineSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  )
import DbSync.Db.Schema.Core (Block (..))
import DbSync.Db.Schema.Ids (BlockId (..), TxId (..))
import DbSync.Extractor (ExtractState (..), ExtractorDef (..))
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Id.Counter (IdCounters (..), mkIdCounter)
import DbSync.Id.DedupMap (emptyMaps)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.Writer (Writer (..))
import DbSync.Writer.Testing (TestWriterState (..), emptyTestWriterState, mkTestWriter)

spec :: Spec
spec = describe "DbSync.Ingest.Pipeline" $ do

  describe "processBlock with coreExtractor" $ do
    it "produces block, tx, and slot_leader rows" $ do
      written <- runPipeline [coreExtractor] blockWith2Txs
      length (twBlocks written) `shouldBe` 1
      length (twTxs written) `shouldBe` 2
      length (twSlotLeaders written) `shouldBe` 1

  describe "processBlock with multiple extractors" $ do
    it "runs all extractors on the block" $ do
      commitRef <- newIORef (0 :: Int)
      let mockExtractor = mkMockExtractor commitRef
      written <- runPipeline [coreExtractor, mockExtractor] emptyBlock
      -- Core produces block + slot_leader; mock does nothing visible in writer
      length (twBlocks written) `shouldBe` 1
      -- Verify mock ran by checking the commit ref it incremented
      mockCount <- readIORef commitRef
      mockCount `shouldBe` 1

  describe "processBlock with empty extractor list" $ do
    it "writes nothing" $ do
      written <- runPipeline [] emptyBlock
      length (twBlocks written) `shouldBe` 0
      length (twTxs written) `shouldBe` 0
      length (twSlotLeaders written) `shouldBe` 0

  describe "processBlock: sequential blocks maintain state" $ do
    it "block IDs continue across calls" $ do
      (w1, w2) <- runPipelineTwoBlocks [coreExtractor] emptyBlock
        (emptyBlock { blkHash = BS.replicate 32 1, blkBlockNo = BlockNo 2 })
      fst (headDef (panic "no block") (twBlocks w1)) `shouldBe` BlockId 1
      fst (headDef (panic "no block") (twBlocks w2)) `shouldBe` BlockId 2

    it "previous_id links blocks correctly" $ do
      (_, w2) <- runPipelineTwoBlocks [coreExtractor] emptyBlock
        (emptyBlock { blkHash = BS.replicate 32 1, blkBlockNo = BlockNo 2 })
      let (_, blk2) = headDef (panic "no block") (twBlocks w2)
      blockPreviousId blk2 `shouldBe` Just (BlockId 1)

    it "tx IDs continue across blocks" $ do
      (w1, w2) <- runPipelineTwoBlocks [coreExtractor] blockWith2Txs
        (blockWith2Txs { blkHash = BS.replicate 32 2, blkBlockNo = BlockNo 2 })
      map fst (twTxs w1) `shouldBe` [TxId 1, TxId 2]
      map fst (twTxs w2) `shouldBe` [TxId 3, TxId 4]

    it "slot leader dedup persists across blocks" $ do
      (w1, w2) <- runPipelineTwoBlocks [coreExtractor] emptyBlock
        (emptyBlock { blkHash = BS.replicate 32 1, blkBlockNo = BlockNo 2 })
      length (twSlotLeaders w1) `shouldBe` 1
      length (twSlotLeaders w2) `shouldBe` 0

-- ---------------------------------------------------------------------------
-- Mock extractors for testing composition
-- ---------------------------------------------------------------------------

-- | A mock extractor that increments a counter when run.
mkMockExtractor :: IORef Int -> ExtractorDef
mkMockExtractor countRef = ExtractorDef
  { pdName         = "mock"
  , pdVersion      = 1
  , pdDependencies = []
  , pdTables       = []
  , pdProcess      = \_ _ _ ->
      atomicModifyIORef' countRef $ \n -> (n + 1, ())
  }

-- ---------------------------------------------------------------------------
-- Test runners
-- ---------------------------------------------------------------------------

runPipeline :: [ExtractorDef] -> GenericBlock -> IO TestWriterState
runPipeline extractors block = do
  stRef <- newIORef mkInitState
  wrRef <- newIORef emptyTestWriterState
  let resolver = mkIngestResolver stRef
      writer   = mkTestWriter wrRef
  processBlock resolver writer extractors block
  readIORef wrRef

runPipelineTwoBlocks :: [ExtractorDef] -> GenericBlock -> GenericBlock -> IO (TestWriterState, TestWriterState)
runPipelineTwoBlocks extractors block1 block2 = do
  stRef <- newIORef mkInitState
  let resolver = mkIngestResolver stRef
  wrRef1 <- newIORef emptyTestWriterState
  processBlock resolver (mkTestWriter wrRef1) extractors block1
  w1 <- readIORef wrRef1
  wrRef2 <- newIORef emptyTestWriterState
  processBlock resolver (mkTestWriter wrRef2) extractors block2
  w2 <- readIORef wrRef2
  pure (w1, w2)

-- ---------------------------------------------------------------------------
-- Initial state
-- ---------------------------------------------------------------------------

mkInitState :: ExtractState
mkInitState = ExtractState
  { esIdCounters = IdCounters
      { icBlockId            = mkIdCounter 1
      , icTxId               = mkIdCounter 1
      , icTxOutId            = mkIdCounter 1
      , icTxInId             = mkIdCounter 1
      , icCollateralTxInId   = mkIdCounter 1
      , icReferenceTxInId    = mkIdCounter 1
      , icTxMetadataId       = mkIdCounter 1
      , icMaTxMintId         = mkIdCounter 1
      , icMaTxOutId          = mkIdCounter 1
      , icSlotLeaderId       = mkIdCounter 1
      , icStakeAddressId     = mkIdCounter 1
      , icPoolHashId         = mkIdCounter 1
      , icMultiAssetId       = mkIdCounter 1
      , icScriptId           = mkIdCounter 1
      }
  , esDedupMaps   = emptyMaps
  , esLastBlockId = Nothing
  }

-- ---------------------------------------------------------------------------
-- Test fixtures
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

blockWith2Txs :: GenericBlock
blockWith2Txs = emptyBlock
  { blkTxs = [mkTx 0 "tx0hash_padding_to_32b_____", mkTx 1 "tx1hash_padding_to_32b_____"]
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
