{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Core extractor.
--
-- Verifies that 'coreExtractor' correctly transforms 'GenericBlock' values
-- into typed Block, Tx, and SlotLeader records via the 'IdResolver' + 'Writer'
-- pipeline. All tests use hand-crafted 'GenericBlock' fixtures and a test
-- writer — no node or DB needed.
module DbSync.Extractor.CoreSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.IORef (newIORef, readIORef)

import qualified Data.ByteString as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  )
import DbSync.Db.Schema.Core (Block (..))
import qualified DbSync.Db.Schema.Core as SC
import DbSync.Db.Schema.Ids (BlockId (..), SlotLeaderId (..), TxId (..))
import DbSync.Extractor (ExtractState (..), ExtractorDef (..))
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Id.Counter (IdCounters (..), mkIdCounter)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Id.DedupMap (emptyMaps)
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.Writer.Testing (TestWriterState (..), emptyTestWriterState, mkTestWriter)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

spec :: Spec
spec = do
  describe "coreExtractor definition" $ do
    it "has name 'core'" $
      pdName coreExtractor `shouldBe` "core"

    it "has version 1" $
      pdVersion coreExtractor `shouldBe` 1

    it "has no dependencies" $
      pdDependencies coreExtractor `shouldBe` []

    it "owns three tables: block, tx, slot_leader" $
      length (pdTables coreExtractor) `shouldBe` 3

  describe "extraction: empty block (0 txs)" $ do
    it "produces 1 block row and 0 tx rows" $ do
      written <- runCore emptyBlock
      length (twBlocks written) `shouldBe` 1
      length (twTxs written) `shouldBe` 0

    it "produces 1 slot_leader row on first encounter" $ do
      written <- runCore emptyBlock
      length (twSlotLeaders written) `shouldBe` 1

  describe "extraction: block with 3 transactions" $ do
    it "produces 1 block row and 3 tx rows" $ do
      written <- runCore blockWith3Txs
      length (twBlocks written) `shouldBe` 1
      length (twTxs written) `shouldBe` 3

  describe "extraction: two blocks with same slot leader" $ do
    it "only produces slot_leader row on first block" $ do
      (w1, w2) <- runCoreTwoBlocks emptyBlock emptyBlock2
      length (twSlotLeaders w1) `shouldBe` 1
      length (twSlotLeaders w2) `shouldBe` 0

  describe "extraction: two blocks with different slot leaders" $ do
    it "produces a slot_leader row for each" $ do
      let differentLeader = emptyBlock2 { blkSlotLeader = BS.replicate 28 0xff }
      (w1, w2) <- runCoreTwoBlocks emptyBlock differentLeader
      length (twSlotLeaders w1) `shouldBe` 1
      length (twSlotLeaders w2) `shouldBe` 1

  describe "extraction: ID monotonicity" $ do
    it "block IDs increase across sequential blocks" $ do
      (w1, w2) <- runCoreTwoBlocks emptyBlock emptyBlock2
      fst (headDef (panic "no block") (twBlocks w1)) `shouldBe` BlockId 1
      fst (headDef (panic "no block") (twBlocks w2)) `shouldBe` BlockId 2

    it "tx IDs increase across blocks" $ do
      let block2 = blockWith3Txs { blkHash = BS.replicate 32 2, blkBlockNo = BlockNo 2 }
      (w1, w2) <- runCoreTwoBlocks blockWith3Txs block2
      map fst (twTxs w1) `shouldBe` [TxId 1, TxId 2, TxId 3]
      map fst (twTxs w2) `shouldBe` [TxId 4, TxId 5, TxId 6]

  describe "extraction: block row correctness" $ do
    it "tx_count matches number of transactions" $ do
      written <- runCore blockWith3Txs
      let (_, blk) = headDef (panic "no block") (twBlocks written)
      blockTxCount blk `shouldBe` 3

    it "previous_id is Nothing for first block" $ do
      written <- runCore emptyBlock
      let (_, blk) = headDef (panic "no block") (twBlocks written)
      blockPreviousId blk `shouldBe` Nothing

    it "previous_id is set for second block" $ do
      (_, w2) <- runCoreTwoBlocks emptyBlock emptyBlock2
      let (_, blk) = headDef (panic "no block") (twBlocks w2)
      blockPreviousId blk `shouldBe` Just (BlockId 1)

  describe "extraction: tx row correctness" $ do
    it "block_id in tx rows matches the block's ID" $ do
      written <- runCore blockWith3Txs
      let blockIds = map (SC.txBlockId . snd) (twTxs written)
      blockIds `shouldBe` [BlockId 1, BlockId 1, BlockId 1]

    it "block_index is sequential within the block" $ do
      written <- runCore blockWith3Txs
      let idxs = map (SC.txBlockIndex . snd) (twTxs written)
      idxs `shouldBe` [0, 1, 2]

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- | Run the core extractor on a single block, return written records.
runCore :: GenericBlock -> IO TestWriterState
runCore block = do
  stRef <- newIORef mkInitState
  wrRef <- newIORef emptyTestWriterState
  let resolver = mkIngestResolver stRef
      writer   = mkTestWriter wrRef
  processBlock resolver writer [coreExtractor] block
  readIORef wrRef

-- | Run the core extractor on two blocks sequentially, return separate results.
runCoreTwoBlocks :: GenericBlock -> GenericBlock -> IO (TestWriterState, TestWriterState)
runCoreTwoBlocks block1 block2 = do
  stRef <- newIORef mkInitState
  wrRef1 <- newIORef emptyTestWriterState
  let resolver = mkIngestResolver stRef
      writer1  = mkTestWriter wrRef1
  processBlock resolver writer1 [coreExtractor] block1
  w1 <- readIORef wrRef1
  wrRef2 <- newIORef emptyTestWriterState
  let writer2 = mkTestWriter wrRef2
  processBlock resolver writer2 [coreExtractor] block2
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

mkTx :: Word64 -> ByteString -> GenericTx
mkTx idx txH = GenericTx
  { txHash              = txH <> BS.replicate (32 - BS.length txH) 0
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
