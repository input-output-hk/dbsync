{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the block processing pipeline.
--
-- Verifies that 'processBlock' correctly composes multiple extractors,
-- threading state and merging 'RowBatches'. Uses the real 'coreExtractor'
-- plus mock extractors to test composition behaviour.
module DbSync.Ingest.PipelineSpec (spec) where

import Cardano.Prelude hiding (Map)

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.List ((!!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  )
import DbSync.Extractor
  ( ExtractState (..)
  , ExtractorDef (..)
  , RowBatches (..)
  )
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Id.Counter (IdCounters (..), mkIdCounter)
import DbSync.Id.DedupMap (emptyMaps)
import DbSync.Ingest.Pipeline (processBlock)

spec :: Spec
spec = describe "DbSync.Ingest.Pipeline" $ do
  let initState = mkInitState

  describe "processBlock with coreExtractor" $ do
    it "produces block, tx, and slot_leader rows" $ do
      let (batches, _st') = processBlock [coreExtractor] blockWith2Txs initState
          rows = unRowBatches batches
      countRows "block" rows `shouldBe` 1
      countRows "tx" rows `shouldBe` 2
      countRows "slot_leader" rows `shouldBe` 1

    it "gives same results as calling coreExtractor directly" $ do
      let (batchesPipeline, st1) = processBlock [coreExtractor] blockWith2Txs initState
          (batchesDirect, st2) = pdExtract coreExtractor blockWith2Txs initState
      unRowBatches batchesPipeline `shouldBe` unRowBatches batchesDirect
      esLastBlockId st1 `shouldBe` esLastBlockId st2

  describe "processBlock with multiple extractors" $ do
    it "merges batches from all extractors" $ do
      let mockExtractor = mkMockExtractor "mock_table" "mock_row_data\n"
          (batches, _st') = processBlock [coreExtractor, mockExtractor] blockWith2Txs initState
          rows = unRowBatches batches
      -- Core produces block, tx, slot_leader
      countRows "block" rows `shouldBe` 1
      countRows "tx" rows `shouldBe` 2
      -- Mock produces mock_table
      countRows "mock_table" rows `shouldBe` 1

    it "threads state through extractors sequentially" $ do
      -- The second extractor should see the state from the first.
      -- Use a mock that reads esLastBlockId (set by coreExtractor).
      let stateReader = mkStateReadingExtractor "state_check"
          (batches, _st') = processBlock [coreExtractor, stateReader] emptyBlock initState
          rows = unRowBatches batches
      -- stateReader writes the esLastBlockId it sees into a row.
      -- After coreExtractor runs, esLastBlockId should be Just 1.
      let checkRows = fromMaybe [] $ Map.lookup "state_check" rows
      length checkRows `shouldBe` 1
      -- The row content should be "1" (the block ID set by coreExtractor)
      BS8.unpack (headDef "" checkRows) `shouldBe` "1"

  describe "processBlock with empty extractor list" $ do
    it "produces empty batches" $ do
      let (batches, _st') = processBlock [] emptyBlock initState
      unRowBatches batches `shouldBe` Map.empty

    it "returns state unchanged" $ do
      let (_batches, st') = processBlock [] emptyBlock initState
      st' `shouldBe` initState

  describe "processBlock: sequential blocks maintain state" $ do
    it "block IDs continue across calls" $ do
      let block1 = emptyBlock
          block2 = emptyBlock { blkHash = BS.replicate 32 1, blkBlockNo = BlockNo 2 }
          (batches1, st1) = processBlock [coreExtractor] block1 initState
          (batches2, _st2) = processBlock [coreExtractor] block2 st1
          bid1 = firstFieldOf "block" (unRowBatches batches1)
          bid2 = firstFieldOf "block" (unRowBatches batches2)
      bid1 `shouldBe` "1"
      bid2 `shouldBe` "2"

    it "previous_id links blocks correctly" $ do
      let block1 = emptyBlock
          block2 = emptyBlock { blkHash = BS.replicate 32 1, blkBlockNo = BlockNo 2 }
          (_batches1, st1) = processBlock [coreExtractor] block1 initState
          (batches2, _st2) = processBlock [coreExtractor] block2 st1
          row = getFirstRow "block" (unRowBatches batches2)
          fields = BS8.split '\t' (BS8.init row)
      -- field 6 is previous_id, should reference block 1's ID
      fields !! 6 `shouldBe` "1"

    it "tx IDs continue across blocks" $ do
      let block1 = blockWith2Txs
          block2 = blockWith2Txs
                     { blkHash = BS.replicate 32 2
                     , blkBlockNo = BlockNo 2
                     }
          (batches1, st1) = processBlock [coreExtractor] block1 initState
          (batches2, _st2) = processBlock [coreExtractor] block2 st1
          txIds1 = allFirstFields "tx" (unRowBatches batches1)
          txIds2 = allFirstFields "tx" (unRowBatches batches2)
      txIds1 `shouldBe` ["1", "2"]
      txIds2 `shouldBe` ["3", "4"]

    it "slot leader dedup persists across blocks" $ do
      -- Both blocks have the same slot leader
      let block1 = emptyBlock
          block2 = emptyBlock { blkHash = BS.replicate 32 1, blkBlockNo = BlockNo 2 }
          (batches1, st1) = processBlock [coreExtractor] block1 initState
          (batches2, _st2) = processBlock [coreExtractor] block2 st1
      countRows "slot_leader" (unRowBatches batches1) `shouldBe` 1
      countRows "slot_leader" (unRowBatches batches2) `shouldBe` 0

-- ---------------------------------------------------------------------------
-- Mock extractors for testing composition
-- ---------------------------------------------------------------------------

-- | A mock extractor that writes a fixed row to a named table.
-- Does not modify state — purely additive.
mkMockExtractor :: Text -> ByteString -> ExtractorDef
mkMockExtractor tableName rowData = ExtractorDef
  { pdName         = "mock_" <> tableName
  , pdVersion      = 1
  , pdDependencies = []
  , pdTables       = []
  , pdExtract      = \_ st ->
      (RowBatches $ Map.singleton tableName [rowData], st)
  }

-- | A mock extractor that reads esLastBlockId from the state and
-- writes it as a row. Used to verify state threading.
mkStateReadingExtractor :: Text -> ExtractorDef
mkStateReadingExtractor tableName = ExtractorDef
  { pdName         = "state_reader"
  , pdVersion      = 1
  , pdDependencies = []
  , pdTables       = []
  , pdExtract      = \_ st ->
      let val = case esLastBlockId st of
            Nothing -> "nothing"
            Just n  -> BS8.pack (show n)
      in (RowBatches $ Map.singleton tableName [val], st)
  }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

countRows :: Text -> Map Text [ByteString] -> Int
countRows table rows = maybe 0 length (Map.lookup table rows)

getFirstRow :: Text -> Map Text [ByteString] -> ByteString
getFirstRow table rows = case Map.lookup table rows of
  Just (r:_) -> r
  _          -> panic $ "No rows for table: " <> table

getRows :: Text -> Map Text [ByteString] -> [ByteString]
getRows table rows = fromMaybe [] (Map.lookup table rows)

firstFieldOf :: Text -> Map Text [ByteString] -> ByteString
firstFieldOf table rows =
  let row = getFirstRow table rows
      fields = BS8.split '\t' (BS8.init row)
  in safeHead "" fields

allFirstFields :: Text -> Map Text [ByteString] -> [ByteString]
allFirstFields table rows =
  map (\row -> safeHead "" (BS8.split '\t' (BS8.init row))) (getRows table rows)

safeHead :: a -> [a] -> a
safeHead d []    = d
safeHead _ (x:_) = x

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
