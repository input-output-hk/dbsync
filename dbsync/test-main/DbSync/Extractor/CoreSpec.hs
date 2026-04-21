{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Core extractor.
--
-- Verifies that 'coreExtractor' correctly transforms 'GenericBlock' values
-- into COPY-encoded rows for the block, tx, and slot_leader tables.
-- All tests use hand-crafted 'GenericBlock' fixtures — no node or DB needed.
module DbSync.Extractor.CoreSpec (spec) where

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

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Block.Types
  ( BlockEra (..)
  , GenericBlock (..)
  , GenericTx (..)
  )
import DbSync.Extractor (ExtractFn, ExtractState (..), ExtractorDef (..), RowBatches (..))
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Id.Counter (IdCounters (..), mkIdCounter)
import DbSync.Id.DedupMap (emptyMaps)

spec :: Spec
spec = do
  let extract = pdExtract coreExtractor
      initState = mkInitState

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
      let (batches, _st') = extract emptyBlock initState
          rows = unRowBatches batches
      countRows "block" rows `shouldBe` 1
      countRows "tx" rows `shouldBe` 0

    it "produces 1 slot_leader row on first encounter" $ do
      let (batches, _st') = extract emptyBlock initState
      countRows "slot_leader" (unRowBatches batches) `shouldBe` 1

  describe "extraction: block with 3 transactions" $ do
    it "produces 1 block row and 3 tx rows" $ do
      let (batches, _st') = extract blockWith3Txs initState
          rows = unRowBatches batches
      countRows "block" rows `shouldBe` 1
      countRows "tx" rows `shouldBe` 3

  describe "extraction: two blocks with same slot leader" $ do
    it "only produces slot_leader row on first block" $ do
      let (batches1, st1) = extract emptyBlock initState
          (batches2, _st2) = extract emptyBlock2 st1
      countRows "slot_leader" (unRowBatches batches1) `shouldBe` 1
      countRows "slot_leader" (unRowBatches batches2) `shouldBe` 0

  describe "extraction: two blocks with different slot leaders" $ do
    it "produces a slot_leader row for each" $ do
      let (batches1, st1) = extract emptyBlock initState
          differentLeader = emptyBlock2 { blkSlotLeader = BS.replicate 28 0xff }
          (batches2, _st2) = extract differentLeader st1
      countRows "slot_leader" (unRowBatches batches1) `shouldBe` 1
      countRows "slot_leader" (unRowBatches batches2) `shouldBe` 1

  describe "extraction: ID monotonicity" $ do
    it "block IDs increase across sequential blocks" $ do
      let (batches1, st1) = extract emptyBlock initState
          (batches2, _st2) = extract emptyBlock2 st1
          bid1 = firstFieldOf "block" (unRowBatches batches1)
          bid2 = firstFieldOf "block" (unRowBatches batches2)
      bid1 `shouldBe` "1"
      bid2 `shouldBe` "2"

    it "tx IDs increase across blocks" $ do
      let block1 = blockWith3Txs
          block2 = blockWith3Txs { blkHash = BS.replicate 32 2, blkBlockNo = BlockNo 2 }
          (batches1, st1) = extract block1 initState
          (batches2, _st2) = extract block2 st1
          txIds1 = allFirstFields "tx" (unRowBatches batches1)
          txIds2 = allFirstFields "tx" (unRowBatches batches2)
      txIds1 `shouldBe` ["1", "2", "3"]
      txIds2 `shouldBe` ["4", "5", "6"]

  describe "extraction: block row correctness" $ do
    it "tx_count matches number of transactions" $ do
      let (batches, _st) = extract blockWith3Txs initState
          row = getFirstRow "block" (unRowBatches batches)
          fields = BS8.split '\t' (BS8.init row)
      -- field 10 is tx_count
      fields !! 10 `shouldBe` "3"

    it "previous_id is NULL for first block" $ do
      let (batches, _st) = extract emptyBlock initState
          row = getFirstRow "block" (unRowBatches batches)
          fields = BS8.split '\t' (BS8.init row)
      -- field 6 is previous_id
      fields !! 6 `shouldBe` "\\N"

    it "previous_id is set for second block" $ do
      let (batches1, st1) = extract emptyBlock initState
          (batches2, _st2) = extract emptyBlock2 st1
          row = getFirstRow "block" (unRowBatches batches2)
          fields = BS8.split '\t' (BS8.init row)
      -- field 6 should be the first block's ID
      fields !! 6 `shouldBe` "1"

  describe "extraction: tx row correctness" $ do
    it "block_id in tx rows matches the block's ID" $ do
      let (batches, _st) = extract blockWith3Txs initState
          txRows = getRows "tx" (unRowBatches batches)
          blockIds = map (\row -> BS8.split '\t' (BS8.init row) !! 2) txRows
      -- All txs should reference block_id=1
      blockIds `shouldBe` ["1", "1", "1"]

    it "block_index is sequential within the block" $ do
      let (batches, _st) = extract blockWith3Txs initState
          txRows = getRows "tx" (unRowBatches batches)
          blockIdxs = map (\row -> BS8.split '\t' (BS8.init row) !! 3) txRows
      blockIdxs `shouldBe` ["0", "1", "2"]

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

-- | Extract the first tab-delimited field (the ID) from the first row of a table.
firstFieldOf :: Text -> Map Text [ByteString] -> ByteString
firstFieldOf table rows =
  let row = getFirstRow table rows
      fields = BS8.split '\t' (BS8.init row)
  in safeHead "" fields

-- | Extract the first field from ALL rows of a table.
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

-- | A second empty block with a different hash/blockno but SAME slot leader.
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
