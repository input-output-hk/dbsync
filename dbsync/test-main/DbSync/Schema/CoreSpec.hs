{-# LANGUAGE OverloadedStrings #-}

-- | Tests for COPY encoding of Block, Tx, and SlotLeader rows.
--
-- These are pure tests — no PostgreSQL needed. They verify that the
-- encoding functions produce correct tab-separated, newline-terminated
-- COPY text format matching the PostgreSQL COPY FROM STDIN protocol.
module DbSync.Schema.CoreSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Core
  ( Block (..)
  , SlotLeader (..)
  , Tx (..)
  , encodeBlockCopy
  , encodeBool
  , encodeHex
  , encodeInt64
  , encodeSlotLeaderCopy
  , encodeTxCopy
  , encodeUTCTime
  , encodeWord64
  )
import DbSync.Db.Schema.Ids
  ( BlockId (..)
  , SlotLeaderId (..)
  , TxId (..)
  )
import DbSync.Db.Types (DbLovelace (..), DbWord64 (..))

spec :: Spec
spec = do
  describe "Encoding helpers" $ do
    it "encodeInt64 produces decimal ASCII" $ do
      encodeInt64 0 `shouldBe` "0"
      encodeInt64 42 `shouldBe` "42"
      encodeInt64 (-1) `shouldBe` "-1"
      encodeInt64 9223372036854775807 `shouldBe` "9223372036854775807"

    it "encodeWord64 produces unsigned decimal ASCII" $ do
      encodeWord64 0 `shouldBe` "0"
      encodeWord64 18446744073709551615 `shouldBe` "18446744073709551615"

    it "encodeBool produces t/f" $ do
      encodeBool True `shouldBe` "t"
      encodeBool False `shouldBe` "f"

    it "encodeHex produces \\x-prefixed hex" $ do
      encodeHex "" `shouldBe` "\\x"
      encodeHex "\x00\xff" `shouldBe` "\\x00ff"
      encodeHex "\xde\xad\xbe\xef" `shouldBe` "\\xdeadbeef"

    it "encodeUTCTime produces PostgreSQL timestamp format" $ do
      let t = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)
      encodeUTCTime t `shouldBe` "2024-01-15 12:00:00"

  describe "encodeBlockCopy" $ do
    it "produces correct tab-separated COPY row" $ do
      let row = encodeBlockCopy (BlockId 1) sampleBlock
      -- Must end with newline
      BS8.last row `shouldBe` '\n'
      -- Must have exactly 16 fields (16 columns) separated by 15 tabs
      let tabCount = BS.count (fromIntegral (fromEnum '\t')) row
      tabCount `shouldBe` 15

    it "encodes NULL fields as \\N" $ do
      let blk = sampleBlock { blockVrfKey = Nothing, blockOpCert = Nothing, blockOpCertCounter = Nothing }
          row = encodeBlockCopy (BlockId 1) blk
      -- The last three fields should be \N
      row `shouldSatisfy` BS.isSuffixOf "\\N\t\\N\t\\N\n"

    it "encodes all non-NULL fields correctly for a known block" $ do
      let row = encodeBlockCopy (BlockId 1) sampleBlock
          fields = BS8.split '\t' (BS8.init row)  -- drop trailing \n, split on tabs
      -- field 0: id
      fields !! 0 `shouldBe` "1"
      -- field 1: hash (hex encoded — backslash is escaped in COPY format)
      fields !! 1 `shouldBe` "\\\\x" <> BS8.replicate 64 '0'  -- 32 zero bytes = 64 hex chars
      -- field 7: slot_leader_id
      fields !! 7 `shouldBe` "10"
      -- field 9: time
      fields !! 9 `shouldBe` "2024-01-15 12:00:00"
      -- field 10: tx_count
      fields !! 10 `shouldBe` "3"

    it "encodes previous_id as NULL for genesis block" $ do
      let blk = sampleBlock { blockPreviousId = Nothing }
          row = encodeBlockCopy (BlockId 1) blk
          fields = BS8.split '\t' (BS8.init row)
      fields !! 6 `shouldBe` "\\N"

    it "encodes previous_id with value for non-genesis block" $ do
      let blk = sampleBlock { blockPreviousId = Just (BlockId 99) }
          row = encodeBlockCopy (BlockId 100) blk
          fields = BS8.split '\t' (BS8.init row)
      fields !! 6 `shouldBe` "99"

  describe "encodeTxCopy" $ do
    it "produces correct tab-separated COPY row" $ do
      let row = encodeTxCopy (TxId 1) sampleTx
      BS8.last row `shouldBe` '\n'
      let tabCount = BS.count (fromIntegral (fromEnum '\t')) row
      tabCount `shouldBe` 12  -- 13 columns = 12 tabs

    it "encodes all fields correctly for a known tx" $ do
      let row = encodeTxCopy (TxId 42) sampleTx
          fields = BS8.split '\t' (BS8.init row)
      -- field 0: id
      fields !! 0 `shouldBe` "42"
      -- field 2: block_id
      fields !! 2 `shouldBe` "1"
      -- field 3: block_index
      fields !! 3 `shouldBe` "0"
      -- field 4: out_sum (lovelace)
      fields !! 4 `shouldBe` "5000000"
      -- field 5: fee (lovelace)
      fields !! 5 `shouldBe` "174000"
      -- field 6: deposit (NULL)
      fields !! 6 `shouldBe` "\\N"
      -- field 10: valid_contract
      fields !! 10 `shouldBe` "t"
      -- field 12: treasury_donation
      fields !! 12 `shouldBe` "0"

    it "encodes invalid_before and invalid_hereafter when present" $ do
      let tx = sampleTx
            { txInvalidBefore = Just (DbWord64 100)
            , txInvalidHereafter = Just (DbWord64 500)
            }
          row = encodeTxCopy (TxId 1) tx
          fields = BS8.split '\t' (BS8.init row)
      fields !! 8 `shouldBe` "100"
      fields !! 9 `shouldBe` "500"

  describe "encodeSlotLeaderCopy" $ do
    it "produces correct tab-separated COPY row" $ do
      let row = encodeSlotLeaderCopy (SlotLeaderId 1) sampleSlotLeader
      BS8.last row `shouldBe` '\n'
      let tabCount = BS.count (fromIntegral (fromEnum '\t')) row
      tabCount `shouldBe` 3  -- 4 columns = 3 tabs

    it "encodes pool_hash_id as NULL when not a pool" $ do
      let row = encodeSlotLeaderCopy (SlotLeaderId 1) sampleSlotLeader
          fields = BS8.split '\t' (BS8.init row)
      -- field 2: pool_hash_id
      fields !! 2 `shouldBe` "\\N"

    it "encodes all fields correctly" $ do
      let row = encodeSlotLeaderCopy (SlotLeaderId 7) sampleSlotLeader
          fields = BS8.split '\t' (BS8.init row)
      fields !! 0 `shouldBe` "7"
      -- field 1: hash (28 bytes = 56 hex chars, + "\\x" prefix = 59 bytes after COPY escaping)
      BS.length (fields !! 1) `shouldBe` 59  -- "\\\\x" (3) + 56 hex chars
      fields !! 3 `shouldBe` "ShelleyGenesis-deadbeef"

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

sampleBlock :: Block
sampleBlock = Block
  { blockHash         = BS.replicate 32 0      -- 32 zero bytes
  , blockEpochNo      = Just 500
  , blockSlotNo       = Just 123456
  , blockEpochSlotNo  = Just 456
  , blockBlockNo      = Just 100
  , blockPreviousId   = Just (BlockId 0)
  , blockSlotLeaderId = SlotLeaderId 10
  , blockSize         = 2048
  , blockTime         = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)
  , blockTxCount      = 3
  , blockProtoMajor   = 9
  , blockProtoMinor   = 0
  , blockVrfKey       = Just "vrf_vk1abc123"
  , blockOpCert       = Just (BS.replicate 32 0)
  , blockOpCertCounter = Just 42
  }

sampleTx :: Tx
sampleTx = Tx
  { txHash             = BS.replicate 32 1    -- 32 bytes of 0x01
  , txBlockId          = BlockId 1
  , txBlockIndex       = 0
  , txOutSum           = DbLovelace 5000000
  , txFee              = DbLovelace 174000
  , txDeposit          = Nothing
  , txSize             = 300
  , txInvalidBefore    = Nothing
  , txInvalidHereafter = Nothing
  , txValidContract    = True
  , txScriptSize       = 0
  , txTreasuryDonation = DbLovelace 0
  }

sampleSlotLeader :: SlotLeader
sampleSlotLeader = SlotLeader
  { slotLeaderHash        = BS.replicate 28 0xde  -- 28 bytes of 0xDE
  , slotLeaderPoolHashId  = Nothing
  , slotLeaderDescription = "ShelleyGenesis-deadbeef"
  }
