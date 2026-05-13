{-# LANGUAGE OverloadedStrings #-}

-- | Statement-level tests for 'DbSync.Db.Statement.Block'.
--
-- See 'DbSync.Db.Statement.SyncStateSpec' for the convention this
-- module follows.
module DbSync.Db.Statement.BlockSpec (spec) where

import Cardano.Prelude

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import Test.Hspec
  ( Spec
  , afterAll_
  , beforeAll_
  , before_
  , describe
  , it
  , shouldBe
  )

import DbSync.Db.Schema.Core
  ( Block (..)
  , SlotLeader (..)
  , blockTableDef
  , slotLeaderTableDef
  )
import DbSync.Db.Schema.Ids (BlockId (..), SlotLeaderId (..))
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Db.Statement.Block
  ( insertBlockStmt
  , queryBlockCountStmt
  , queryBlockIdByHashStmt
  , queryLatestBlockIdStmt
  , queryLatestBlockNoStmt
  , queryLatestSlotNoStmt
  )
import DbSync.Db.Statement.SlotLeader (insertSlotLeaderStmt)
import DbSync.Test.Database
  ( setupFollowTipSchema
  , teardownSchema
  , truncateAllTables
  )
import DbSync.Test.Hasql (runStatement, withTestConnection)

tables :: [TableDef]
tables = [blockTableDef, slotLeaderTableDef]

spec :: Spec
spec = describe "DbSync.Db.Statement.Block" $
  beforeAll_ (setupFollowTipSchema tables [("core", 1)]) $
  afterAll_  (teardownSchema tables) $
  before_    (truncateAllTables ["block", "slot_leader"]) $ do

    describe "insertBlockStmt" $ do
      it "returns id 1 for the first insert" $
        withTestConnection $ \conn -> do
          slid <- runStatement conn sampleLeader insertSlotLeaderStmt
          bid  <- runStatement conn (sampleBlock slid Nothing 1 100)
                              insertBlockStmt
          bid `shouldBe` BlockId 1

      it "returns monotonically increasing ids" $
        withTestConnection $ \conn -> do
          slid <- runStatement conn sampleLeader insertSlotLeaderStmt
          b1   <- runStatement conn (sampleBlock slid Nothing       1 100) insertBlockStmt
          b2   <- runStatement conn (sampleBlock slid (Just b1)     2 200) insertBlockStmt
          b3   <- runStatement conn (sampleBlock slid (Just b2)     3 300) insertBlockStmt
          [b1, b2, b3] `shouldBe` [BlockId 1, BlockId 2, BlockId 3]

      it "round-trips a block with NULL block_no (Byron EBB shape)" $
        withTestConnection $ \conn -> do
          slid <- runStatement conn sampleLeader insertSlotLeaderStmt
          let ebb = (sampleBlock slid Nothing 0 100) { blockBlockNo = Nothing }
          bid <- runStatement conn ebb insertBlockStmt
          bid `shouldBe` BlockId 1

    describe "queryBlockIdByHashStmt" $ do
      it "returns Nothing when the hash is unknown" $
        withTestConnection $ \conn -> do
          mId <- runStatement conn "\xff\xff" queryBlockIdByHashStmt
          mId `shouldBe` Nothing

      it "returns Just id after an insert" $
        withTestConnection $ \conn -> do
          slid <- runStatement conn sampleLeader insertSlotLeaderStmt
          let blk = sampleBlock slid Nothing 1 100
          bid <- runStatement conn blk insertBlockStmt
          mId <- runStatement conn (blockHash blk) queryBlockIdByHashStmt
          mId `shouldBe` Just bid

    describe "queryBlockCountStmt" $ do
      it "returns 0 on an empty table" $
        withTestConnection $ \conn -> do
          n <- runStatement conn () queryBlockCountStmt
          n `shouldBe` 0

      it "tracks inserts" $
        withTestConnection $ \conn -> do
          slid <- runStatement conn sampleLeader insertSlotLeaderStmt
          _    <- runStatement conn (sampleBlock slid Nothing 1 100) insertBlockStmt
          _    <- runStatement conn (sampleBlock slid Nothing 2 200) insertBlockStmt
          n <- runStatement conn () queryBlockCountStmt
          n `shouldBe` 2

    describe "queryLatestBlockNoStmt" $ do
      it "returns Nothing on an empty table" $
        withTestConnection $ \conn -> do
          mNo <- runStatement conn () queryLatestBlockNoStmt
          mNo `shouldBe` Nothing

      it "returns the largest block_no" $
        withTestConnection $ \conn -> do
          slid <- runStatement conn sampleLeader insertSlotLeaderStmt
          _    <- runStatement conn (sampleBlock slid Nothing 1 100) insertBlockStmt
          _    <- runStatement conn (sampleBlock slid Nothing 7 700) insertBlockStmt
          _    <- runStatement conn (sampleBlock slid Nothing 3 300) insertBlockStmt
          mNo <- runStatement conn () queryLatestBlockNoStmt
          mNo `shouldBe` Just 7

      it "ignores blocks with NULL block_no" $
        withTestConnection $ \conn -> do
          slid <- runStatement conn sampleLeader insertSlotLeaderStmt
          let ebb = (sampleBlock slid Nothing 0 100) { blockBlockNo = Nothing }
          _ <- runStatement conn ebb insertBlockStmt
          _ <- runStatement conn (sampleBlock slid Nothing 1 200) insertBlockStmt
          mNo <- runStatement conn () queryLatestBlockNoStmt
          mNo `shouldBe` Just 1

    describe "queryLatestSlotNoStmt" $ do
      it "returns 0 on an empty table" $
        withTestConnection $ \conn -> do
          n <- runStatement conn () queryLatestSlotNoStmt
          n `shouldBe` 0

      it "returns the largest slot_no" $
        withTestConnection $ \conn -> do
          slid <- runStatement conn sampleLeader insertSlotLeaderStmt
          _    <- runStatement conn (sampleBlock slid Nothing 1 100) insertBlockStmt
          _    <- runStatement conn (sampleBlock slid Nothing 5 999) insertBlockStmt
          n <- runStatement conn () queryLatestSlotNoStmt
          n `shouldBe` 999

    describe "queryLatestBlockIdStmt" $ do
      it "returns Nothing on an empty table" $
        withTestConnection $ \conn -> do
          mId <- runStatement conn () queryLatestBlockIdStmt
          mId `shouldBe` Nothing

      it "returns the id of the block with the largest slot_no" $
        withTestConnection $ \conn -> do
          slid <- runStatement conn sampleLeader insertSlotLeaderStmt
          _    <- runStatement conn (sampleBlock slid Nothing 1 100) insertBlockStmt
          tip  <- runStatement conn (sampleBlock slid Nothing 7 700) insertBlockStmt
          _    <- runStatement conn (sampleBlock slid Nothing 3 300) insertBlockStmt
          mId  <- runStatement conn () queryLatestBlockIdStmt
          mId `shouldBe` Just tip

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleLeader :: SlotLeader
sampleLeader = SlotLeader
  { slotLeaderHash        = "\x10\x20\x30\x40"
  , slotLeaderPoolHashId  = Nothing
  , slotLeaderDescription = "test-leader"
  }

-- | Build a 'Block' parameterised by slot_leader_id, optional
-- previous_id, block_no, and slot_no. Hash is derived from block_no
-- so two blocks with different numbers don't collide.
sampleBlock :: SlotLeaderId -> Maybe BlockId -> Word64 -> Word64 -> Block
sampleBlock slid prev bno sno = Block
  { blockHash          = "\x00\x00\x00\x00\x00\x00\x00" <> hashByte bno
  , blockEpochNo       = Just 0
  , blockSlotNo        = Just sno
  , blockEpochSlotNo   = Just sno
  , blockBlockNo       = Just bno
  , blockPreviousId    = prev
  , blockSlotLeaderId  = slid
  , blockSize          = 1024
  , blockTime          = epoch
  , blockTxCount       = 0
  , blockProtoMajor    = 8
  , blockProtoMinor    = 0
  , blockVrfKey        = Nothing
  , blockOpCert        = Nothing
  , blockOpCertCounter = Nothing
  }
  where
    epoch = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
    hashByte :: Word64 -> ByteString
    hashByte n = encodeUtf8 (show n)
