{-# LANGUAGE OverloadedStrings #-}

-- | Statement-level tests for 'DbSync.Db.Statement.SlotLeader'.
module DbSync.Db.Statement.SlotLeaderSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe)

import DbSync.Db.Schema.Core (SlotLeader (..), slotLeaderTableDef)
import DbSync.Db.Schema.Ids (SlotLeaderId (..), PoolHashId (..))
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.SlotLeader
  ( insertSlotLeaderStmt
  , querySlotLeaderCountStmt
  , querySlotLeaderIdStmt
  )
import DbSync.Test.Database
  ( queryTestDb
  , setupFollowTipSchema
  , teardownSchema
  , truncateAllTables
  )
import DbSync.Test.Hasql (runStatement, withTestConnection)
import DbSync.Test.PgAssertions (tableColumn)

tables :: [TableDef]
tables = [slotLeaderTableDef]

spec :: Spec
spec = describe "DbSync.Db.Statement.SlotLeader" $
  beforeAll_ (setupFollowTipSchema tables [("core", 1)]) $
  afterAll_  (teardownSchema tables) $
  before_    (truncateAllTables (map tdName tables)) $ do

    describe "insertSlotLeaderStmt" $ do
      it "returns id 1 for the first insert" $
        withTestConnection $ \conn -> do
          sid <- runStatement conn sampleNoPool insertSlotLeaderStmt
          sid `shouldBe` SlotLeaderId 1

      it "returns monotonically increasing ids" $
        withTestConnection $ \conn -> do
          s1 <- runStatement conn sampleNoPool             insertSlotLeaderStmt
          s2 <- runStatement conn (renamed "leader-b")     insertSlotLeaderStmt
          s3 <- runStatement conn (renamed "leader-c")     insertSlotLeaderStmt
          [s1, s2, s3] `shouldBe` [SlotLeaderId 1, SlotLeaderId 2, SlotLeaderId 3]

      it "round-trips a non-null pool_hash_id" $
        withTestConnection $ \conn -> do
          let row = sampleNoPool { slotLeaderPoolHashId = Just (PoolHashId 42) }
          _ <- runStatement conn row insertSlotLeaderStmt
          phid <- T.strip <$> queryTestDb
            ( "SELECT " <> tableColumn slotLeaderTableDef "pool_hash_id"
                <> " FROM " <> tdName slotLeaderTableDef <> " LIMIT 1;"
            )
          phid `shouldBe` "42"

      it "preserves NULL pool_hash_id" $
        withTestConnection $ \conn -> do
          _ <- runStatement conn sampleNoPool insertSlotLeaderStmt
          isNull <- T.strip <$> queryTestDb
            ( "SELECT " <> tableColumn slotLeaderTableDef "pool_hash_id"
                <> " IS NULL FROM " <> tdName slotLeaderTableDef <> " LIMIT 1;"
            )
          isNull `shouldBe` "t"

    describe "querySlotLeaderIdStmt" $ do
      it "returns Nothing when the hash is unknown" $
        withTestConnection $ \conn -> do
          mId <- runStatement conn "\xff\xff\xff\xff" querySlotLeaderIdStmt
          mId `shouldBe` Nothing

      it "returns Just id after an insert" $
        withTestConnection $ \conn -> do
          sid <- runStatement conn sampleNoPool insertSlotLeaderStmt
          mId <- runStatement conn (slotLeaderHash sampleNoPool) querySlotLeaderIdStmt
          mId `shouldBe` Just sid

      it "is unaffected by other rows" $
        withTestConnection $ \conn -> do
          _    <- runStatement conn (renamed "other") insertSlotLeaderStmt
          sid  <- runStatement conn sampleNoPool      insertSlotLeaderStmt
          mId  <- runStatement conn (slotLeaderHash sampleNoPool) querySlotLeaderIdStmt
          mId `shouldBe` Just sid

    describe "querySlotLeaderCountStmt" $ do
      it "returns 0 on an empty table" $
        withTestConnection $ \conn -> do
          n <- runStatement conn () querySlotLeaderCountStmt
          n `shouldBe` 0

      it "tracks inserts" $
        withTestConnection $ \conn -> do
          _ <- runStatement conn sampleNoPool         insertSlotLeaderStmt
          _ <- runStatement conn (renamed "leader-b") insertSlotLeaderStmt
          n <- runStatement conn () querySlotLeaderCountStmt
          n `shouldBe` 2

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleNoPool :: SlotLeader
sampleNoPool = SlotLeader
  { slotLeaderHash        = "\x01\x02\x03\x04"
  , slotLeaderPoolHashId  = Nothing
  , slotLeaderDescription = "leader-a"
  }

renamed :: Text -> SlotLeader
renamed name = sampleNoPool
  { slotLeaderHash        = encodeUtf8 name
  , slotLeaderDescription = name
  }
