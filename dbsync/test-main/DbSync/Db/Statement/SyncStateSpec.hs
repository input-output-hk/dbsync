{-# LANGUAGE OverloadedStrings #-}

-- | Statement-level tests for 'DbSync.Db.Statement.SyncState'.
--
-- This module is the worked reference for the
-- @DbSync.Db.Statement.<Name>Spec@ pattern: open a test connection,
-- 'runStatement' the value under test, assert. Wrapper-level
-- coverage for the same Statements (going through
-- 'DbSync.Checkpoint.SyncState.readSyncState' /
-- 'DbSync.Checkpoint.SyncState.seedSyncState' /
-- 'DbSync.Checkpoint.SyncState.writeSyncState' /
-- 'DbSync.Checkpoint.SyncState.markSnapshotComplete' /
-- 'DbSync.Checkpoint.SyncState.markSyncComplete') lives in
-- 'DbSync.Checkpoint.SyncStateSpec'; the wrappers add caller-name
-- error context and \"row count\" assertions on top of the bare
-- Statement, so each layer pulls its own weight in tests.
--
-- The Statements here are the ones we'll be expanding when
-- 'PreparingForChainTip' and 'FollowingChainTip' land — having the
-- pattern in place before then keeps the new query work testable
-- from the first commit.
module DbSync.Db.Statement.SyncStateSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Db.Schema.SyncState (SyncStateRow (..))
import DbSync.Db.Statement.SyncState
  ( markSnapshotCompleteStmt
  , markSyncCompleteStmt
  , readSyncStateStmt
  , seedSyncStateStmt
  , writeSyncStateStmt
  )
import DbSync.Test.Database (queryTestDb, testConnStr)
import DbSync.Test.Hasql (runStatement, withTestConnection)

spec :: Spec
spec = describe "DbSync.Db.Statement.SyncState" $
  beforeAll_ (dropSchema [] [] testConnStr >> initSchema [] [] testConnStr) $
  afterAll_  (dropSchema [] [] testConnStr) $
  before_    truncateSyncState $ do

    describe "readSyncStateStmt" $
      it "returns Nothing on an empty table" $
        withTestConnection $ \conn -> do
          row <- runStatement conn () readSyncStateStmt
          row `shouldBe` Nothing

    describe "seedSyncStateStmt" $ do
      it "inserts the singleton row" $
        withTestConnection $ \conn -> do
          runStatement conn (1, False) seedSyncStateStmt
          row <- runStatement conn () readSyncStateStmt
          row `shouldSatisfy` isJust
          case row of
            Just r -> do
              ssrSchemaVersionApplied r `shouldBe` 1
              ssrLedgerEnabled r        `shouldBe` False
            Nothing -> panic "row should be present"

      it "is idempotent (ON CONFLICT DO NOTHING)" $
        withTestConnection $ \conn -> do
          runStatement conn (1, False) seedSyncStateStmt
          -- A second seed with different args is a no-op — the
          -- first seed's values win.
          runStatement conn (1, True)  seedSyncStateStmt
          rowCount <- T.strip <$> queryTestDb "SELECT count(*) FROM dbsync_sync_state;"
          rowCount `shouldBe` "1"
          row <- runStatement conn () readSyncStateStmt
          case row of
            Just r  -> ssrLedgerEnabled r `shouldBe` False
            Nothing -> panic "row should be present"

    describe "writeSyncStateStmt" $ do
      it "reports 0 rows affected when the row was never seeded" $
        withTestConnection $ \conn -> do
          n <- runStatement conn sampleRow writeSyncStateStmt
          n `shouldBe` 0

      it "round-trips every column through readSyncStateStmt" $
        withTestConnection $ \conn -> do
          runStatement conn (1, True) seedSyncStateStmt
          n <- runStatement conn sampleRow writeSyncStateStmt
          n `shouldBe` 1
          mRow <- runStatement conn () readSyncStateStmt
          case mRow of
            Just row -> row `shouldBe` sampleRow
            Nothing  -> panic "row vanished after write"

    describe "markSnapshotCompleteStmt" $
      it "updates last_snapshot_slot in isolation" $
        withTestConnection $ \conn -> do
          runStatement conn (1, True) seedSyncStateStmt
          _ <- runStatement conn sampleRow writeSyncStateStmt
          n <- runStatement conn 1234 markSnapshotCompleteStmt
          n `shouldBe` 1
          mRow <- runStatement conn () readSyncStateStmt
          case mRow of
            Just row -> do
              ssrLastSnapshotSlot row    `shouldBe` Just 1234
              -- Consumer-owned fields are untouched.
              ssrLastCommittedSlot row   `shouldBe` ssrLastCommittedSlot sampleRow
              ssrSchemaVersionApplied row `shouldBe` ssrSchemaVersionApplied sampleRow
            Nothing -> panic "row vanished"

    describe "markSyncCompleteStmt" $
      it "flips sync_complete to true once seeded" $
        withTestConnection $ \conn -> do
          runStatement conn (1, False) seedSyncStateStmt
          n <- runStatement conn () markSyncCompleteStmt
          n `shouldBe` 1
          mRow <- runStatement conn () readSyncStateStmt
          case mRow of
            Just row -> ssrSyncComplete row `shouldBe` True
            Nothing  -> panic "row vanished"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Empty the sync-state table between tests. Cheaper than dropping
-- and re-creating the whole schema; same independence guarantee as
-- long as no test leaks rows in dependent tables.
truncateSyncState :: IO ()
truncateSyncState = do
  -- Use psql via 'queryTestDb' rather than another hasql connection
  -- to keep this helper independent of 'runStatement' (the thing
  -- under test).
  _ <- queryTestDb "TRUNCATE TABLE dbsync_sync_state;"
  pure ()

-- ---------------------------------------------------------------------------
-- Fixture row
-- ---------------------------------------------------------------------------

-- | Non-trivial row exercising every counter and the
-- @last_committed_*@ trio. Field values are deliberately
-- distinguishable on round-trip.
sampleRow :: SyncStateRow
sampleRow = SyncStateRow
  { ssrLastCommittedSlot             = Just 1000
  , ssrLastCommittedBlockNo          = Just 500
  , ssrLastCommittedBlockHash        = Just "\xde\xad\xbe\xef"
  , ssrLastSnapshotSlot              = Nothing
  , ssrBlockIdCounter                = 501
  , ssrTxIdCounter                   = 1500
  , ssrTxOutIdCounter                = 3000
  , ssrTxInIdCounter                 = 2800
  , ssrCollateralTxInIdCounter       = 100
  , ssrReferenceTxInIdCounter        = 50
  , ssrTxMetadataIdCounter           = 200
  , ssrMaTxMintIdCounter             = 300
  , ssrMaTxOutIdCounter              = 400
  , ssrSlotLeaderIdCounter           = 10
  , ssrStakeAddressIdCounter         = 750
  , ssrPoolHashIdCounter             = 25
  , ssrMultiAssetIdCounter           = 600
  , ssrScriptIdCounter               = 80
  , ssrStakeRegistrationIdCounter    = 700
  , ssrStakeDeregistrationIdCounter  = 50
  , ssrDelegationIdCounter           = 900
  , ssrWithdrawalIdCounter           = 200
  , ssrPoolUpdateIdCounter           = 30
  , ssrPoolMetadataRefIdCounter      = 20
  , ssrPoolOwnerIdCounter            = 35
  , ssrPoolRetireIdCounter           = 5
  , ssrPoolRelayIdCounter            = 40
  , ssrTxCborIdCounter               = 1500
  , ssrEpochSyncStatsIdCounter       = 5
  , ssrAdaPotsIdCounter              = 5
  , ssrSchemaVersionApplied          = 1
  , ssrLedgerEnabled                 = True
  , ssrSyncComplete                  = False
  }
