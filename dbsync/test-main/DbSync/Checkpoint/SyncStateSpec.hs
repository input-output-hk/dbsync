{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for 'DbSync.Checkpoint.SyncState'.
--
-- Exercises the full read\/write round-trip against a real PostgreSQL
-- database, including the edge cases that the boot flow depends on:
--
--   * 'readSyncState' on an empty table returns 'Nothing'.
--   * 'seedSyncState' is idempotent — second call is a no-op thanks
--     to the @ON CONFLICT DO NOTHING@ clause.
--   * 'writeSyncState' round-trips every field of a 'SyncStateRow'.
--   * 'writeSyncState' refuses to silently succeed when the row is
--     missing (i.e. when 'seedSyncState' never ran).
--
-- Requires a running PostgreSQL instance and a @dbsync_test@ database
-- the current user can create tables in.
module DbSync.Checkpoint.SyncStateSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.Text as T

import qualified System.Process

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe, shouldSatisfy)

import DbSync.Checkpoint.SyncState
  ( ControlConnection
  , SyncStateRow (..)
  , closeControlConnection
  , openControlConnection
  , readSyncState
  , seedSyncState
  , writeSyncState
  )
import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Error (AppError (..))
import DbSync.Test.Database (queryTestDb, testConnBs, testConnStr)

spec :: Spec
spec = describe "DbSync.Checkpoint.SyncState" $
  beforeAll_ (dropSchema [] [] testConnStr >> initSchema [] [] testConnStr) $
  afterAll_  (dropSchema [] [] testConnStr) $
  before_    resetSyncStateTable $ do

    describe "readSyncState on an un-seeded table" $
      it "returns Nothing" $
        withControlConnection $ \conn -> do
          row <- readSyncState conn
          row `shouldBe` Nothing

    describe "seedSyncState" $ do
      it "inserts the singleton row with all defaults" $
        withControlConnection $ \conn -> do
          seedSyncState conn 1 False
          mRow <- readSyncState conn
          mRow `shouldSatisfy` isJust
          case mRow of
            Nothing  -> panic "already asserted"
            Just row -> do
              ssrSchemaVersionApplied row   `shouldBe` 1
              ssrLedgerEnabled row          `shouldBe` False
              -- Every counter defaults to 1
              ssrBlockIdCounter row         `shouldBe` 1
              ssrTxIdCounter row            `shouldBe` 1
              ssrSlotLeaderIdCounter row    `shouldBe` 1
              ssrEpochSyncStatsIdCounter row `shouldBe` 1
              -- last_committed_* are NULL on a fresh seed
              ssrLastCommittedSlot row      `shouldBe` Nothing
              ssrLastCommittedBlockNo row   `shouldBe` Nothing
              ssrLastCommittedBlockHash row `shouldBe` Nothing

      it "captures ledger_enabled = True when requested" $
        withControlConnection $ \conn -> do
          seedSyncState conn 1 True
          mRow <- readSyncState conn
          case mRow of
            Just row -> ssrLedgerEnabled row `shouldBe` True
            Nothing  -> panic "seed did not persist"

      it "is idempotent — second call does not create a second row" $
        withControlConnection $ \conn -> do
          seedSyncState conn 1 False
          seedSyncState conn 1 False
          seedSyncState conn 1 True    -- different args — still a no-op
          rowCount <- T.strip <$> queryTestDb "SELECT count(*) FROM dbsync_sync_state;"
          rowCount `shouldBe` "1"
          -- And the first seeding wins (ledger_enabled stays False)
          mRow <- readSyncState conn
          case mRow of
            Just row -> ssrLedgerEnabled row `shouldBe` False
            Nothing  -> panic "row vanished between seeds"

      it "enforces the id=1 CHECK — manual INSERT with id=2 fails" $ do
        result <- tryRaisingInsert
        result `shouldSatisfy` isLeft

    describe "writeSyncState round-trip" $ do
      it "writes every field, then readSyncState returns the same row" $
        withControlConnection $ \conn -> do
          seedSyncState conn 1 True
          writeSyncState conn sampleRow
          mReadBack <- readSyncState conn
          case mReadBack of
            Just readBack -> readBack `shouldBe` sampleRow
            Nothing       -> panic "row vanished after write"

      it "overwrites previous values on repeated writes" $
        withControlConnection $ \conn -> do
          seedSyncState conn 1 True
          writeSyncState conn sampleRow
          writeSyncState conn sampleRow { ssrLastCommittedSlot = Just 12345 }
          mReadBack <- readSyncState conn
          case mReadBack of
            Just readBack -> ssrLastCommittedSlot readBack `shouldBe` Just 12345
            Nothing       -> panic "row vanished after write"

      it "throws AppDatabaseError when the row was never seeded" $
        withControlConnection $ \conn -> do
          -- resetSyncStateTable above leaves the table empty; don't seed.
          result <- try (writeSyncState conn sampleRow)
          case result of
            Left (AppDatabaseError _ msg) ->
              msg `shouldSatisfy` T.isInfixOf "expected exactly 1"
            Left other -> panic $ "Wrong exception type: " <> show other
            Right ()   -> panic "writeSyncState should have thrown"

      it "preserves NULL in last_committed_block_hash" $
        withControlConnection $ \conn -> do
          seedSyncState conn 1 False
          writeSyncState conn sampleRow { ssrLastCommittedBlockHash = Nothing }
          mReadBack <- readSyncState conn
          case mReadBack of
            Just readBack -> ssrLastCommittedBlockHash readBack `shouldBe` Nothing
            Nothing       -> panic "row vanished after write"

      it "round-trips a realistic 32-byte block hash" $
        withControlConnection $ \conn -> do
          seedSyncState conn 1 False
          let bigHash = BS.pack
                [ 0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89
                , 0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89
                , 0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89
                , 0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89
                ]
          writeSyncState conn sampleRow { ssrLastCommittedBlockHash = Just bigHash }
          mReadBack <- readSyncState conn
          case mReadBack of
            Just readBack -> ssrLastCommittedBlockHash readBack `shouldBe` Just bigHash
            Nothing       -> panic "row vanished after write"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withControlConnection :: (ControlConnection -> IO a) -> IO a
withControlConnection = bracket (openControlConnection testConnBs) closeControlConnection

-- | Empty the sync-state table between tests. Avoids re-creating the
-- schema on every test (which is slow) but still gives each @it@
-- a fresh starting point.
resetSyncStateTable :: IO ()
resetSyncStateTable = do
  _ <- System.Process.readProcessWithExitCode
    "psql"
    [T.unpack testConnStr, "-q", "-c", "TRUNCATE TABLE dbsync_sync_state;"]
    ""
  pure ()

-- | Attempt a raw @INSERT@ that violates the @CHECK (id = 1)@
-- constraint. Uses @psql@ directly so the error path is exercised
-- server-side — our own 'ControlConnection' code never produces
-- such INSERTs. Returns @Right ()@ on (unexpected) success or
-- @Left errorText@ on the expected failure.
tryRaisingInsert :: IO (Either Text ())
tryRaisingInsert = do
  (exit, _out, err) <- System.Process.readProcessWithExitCode
    "psql"
    [ T.unpack testConnStr
    , "-q"
    , "-v", "ON_ERROR_STOP=1"
    , "-c"
    , "INSERT INTO dbsync_sync_state (id, schema_version_applied, ledger_enabled) \
      \VALUES (2, 1, false);"
    ]
    ""
  case exit of
    ExitSuccess -> pure (Right ())
    _           -> pure (Left (T.pack err))

-- ---------------------------------------------------------------------------
-- Sample row
-- ---------------------------------------------------------------------------

-- | Non-trivial row exercising every field. Values chosen to be
-- distinguishable on round-trip.
sampleRow :: SyncStateRow
sampleRow = SyncStateRow
  { ssrLastCommittedSlot             = Just 1000
  , ssrLastCommittedBlockNo          = Just 500
  , ssrLastCommittedBlockHash        = Just (BS.pack [0xde, 0xad, 0xbe, 0xef])
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
  }
