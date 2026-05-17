{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Integration tests for 'DbSync.Checkpoint.Manager.commitEpoch'.
--
-- The tests drive the full epoch-commit path: multiple bulk
-- connections write rows, 'commitEpoch' is invoked, and we verify:
--
--   * All loader connections flushed their rows to the target tables.
--   * @dbsync_sync_state@ advanced to the committed slot.
--   * The ID counters match the values we passed in.
--   * A subsequent @lsReopen@ (implicit inside 'commitEpoch') lets
--     the writer keep streaming without errors.
--   * On the failure path (sync-state write fails) the already-flushed
--     rows remain in PG, while @last_committed_slot@ stays at the
--     previous epoch — the exact “rows past last_committed_slot”
--     scenario that the resume flow cleans up on boot.
--
-- Requires a running PostgreSQL instance with a @dbsync_test@ database.
module DbSync.Checkpoint.ManagerSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import qualified Data.Text as T

import qualified System.Process

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe)

import DbSync.Checkpoint.Manager (commitEpoch)
import DbSync.Db.Loader (LoaderStream (..), closeLoaderStream, mkLoaderStream)
import DbSync.Env (LoaderWithControl (..))
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Db.Loader.Encoder
  ( buildCopyRow
  , bBool
  , bHex
  , bInt64
  , bText
  , bUTCTime
  , bWord16
  , bWord64
  )
import DbSync.Checkpoint.SyncState
  ( ControlConnection
  , SyncStateRow (..)
  , closeControlConnection
  , openControlConnection
  , readSyncState
  , seedSyncState
  )
import DbSync.Test.Database (queryTestDb, testConnBs, testConnStr, testHasqlSettings, truncateAllTables)
import DbSync.AppM (runAppM)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

coreTables :: [TableDef]
coreTables = [blockTableDef, txTableDef, slotLeaderTableDef]

coreVersions :: [(Text, Int)]
coreVersions = [("core", 1)]

coreTableNames :: [Text]
coreTableNames = ["block", "tx", "slot_leader"]

-- | A SyncStateRow representing "epoch 5 just committed". Used both
-- for writes and for comparison on read.
epoch5Row :: SyncStateRow
epoch5Row = SyncStateRow
  { ssrLastCommittedSlot             = Just 20000
  , ssrLastCommittedBlockNo          = Just 999
  , ssrLastCommittedBlockHash        = Just (BS.pack [0xaa, 0xbb, 0xcc, 0xdd])
  , ssrLastSnapshotSlot              = Nothing
  , ssrBlockIdCounter                = 1000
  , ssrTxIdCounter                   = 5000
  , ssrTxOutIdCounter                = 15000
  , ssrTxInIdCounter                 = 14000
  , ssrCollateralTxInIdCounter       = 50
  , ssrReferenceTxInIdCounter        = 25
  , ssrTxMetadataIdCounter           = 100
  , ssrMaTxMintIdCounter             = 150
  , ssrMaTxOutIdCounter              = 200
  , ssrSlotLeaderIdCounter           = 8
  , ssrAddressIdCounter              = 1
  , ssrStakeAddressIdCounter         = 400
  , ssrPoolHashIdCounter             = 12
  , ssrMultiAssetIdCounter           = 300
  , ssrScriptIdCounter               = 40
  , ssrStakeRegistrationIdCounter    = 350
  , ssrStakeDeregistrationIdCounter  = 20
  , ssrDelegationIdCounter           = 450
  , ssrWithdrawalIdCounter           = 100
  , ssrPoolUpdateIdCounter           = 15
  , ssrPoolMetadataRefIdCounter      = 10
  , ssrPoolOwnerIdCounter            = 20
  , ssrPoolRetireIdCounter           = 3
  , ssrPoolRelayIdCounter            = 25
  , ssrTxCborIdCounter               = 5000
  , ssrEpochSyncStatsIdCounter       = 6
  , ssrAdaPotsIdCounter              = 6
  , ssrCollateralTxOutIdCounter              = 1
  , ssrSchemaVersionApplied          = 1
  , ssrLedgerEnabled                 = False
  , ssrSyncComplete                  = False
  }

-- | A follow-up SyncStateRow for epoch 6, advancing slot, block_no,
-- and a subset of counters.
epoch6Row :: SyncStateRow
epoch6Row = epoch5Row
  { ssrLastCommittedSlot    = Just 25000
  , ssrLastCommittedBlockNo = Just 1500
  , ssrBlockIdCounter       = 1501
  , ssrTxIdCounter           = 7500
  , ssrSlotLeaderIdCounter   = 9
  , ssrAddressIdCounter      = 1
  , ssrEpochSyncStatsIdCounter = 7
  }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withControlConnection :: (ControlConnection -> IO a) -> IO a
withControlConnection =
  bracket (openControlConnection testHasqlSettings) closeControlConnection

-- | Write one synthetic row into each of @block@, @tx@, @slot_leader@.
-- Uses the real 'buildCopyRow' encoders from @dbsync-db@ — hand-rolled
-- escaping would be off-by-one on the double-backslash convention.
writeOneOfEach :: LoaderStream -> Int64 -> IO ()
writeOneOfEach bs baseId = do
  -- slot_leader (id, hash, pool_hash_id, description)
  lsWriteRow bs "slot_leader" $
    buildCopyRow
      [ Just $ bInt64 baseId
      , Just $ bHex (BS.replicate 28 0xab)     -- 28-byte placeholder hash
      , Nothing                                -- pool_hash_id = NULL
      , Just $ bText "leader"
      ]
  -- block — 16 columns (matches blockTableDef)
  lsWriteRow bs "block" $
    buildCopyRow
      [ Just $ bInt64 baseId                           -- id
      , Just $ bHex (BS.replicate 32 0xaa)             -- hash
      , Just $ bWord64 5                               -- epoch_no
      , Just $ bWord64 (fromIntegral (20000 + baseId)) -- slot_no
      , Just $ bWord64 0                               -- epoch_slot_no
      , Just $ bWord64 (fromIntegral baseId)           -- block_no
      , Nothing                                        -- previous_id
      , Just $ bInt64 baseId                           -- slot_leader_id
      , Just $ bWord64 512                             -- size
      , Just $ bUTCTime sampleTime                     -- time
      , Just $ bWord64 1                               -- tx_count
      , Just $ bWord16 9                               -- proto_major
      , Just $ bWord16 0                               -- proto_minor
      , Nothing                                        -- vrf_key
      , Nothing                                        -- op_cert
      , Nothing                                        -- op_cert_counter
      ]
  -- tx — 13 columns (matches txTableDef)
  lsWriteRow bs "tx" $
    buildCopyRow
      [ Just $ bInt64 baseId                  -- id
      , Just $ bHex (BS.replicate 32 0xbb)    -- hash
      , Just $ bInt64 baseId                  -- block_id
      , Just $ bWord64 0                      -- block_index
      , Just $ bWord64 5000000                -- out_sum
      , Just $ bWord64 174000                 -- fee
      , Nothing                               -- deposit
      , Just $ bWord64 300                    -- size
      , Nothing                               -- invalid_before
      , Nothing                               -- invalid_hereafter
      , Just $ bBool True                     -- valid_contract
      , Just $ bWord64 0                      -- script_size
      , Just $ bWord64 0                      -- treasury_donation
      ]

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DbSync.Checkpoint.Manager.commitEpoch" $
  beforeAll_ (dropSchema coreTables coreVersions testConnStr >> initSchema coreTables coreVersions testConnStr) $
  afterAll_  (dropSchema coreTables coreVersions testConnStr) $
  before_    resetFixtures $ do

    it "flushes bulk data AND advances dbsync_sync_state atomically" $ do
      -- Arrange: control conn seeded, loader stream primed.
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        -- Act: push one row through each of the three tables, commit.
        writeOneOfEach bs 1
        runAppM (LoaderWithControl bs ctrl) (commitEpoch epoch5Row)
        closeLoaderStream bs
        -- Assert: all three data tables now have one row.
        blockCount <- T.strip <$> queryTestDb "SELECT count(*) FROM block;"
        txCount    <- T.strip <$> queryTestDb "SELECT count(*) FROM tx;"
        slCount    <- T.strip <$> queryTestDb "SELECT count(*) FROM slot_leader;"
        blockCount `shouldBe` "1"
        txCount    `shouldBe` "1"
        slCount    `shouldBe` "1"
        -- And sync_state reflects the new epoch.
        mRow <- runAppM ctrl readSyncState 
        case mRow of
          Just row -> row `shouldBe` epoch5Row
          Nothing  -> panic "sync_state was not updated"

    it "counters advance monotonically across two epochs" $
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        let env = LoaderWithControl bs ctrl
        -- Epoch 5
        writeOneOfEach bs 1
        runAppM env (commitEpoch epoch5Row)
        -- Epoch 6
        writeOneOfEach bs 2
        runAppM env (commitEpoch epoch6Row)
        closeLoaderStream bs
        mRow <- runAppM ctrl readSyncState
        case mRow of
          Just row -> do
            ssrLastCommittedSlot row    `shouldBe` Just 25000
            ssrLastCommittedBlockNo row `shouldBe` Just 1500
            ssrBlockIdCounter row       `shouldBe` 1501
            ssrTxIdCounter row          `shouldBe` 7500
          Nothing  -> panic "sync_state missing after two epochs"

    it "data table grows by one row per epoch" $
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        let env = LoaderWithControl bs ctrl
        writeOneOfEach bs 1
        runAppM env (commitEpoch epoch5Row)
        writeOneOfEach bs 2
        runAppM env (commitEpoch epoch6Row)
        closeLoaderStream bs
        blockCount <- T.strip <$> queryTestDb "SELECT count(*) FROM block;"
        blockCount `shouldBe` "2"

    it "when sync-state write fails, data rows stay committed (resume cleans up on boot)" $
      withControlConnection $ \ctrl -> do
        -- Deliberately do NOT seed the row. The UPDATE in
        -- writeSyncState will affect 0 rows and throw, but the bulk
        -- commit has already landed.
        bs <- mkLoaderStream testConnBs coreTables
        writeOneOfEach bs 1
        result <- try (runAppM (LoaderWithControl bs ctrl) (commitEpoch epoch5Row))
        closeLoaderStream bs
        case result of
          Left (_ :: SomeException) -> pure ()
          Right ()                  -> panic "commitEpoch should have thrown"
        -- Data is present (I1's soft variant: sync state ≤ data tip).
        blockCount <- T.strip <$> queryTestDb "SELECT count(*) FROM block;"
        blockCount `shouldBe` "1"
        -- Sync state is still empty (was never seeded + write failed).
        mRow <- runAppM ctrl readSyncState
        mRow `shouldBe` Nothing

  where
    -- Before each test: truncate everything so we start from a known
    -- state. Faster than drop/init, adequate for isolation.
    resetFixtures :: IO ()
    resetFixtures = do
      truncateAllTables coreTableNames
      _ <- System.Process.readProcessWithExitCode
        "psql"
        [T.unpack testConnStr, "-q", "-c", "TRUNCATE TABLE dbsync_sync_state;"]
        ""
      pure ()
