{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the resume path:
--
--   * 'DbSync.Checkpoint.Resume.deleteRowsPastSlot' — past-resume
--     row cleanup.
--   * 'DbSync.Checkpoint.SyncState.rebuildDedupMaps' — repopulating
--     the in-memory dedup maps from PG.
--   * 'DbSync.Checkpoint.SyncState.fetchBlockHashAtSlot' — looking
--     up the canonical block hash for a snapshot's slot.
module DbSync.Checkpoint.ResumeSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import qualified Data.Text as T

import qualified System.Process

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe)

import qualified Data.ByteString.Short as SBS

import DbSync.Checkpoint.Resume (deleteRowsPastSlot)
import DbSync.Trace.Backend (mkNullTracer)
import DbSync.Checkpoint.SyncState
  ( ControlConnection
  , SyncStateRow (..)
  , closeControlConnection
  , fetchBlockHashAtSlot
  , openControlConnection
  , rebuildDedupMaps
  , seedSyncState
  , writeSyncState
  )
import DbSync.Copy.Writer (CopyWriter (..), closeCopyWriter, mkCopyWriter)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Id.DedupMap (DedupMaps (..), lookupOrInsert, size)
import DbSync.Db.Writer.Copy.Encoder
  ( buildCopyRow
  , bBool
  , bHex
  , bInt64
  , bText
  , bUTCTime
  , bWord16
  , bWord64
  )
import DbSync.Test.Database (queryTestDb, testConnBs, testConnStr, testHasqlSettings, truncateAllTables)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

coreTables :: [TableDef]
coreTables = [blockTableDef, txTableDef, slotLeaderTableDef]

coreVersions :: [(Text, Int)]
coreVersions = [("core", 1)]

coreTableNames :: [Text]
coreTableNames = ["block", "tx", "slot_leader"]

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

-- | A row that records committed progress at @slot_no = boundary@,
-- with the dedup counters set to @nextSlotLeaderId@. Other counters
-- carry their seeded values (1) so 'deleteDedupByCounterStmt' is a
-- no-op for them.
rowAtBoundary :: Word64 -> Int64 -> SyncStateRow
rowAtBoundary boundarySlot nextSlotLeaderId = SyncStateRow
  { ssrLastCommittedSlot             = Just boundarySlot
  , ssrLastCommittedBlockNo          = Just 99
  , ssrLastCommittedBlockHash        = Just (BS.replicate 32 0xaa)
  , ssrLastSnapshotSlot              = Nothing
  , ssrBlockIdCounter                = 1
  , ssrTxIdCounter                   = 1
  , ssrTxOutIdCounter                = 1
  , ssrTxInIdCounter                 = 1
  , ssrCollateralTxInIdCounter       = 1
  , ssrReferenceTxInIdCounter        = 1
  , ssrTxMetadataIdCounter           = 1
  , ssrMaTxMintIdCounter             = 1
  , ssrMaTxOutIdCounter              = 1
  , ssrSlotLeaderIdCounter           = nextSlotLeaderId
  , ssrAddressIdCounter              = 1
  , ssrStakeAddressIdCounter         = 1
  , ssrPoolHashIdCounter             = 1
  , ssrMultiAssetIdCounter           = 1
  , ssrScriptIdCounter               = 1
  , ssrStakeRegistrationIdCounter    = 1
  , ssrStakeDeregistrationIdCounter  = 1
  , ssrDelegationIdCounter           = 1
  , ssrWithdrawalIdCounter           = 1
  , ssrPoolUpdateIdCounter           = 1
  , ssrPoolMetadataRefIdCounter      = 1
  , ssrPoolOwnerIdCounter            = 1
  , ssrPoolRetireIdCounter           = 1
  , ssrPoolRelayIdCounter            = 1
  , ssrTxCborIdCounter               = 1
  , ssrEpochSyncStatsIdCounter       = 1
  , ssrAdaPotsIdCounter              = 1
  , ssrCollateralTxOutIdCounter              = 1
  , ssrSchemaVersionApplied          = 1
  , ssrLedgerEnabled                 = False
  , ssrSyncComplete                  = False
  }

-- | Push synthetic rows for blocks @[1..n]@. Block @i@ has slot
-- @i * 100@ and @block_no = i@. Tx @i@ points to block @i@.
-- slot_leader @i@ has hash @0xab @ replicated.
populateChain :: CopyWriter -> Int64 -> IO ()
populateChain cw n = do
  forM_ [1 .. n] $ \i -> do
    cwWriteRow cw "slot_leader" $
      buildCopyRow
        [ Just $ bInt64 i
        , Just $ bHex (BS.replicate 28 (fromIntegral (0xa0 + i)))
        , Nothing
        , Just $ bText ("leader-" <> T.pack (show i))
        ]
    cwWriteRow cw "block" $
      buildCopyRow
        [ Just $ bInt64 i                                            -- id
        , Just $ bHex (BS.replicate 32 (fromIntegral (0xb0 + i)))    -- hash
        , Just $ bWord64 5                                           -- epoch_no
        , Just $ bWord64 (fromIntegral i * 100)                      -- slot_no
        , Just $ bWord64 0                                           -- epoch_slot_no
        , Just $ bWord64 (fromIntegral i)                            -- block_no
        , Nothing                                                    -- previous_id
        , Just $ bInt64 i                                            -- slot_leader_id
        , Just $ bWord64 512                                         -- size
        , Just $ bUTCTime sampleTime                                 -- time
        , Just $ bWord64 1                                           -- tx_count
        , Just $ bWord16 9                                           -- proto_major
        , Just $ bWord16 0                                           -- proto_minor
        , Nothing                                                    -- vrf_key
        , Nothing                                                    -- op_cert
        , Nothing                                                    -- op_cert_counter
        ]
    cwWriteRow cw "tx" $
      buildCopyRow
        [ Just $ bInt64 i                                            -- id
        , Just $ bHex (BS.replicate 32 (fromIntegral (0xc0 + i)))    -- hash
        , Just $ bInt64 i                                            -- block_id
        , Just $ bWord64 0                                           -- block_index
        , Just $ bWord64 5_000_000                                   -- out_sum
        , Just $ bWord64 174_000                                     -- fee
        , Nothing                                                    -- deposit
        , Just $ bWord64 300                                         -- size
        , Nothing                                                    -- invalid_before
        , Nothing                                                    -- invalid_hereafter
        , Just $ bBool True                                          -- valid_contract
        , Just $ bWord64 0                                           -- script_size
        , Just $ bWord64 0                                           -- treasury_donation
        ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withControlConnection :: (ControlConnection -> IO a) -> IO a
withControlConnection =
  bracket (openControlConnection testHasqlSettings) closeControlConnection

countRows :: Text -> IO Int
countRows table = do
  raw <- queryTestDb $ "SELECT count(*) FROM \"" <> table <> "\";"
  case readMaybe (T.unpack (T.strip raw)) of
    Just n  -> pure n
    Nothing -> panic $ "countRows " <> table <> ": unparseable result " <> raw

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec =
  beforeAll_ (dropSchema coreTables coreVersions testConnStr >> initSchema coreTables coreVersions testConnStr) $
  afterAll_  (dropSchema coreTables coreVersions testConnStr) $
  before_    resetFixtures $ do
    deleteRowsPastSlotSpec
    rebuildDedupMapsSpec
    fetchBlockHashAtSlotSpec

deleteRowsPastSlotSpec :: Spec
deleteRowsPastSlotSpec = describe "DbSync.Checkpoint.Resume.deleteRowsPastSlot" $ do

    it "is a no-op when last_committed_slot is Nothing" $ do
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 3
        cwCommit cw
        closeCopyWriter cw

        let row = (rowAtBoundary 100 1) { ssrLastCommittedSlot = Nothing }
        deleted <- deleteRowsPastSlot ctrl coreTables row
        deleted `shouldBe` 0

        -- All rows still present.
        countRows "block"       >>= (`shouldBe` 3)
        countRows "tx"          >>= (`shouldBe` 3)
        countRows "slot_leader" >>= (`shouldBe` 3)

    it "deletes block rows past last_committed_slot" $ do
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 5  -- slots 100, 200, 300, 400, 500
        cwCommit cw
        closeCopyWriter cw

        -- Boundary at slot 300: blocks 4 (slot=400) and 5 (slot=500)
        -- are past. Counter at 6 (one past the highest populated id)
        -- so the dedup-cleanup leaves slot_leader alone.
        let row = rowAtBoundary 300 6
        writeSyncState ctrl row
        deleted <- deleteRowsPastSlot ctrl coreTables row

        -- 2 blocks + 2 tx (cleaned via block_id) past the slot.
        deleted `shouldBe` 4
        countRows "block"       >>= (`shouldBe` 3)
        countRows "tx"          >>= (`shouldBe` 3)
        countRows "slot_leader" >>= (`shouldBe` 5)

    it "deletes tx rows whose block crossed the boundary" $ do
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 5
        cwCommit cw
        closeCopyWriter cw

        let row = rowAtBoundary 250 6
        writeSyncState ctrl row
        _ <- deleteRowsPastSlot ctrl coreTables row

        -- Only tx for blocks at slots <= 250 should remain (blocks 1, 2).
        remainingTx <-
          T.strip <$> queryTestDb "SELECT id FROM tx ORDER BY id;"
        remainingTx `shouldBe` "1\n2"

    it "deletes dedup rows whose id >= the recorded counter" $ do
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 5
        cwCommit cw
        closeCopyWriter cw

        -- Pretend the counter for slot_leader was at 4 when the
        -- crash happened: ids 4 and 5 are past the committed point.
        let row = rowAtBoundary 1000 4    -- boundary above all slots
        writeSyncState ctrl row
        _ <- deleteRowsPastSlot ctrl coreTables row

        countRows "slot_leader" >>= (`shouldBe` 3)
        remaining <-
          T.strip <$> queryTestDb "SELECT id FROM slot_leader ORDER BY id;"
        remaining `shouldBe` "1\n2\n3"

    it "leaves everything alone when no rows are past the boundary" $ do
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 3
        cwCommit cw
        closeCopyWriter cw

        -- Boundary higher than the max slot (300). slot_leader counter
        -- at 4 means \"next id is 4\"; ids 1-3 are below that.
        let row = rowAtBoundary 9_999 4
        writeSyncState ctrl row
        deleted <- deleteRowsPastSlot ctrl coreTables row
        deleted `shouldBe` 0

        countRows "block"       >>= (`shouldBe` 3)
        countRows "tx"          >>= (`shouldBe` 3)
        countRows "slot_leader" >>= (`shouldBe` 3)

-- ---------------------------------------------------------------------------
-- rebuildDedupMaps
-- ---------------------------------------------------------------------------

rebuildDedupMapsSpec :: Spec
rebuildDedupMapsSpec = describe "DbSync.Checkpoint.SyncState.rebuildDedupMaps" $ do

    it "returns empty maps when no rows exist" $
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        maps <- rebuildDedupMaps mkNullTracer ctrl coreTables
        size (dmsSlotLeader maps)   >>= (`shouldBe` 0)
        size (dmsStakeAddress maps) >>= (`shouldBe` 0)
        size (dmsPoolHash maps)     >>= (`shouldBe` 0)
        size (dmsMultiAsset maps)   >>= (`shouldBe` 0)

    it "loads slot_leader rows back into the dedup map" $
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 3
        cwCommit cw
        closeCopyWriter cw

        maps <- rebuildDedupMaps mkNullTracer ctrl coreTables
        size (dmsSlotLeader maps) >>= (`shouldBe` 3)

        -- Looking up a known key returns the existing id, doesn't
        -- allocate a new one. (id 1 = first leader's hash, replicated
        -- 0xa1 across 28 bytes — see populateChain.)
        let key1 = SBS.toShort (BS.replicate 28 0xa1)
        (rowId, isNew) <- lookupOrInsert key1 (dmsSlotLeader maps)
        rowId `shouldBe` 1
        isNew `shouldBe` False

    it "advances the counter past existing ids so new keys avoid collisions" $
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 3
        cwCommit cw
        closeCopyWriter cw

        maps <- rebuildDedupMaps mkNullTracer ctrl coreTables
        let unseenKey = SBS.toShort (BS.replicate 28 0xff)
        (rowId, isNew) <- lookupOrInsert unseenKey (dmsSlotLeader maps)
        isNew `shouldBe` True
        rowId `shouldBe` 4    -- next id past the 3 rebuilt entries

    it "skips dedup tables not present in the schema list" $
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        -- Only block + tx in the schema list — slot_leader is absent,
        -- so its map is left empty even if rows exist server-side.
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 3
        cwCommit cw
        closeCopyWriter cw

        maps <- rebuildDedupMaps mkNullTracer ctrl [blockTableDef, txTableDef]
        size (dmsSlotLeader maps) >>= (`shouldBe` 0)

-- ---------------------------------------------------------------------------
-- fetchBlockHashAtSlot
-- ---------------------------------------------------------------------------

fetchBlockHashAtSlotSpec :: Spec
fetchBlockHashAtSlotSpec = describe "DbSync.Checkpoint.SyncState.fetchBlockHashAtSlot" $ do

    it "returns the hash of the block at the given slot" $
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 5  -- slots 100, 200, 300, 400, 500
        cwCommit cw
        closeCopyWriter cw

        -- 'populateChain' writes block i with hash 0xb0+i replicated.
        -- block 3 sits at slot 300 with hash 0xb3 replicated 32 times.
        result <- fetchBlockHashAtSlot ctrl 300
        result `shouldBe` Just (BS.replicate 32 0xb3)

    it "returns Nothing when no block is at the given slot" $
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        cw <- mkCopyWriter testConnBs coreTables
        populateChain cw 3
        cwCommit cw
        closeCopyWriter cw

        -- Slot 12345 is between populated slots, not on any of them.
        result <- fetchBlockHashAtSlot ctrl 12345
        result `shouldBe` Nothing

    it "returns Nothing on an empty block table" $
      withControlConnection $ \ctrl -> do
        seedSyncState ctrl 1 False
        result <- fetchBlockHashAtSlot ctrl 100
        result `shouldBe` Nothing

resetFixtures :: IO ()
resetFixtures = do
  truncateAllTables coreTableNames
  _ <- System.Process.readProcessWithExitCode
    "psql"
    [T.unpack testConnStr, "-q", "-c", "TRUNCATE TABLE dbsync_sync_state;"]
    ""
  pure ()
