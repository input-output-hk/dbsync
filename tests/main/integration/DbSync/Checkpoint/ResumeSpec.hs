{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the resume path:
--
--   * 'DbSync.Checkpoint.Resume.deleteRowsPastSlot' — past-resume
--     row cleanup.
--   * 'DbSync.Checkpoint.SyncState.rebuildDedupMaps' — repopulating
--     the LSM-backed dedup stores from PG.
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

import DbSync.AppM (runAppM)
import DbSync.Checkpoint.Resume (CleanupMode (..), deleteRowsPastSlot)
import DbSync.Env (TracerWithControl (..))
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
import DbSync.Db.Loader (LoaderStream (..), closeLoaderStream, mkLoaderStream)
import DbSync.Phase.Type (SyncPhase (..), renderPhase)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.EpochSyncStats
  ( EpochSyncStats (..)
  , encodeEpochSyncStatsCopy
  , epochSyncStatsTableDef
  )
import DbSync.Db.Schema.Ids (EpochSyncStatsId (..))
import DbSync.Db.Schema.Init (dropSchema, initSchema)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert, sizeApprox)
import DbSync.Test.Lsm (withTestLsmSession)
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
import DbSync.Test.Database (queryTestDb, testConnBs, testConnStr, testHasqlSettings, truncateAllTables)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

coreTables :: [TableDef]
coreTables =
  [ blockTableDef
  , txTableDef
  , slotLeaderTableDef
  , epochSyncStatsTableDef
  ]

coreVersions :: [(Text, Int)]
coreVersions = [("core", 1), ("epoch_sync_stats", 1)]

coreTableNames :: [Text]
coreTableNames = map tdName coreTables

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

-- | A row that records committed progress at @slot_no = boundary@,
-- with the @slot_leader_id_counter@ set to @nextSlotLeaderId@.
--
-- Every other counter defaults to 'safeCounter' — a value safely
-- past any id 'populateChain' or 'populateEpochSyncStats' allocates,
-- so the counter pass of the cleanup is a no-op unless an
-- individual test overrides specific fields.
rowAtBoundary :: Word64 -> Int64 -> SyncStateRow
rowAtBoundary boundarySlot nextSlotLeaderId = SyncStateRow
  { ssrLastCommittedSlot             = Just boundarySlot
  , ssrLastCommittedBlockNo          = Just 99
  , ssrLastCommittedBlockHash        = Just (BS.replicate 32 0xaa)
  , ssrLastSnapshotSlot              = Nothing
  , ssrBlockIdCounter                = safeCounter
  , ssrTxIdCounter                   = safeCounter
  , ssrTxOutIdCounter                = safeCounter
  , ssrTxInIdCounter                 = safeCounter
  , ssrCollateralTxInIdCounter       = safeCounter
  , ssrReferenceTxInIdCounter        = safeCounter
  , ssrTxMetadataIdCounter           = safeCounter
  , ssrMaTxMintIdCounter             = safeCounter
  , ssrMaTxOutIdCounter              = safeCounter
  , ssrSlotLeaderIdCounter           = nextSlotLeaderId
  , ssrAddressIdCounter              = safeCounter
  , ssrStakeAddressIdCounter         = safeCounter
  , ssrPoolHashIdCounter             = safeCounter
  , ssrMultiAssetIdCounter           = safeCounter
  , ssrScriptIdCounter               = safeCounter
  , ssrStakeRegistrationIdCounter    = safeCounter
  , ssrStakeDeregistrationIdCounter  = safeCounter
  , ssrDelegationIdCounter           = safeCounter
  , ssrWithdrawalIdCounter           = safeCounter
  , ssrPoolUpdateIdCounter           = safeCounter
  , ssrPoolMetadataRefIdCounter      = safeCounter
  , ssrPoolOwnerIdCounter            = safeCounter
  , ssrPoolRetireIdCounter           = safeCounter
  , ssrPoolRelayIdCounter            = safeCounter
  , ssrTxCborIdCounter               = safeCounter
  , ssrEpochSyncStatsIdCounter       = safeCounter
  , ssrAdaPotsIdCounter              = safeCounter
  , ssrCollateralTxOutIdCounter      = safeCounter
  , ssrSchemaVersionApplied          = 1
  , ssrLedgerEnabled                 = False
  , ssrSyncComplete                  = False
  , ssrPendingRollbackSlot           = Nothing
  }

-- | A counter value safely past every id this spec's @populate*@
-- helpers allocate. Lets a test exercise one specific counter via
-- record-update syntax without the other counters tripping the
-- belt-and-braces sweep.
safeCounter :: Int64
safeCounter = 1000

-- | Push synthetic rows for blocks @[1..n]@. Block @i@ has slot
-- @i * 100@ and @block_no = i@. Tx @i@ points to block @i@.
-- slot_leader @i@ has hash @0xab @ replicated.
populateChain :: LoaderStream -> Int64 -> IO ()
populateChain bs n = do
  forM_ [1 .. n] $ \i -> do
    lsWriteRow bs (tdName slotLeaderTableDef) $
      buildCopyRow
        [ Just $ bInt64 i
        , Just $ bHex (BS.replicate 28 (fromIntegral (0xa0 + i)))
        , Nothing
        , Just $ bText ("leader-" <> T.pack (show i))
        ]
    lsWriteRow bs (tdName blockTableDef) $
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
    lsWriteRow bs (tdName txTableDef) $
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

-- | Push synthetic @epoch_sync_stats@ rows @[1..n]@. Stands in for
-- the Skip-with-counter case: the table has neither @slot_no@ nor
-- @block_id@, so the resume cleanup has to lean on the counter
-- pass to prune lagging rows.
populateEpochSyncStats :: LoaderStream -> Int64 -> IO ()
populateEpochSyncStats bs n =
  forM_ [1 .. n] $ \i ->
    lsWriteRow bs (tdName epochSyncStatsTableDef) $
      encodeEpochSyncStatsCopy
        (EpochSyncStatsId i)
        EpochSyncStats
          { epochSyncStatsEpochNo         = fromIntegral (i - 1)
          , epochSyncStatsBlocksProcessed = 100
          , epochSyncStatsBlocksPerSec    = 500.0
          , epochSyncStatsElapsedSec      = 0.2
          , epochSyncStatsSyncedAt        = sampleTime
          , epochSyncStatsPhase           = renderPhase IngestChainHistory
          }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withControlConnection :: (ControlConnection -> IO a) -> IO a
withControlConnection =
  bracket (openControlConnection testHasqlSettings) closeControlConnection

countRows :: TableDef -> IO Int
countRows td = do
  raw <- queryTestDb $ "SELECT count(*) FROM \"" <> tdName td <> "\";"
  case readMaybe (T.unpack (T.strip raw)) of
    Just n  -> pure n
    Nothing -> panic $ "countRows " <> tdName td <> ": unparseable result " <> raw

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
    ingestResumeSpec
    followRestartSpec

ingestResumeSpec :: Spec
ingestResumeSpec = describe "IngestResume" $ do

    it "is a no-op when last_committed_slot is Nothing" $ do
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 3
        lsCommit bs
        closeLoaderStream bs

        let row = (rowAtBoundary 100 1) { ssrLastCommittedSlot = Nothing }
        deleted <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot IngestResume coreTables row)
        deleted `shouldBe` 0

        countRows blockTableDef      >>= (`shouldBe` 3)
        countRows txTableDef         >>= (`shouldBe` 3)
        countRows slotLeaderTableDef >>= (`shouldBe` 3)

    it "deletes block rows past last_committed_slot" $ do
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 5  -- slots 100, 200, 300, 400, 500
        lsCommit bs
        closeLoaderStream bs

        -- Counter at 6 keeps slot_leader untouched; the test focuses
        -- on the slot-based fact-table cleanup.
        let row = rowAtBoundary 300 6
        runAppM ctrl (writeSyncState row)
        deleted <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot IngestResume coreTables row)

        deleted `shouldBe` 4
        countRows blockTableDef      >>= (`shouldBe` 3)
        countRows txTableDef         >>= (`shouldBe` 3)
        countRows slotLeaderTableDef >>= (`shouldBe` 5)

    it "deletes tx rows whose block crossed the boundary" $ do
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 5
        lsCommit bs
        closeLoaderStream bs

        let row = rowAtBoundary 250 6
        runAppM ctrl (writeSyncState row)
        _ <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot IngestResume coreTables row)

        remainingTx <- T.strip <$> queryTestDb
          ("SELECT id FROM " <> tdName txTableDef <> " ORDER BY id;")
        remainingTx `shouldBe` "1\n2"

    it "deletes dedup rows whose id >= the recorded counter" $ do
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 5
        lsCommit bs
        closeLoaderStream bs

        -- Counter at 4 means "next id is 4"; ids 4 and 5 are past
        -- the committed point.
        let row = rowAtBoundary 1000 4
        runAppM ctrl (writeSyncState row)
        _ <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot IngestResume coreTables row)

        countRows slotLeaderTableDef >>= (`shouldBe` 3)
        remaining <- T.strip <$> queryTestDb
          ("SELECT id FROM " <> tdName slotLeaderTableDef <> " ORDER BY id;")
        remaining `shouldBe` "1\n2\n3"

    it "deletes Skip-with-counter rows past the recorded counter" $ do
      -- @epoch_sync_stats@ has no @slot_no@ or @block_id@, so the
      -- only mechanism that can prune lagging rows is the counter
      -- pass. Pre-fix this was a no-op; the row past the recorded
      -- counter survived boot and the next boundary's COPY collided
      -- on it.
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 5
        populateEpochSyncStats bs 5
        lsCommit bs
        closeLoaderStream bs

        -- Counter at 3 means "next id is 3"; ids 3, 4, 5 are past
        -- the committed point.
        let row = (rowAtBoundary 1000 6)
                    { ssrEpochSyncStatsIdCounter = 3 }
        runAppM ctrl (writeSyncState row)
        _ <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot IngestResume coreTables row)

        countRows epochSyncStatsTableDef >>= (`shouldBe` 2)
        remaining <- T.strip <$> queryTestDb
          ("SELECT id FROM " <> tdName epochSyncStatsTableDef <> " ORDER BY id;")
        remaining `shouldBe` "1\n2"

    it "runs the counter pass alongside the slot pass (belt-and-braces)" $ do
      -- A counter that's even further back than @last_committed_slot@
      -- still wipes additional rows the slot pass left behind. The
      -- inverse — slot ahead of counter — would be the steady-state
      -- after a successful boundary commit, and is covered by the
      -- "no rows past the boundary" case below.
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 5
        populateEpochSyncStats bs 5
        lsCommit bs
        closeLoaderStream bs

        -- last_committed_slot=300 keeps blocks 1..3; the slot_leader
        -- counter at 2 separately wipes ids 2..5 of slot_leader; the
        -- epoch_sync_stats counter at 4 wipes ids 4..5 there.
        let row = (rowAtBoundary 300 2)
                    { ssrEpochSyncStatsIdCounter = 4 }
        runAppM ctrl (writeSyncState row)
        _ <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot IngestResume coreTables row)

        countRows blockTableDef          >>= (`shouldBe` 3)
        countRows txTableDef             >>= (`shouldBe` 3)
        countRows slotLeaderTableDef     >>= (`shouldBe` 1)
        countRows epochSyncStatsTableDef >>= (`shouldBe` 3)

    it "leaves everything alone when no rows are past the boundary" $ do
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 3
        lsCommit bs
        closeLoaderStream bs

        let row = rowAtBoundary 9_999 4
        runAppM ctrl (writeSyncState row)
        deleted <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot IngestResume coreTables row)
        deleted `shouldBe` 0

        countRows blockTableDef      >>= (`shouldBe` 3)
        countRows txTableDef         >>= (`shouldBe` 3)
        countRows slotLeaderTableDef >>= (`shouldBe` 3)

followRestartSpec :: Spec
followRestartSpec = describe "FollowRestart" $ do

    it "skips the counter DELETE even when ids exceed the counter" $ do
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 5
        populateEpochSyncStats bs 5
        lsCommit bs
        closeLoaderStream bs

        -- Counters frozen at the start-of-Follow value (1). Every
        -- existing row has id >= 1; the IngestResume mode would
        -- wipe them all. FollowRestart must leave them alone — the
        -- counters are stale on this path because
        -- @writeSyncStateSlotStmt@ doesn't touch them.
        let row = (rowAtBoundary 9_999 1)
                    { ssrLastCommittedBlockNo = Just 5
                    , ssrEpochSyncStatsIdCounter = 1
                    }
        runAppM ctrl (writeSyncState row)
        deleted <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot FollowRestart coreTables row)
        deleted `shouldBe` 0

        countRows slotLeaderTableDef     >>= (`shouldBe` 5)
        countRows blockTableDef          >>= (`shouldBe` 5)
        countRows txTableDef             >>= (`shouldBe` 5)
        countRows epochSyncStatsTableDef >>= (`shouldBe` 5)

    it "still trims fact-table rows past last_committed_slot" $ do
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 5
        lsCommit bs
        closeLoaderStream bs

        -- Defensive byBlockId / bySlot cleanup remains active: if a
        -- bug leaves fact rows past the committed slot, this still
        -- catches them. Counter at 1 must not affect the count.
        let row = rowAtBoundary 300 1
        runAppM ctrl (writeSyncState row)
        deleted <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot FollowRestart coreTables row)

        deleted `shouldBe` 4
        countRows blockTableDef      >>= (`shouldBe` 3)
        countRows txTableDef         >>= (`shouldBe` 3)
        countRows slotLeaderTableDef >>= (`shouldBe` 5)

    it "is a no-op when last_committed_slot is Nothing" $ do
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 3
        lsCommit bs
        closeLoaderStream bs

        let row = (rowAtBoundary 100 1) { ssrLastCommittedSlot = Nothing }
        deleted <- runAppM (TracerWithControl mkNullTracer ctrl) (deleteRowsPastSlot FollowRestart coreTables row)
        deleted `shouldBe` 0

        countRows slotLeaderTableDef >>= (`shouldBe` 3)

-- ---------------------------------------------------------------------------
-- rebuildDedupMaps
-- ---------------------------------------------------------------------------

rebuildDedupMapsSpec :: Spec
rebuildDedupMapsSpec = describe "DbSync.Checkpoint.SyncState.rebuildDedupMaps" $ do

    it "returns empty stores when no rows exist" $
      withControlConnection $ \ctrl ->
      withTestLsmSession $ \lsm -> do
        runAppM ctrl (seedSyncState 1 False)
        stores <- runAppM (TracerWithControl mkNullTracer ctrl)
                    (rebuildDedupMaps coreTables lsm)
        sizeApprox (dstSlotLeader stores)   >>= (`shouldBe` 0)
        sizeApprox (dstStakeAddress stores) >>= (`shouldBe` 0)
        sizeApprox (dstPoolHash stores)     >>= (`shouldBe` 0)
        sizeApprox (dstMultiAsset stores)   >>= (`shouldBe` 0)

    it "loads slot_leader rows back into the dedup store" $
      withControlConnection $ \ctrl ->
      withTestLsmSession $ \lsm -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 3
        lsCommit bs
        closeLoaderStream bs

        stores <- runAppM (TracerWithControl mkNullTracer ctrl)
                    (rebuildDedupMaps coreTables lsm)
        sizeApprox (dstSlotLeader stores) >>= (`shouldBe` 3)

        -- Looking up a known key returns the existing id, doesn't
        -- allocate a new one. (id 1 = first leader's hash, replicated
        -- 0xa1 across 28 bytes — see populateChain.)
        let key1 = SBS.toShort (BS.replicate 28 0xa1)
        (rowId, isNew) <- lookupOrInsert key1 (dstSlotLeader stores)
        rowId `shouldBe` 1
        isNew `shouldBe` False

    it "advances the counter past existing ids so new keys avoid collisions" $
      withControlConnection $ \ctrl ->
      withTestLsmSession $ \lsm -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 3
        lsCommit bs
        closeLoaderStream bs

        stores <- runAppM (TracerWithControl mkNullTracer ctrl)
                    (rebuildDedupMaps coreTables lsm)
        let unseenKey = SBS.toShort (BS.replicate 28 0xff)
        (rowId, isNew) <- lookupOrInsert unseenKey (dstSlotLeader stores)
        isNew `shouldBe` True
        rowId `shouldBe` 4    -- next id past the 3 rebuilt entries

    it "skips dedup tables not present in the schema list" $
      withControlConnection $ \ctrl ->
      withTestLsmSession $ \lsm -> do
        runAppM ctrl (seedSyncState 1 False)
        -- Only block + tx in the schema list — slot_leader is absent,
        -- so its store is left empty even if rows exist server-side.
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 3
        lsCommit bs
        closeLoaderStream bs

        stores <- runAppM (TracerWithControl mkNullTracer ctrl)
                    (rebuildDedupMaps [blockTableDef, txTableDef] lsm)
        sizeApprox (dstSlotLeader stores) >>= (`shouldBe` 0)

-- ---------------------------------------------------------------------------
-- fetchBlockHashAtSlot
-- ---------------------------------------------------------------------------

fetchBlockHashAtSlotSpec :: Spec
fetchBlockHashAtSlotSpec = describe "DbSync.Checkpoint.SyncState.fetchBlockHashAtSlot" $ do

    it "returns the hash of the block at the given slot" $
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 5  -- slots 100, 200, 300, 400, 500
        lsCommit bs
        closeLoaderStream bs

        -- 'populateChain' writes block i with hash 0xb0+i replicated.
        -- block 3 sits at slot 300 with hash 0xb3 replicated 32 times.
        result <- runAppM ctrl (fetchBlockHashAtSlot 300)
        result `shouldBe` Just (BS.replicate 32 0xb3)

    it "returns Nothing when no block is at the given slot" $
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        bs <- mkLoaderStream testConnBs coreTables
        populateChain bs 3
        lsCommit bs
        closeLoaderStream bs

        -- Slot 12345 is between populated slots, not on any of them.
        result <- runAppM ctrl (fetchBlockHashAtSlot 12345)
        result `shouldBe` Nothing

    it "returns Nothing on an empty block table" $
      withControlConnection $ \ctrl -> do
        runAppM ctrl (seedSyncState 1 False)
        result <- runAppM ctrl (fetchBlockHashAtSlot 100)
        result `shouldBe` Nothing

resetFixtures :: IO ()
resetFixtures = do
  truncateAllTables coreTableNames
  _ <- System.Process.readProcessWithExitCode
    "psql"
    [ T.unpack testConnStr, "-q", "-c"
    , "TRUNCATE TABLE " <> T.unpack (tdName syncStateTableDef) <> ";"
    ]
    ""
  pure ()
