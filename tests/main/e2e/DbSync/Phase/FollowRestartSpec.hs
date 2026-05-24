{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Restart a sync that's already in 'FollowingChainTip' and verify
-- the second run picks up cleanly across three configurations:
--
--   * Ledger off — the dedup-counter cleanup must not wipe
--     legitimately-committed rows on a Follow restart
--     ('CleanupMode.FollowRestart').
--   * Ledger on, snapshot aligned with PG — the restart path
--     restores the in-memory 'LedgerDB' from the on-disk snapshot.
--   * Ledger on, snapshot lags @last_committed_slot@ (the natural
--     state on any non-boundary stop) — the restart path replays
--     the gap through the ledger worker via the receiver fan-out;
--     Follow\'s consumer skips its PG-write path for the replayed
--     range so committed rows are preserved.
module DbSync.Phase.FollowRestartSpec (spec) where

import Cardano.Prelude

import qualified Data.List as List
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Config.Types (SyncConfig)
import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Pool (poolHashTableDef)
import DbSync.Db.Schema.StakeDelegation (stakeAddressTableDef)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (txOutTableDef)
import DbSync.Trace.Backend (mkTestTracer)
import DbSync.Trace.Types (LogMsg (..))
import DbSync.Test.AppHarness
  ( defaultTestProfile
  , ledgerEnabledTestProfile
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E
  ( conwayConfigDir
  , forgeAndWaitForBlocks
  , listLedgerSnapshots
  , syncCompleteTrue
  , withAppSession
  , withAppSessionResume
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode (MockNode, forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions (countRows, tableColumn)

spec :: Spec
spec = describe "FollowingChainTip restart" $ do

  it "preserves dedup rows and resumes inserting (ledger off)" $
    runRestartScenario defaultTestProfile RequireNoSnapshot

  it "loads snapshot and resumes inserting (ledger on)" $
    -- 'ledgerEnabledTestProfile' lowers the snapshot near-tip
    -- threshold to epoch 2 so snapshots fire on the short fixture
    -- chains; production default of @580@ would mean no snapshot
    -- ever lands during a typical test run.
    runRestartScenario ledgerEnabledTestProfile RequireSnapshot

  it "replays the natural mid-epoch gap without rolling PG back (ledger on)" $
    runMidEpochReplayScenario

-- | Tables whose row counts must survive the restart. They get IDs
-- from PG sequences during Follow and live with the "stale counter"
-- the cleanup bug used to wipe. Pre / post counts must match.
preservedTables :: [Text]
preservedTables = map tdName
  [ blockTableDef
  , slotLeaderTableDef
  , poolHashTableDef
  , txTableDef
  , txOutTableDef
  , addressTableDef
  , stakeAddressTableDef
  ]

data SnapshotRequirement = RequireSnapshot | RequireNoSnapshot

runRestartScenario :: SyncConfig -> SnapshotRequirement -> IO ()
runRestartScenario profile snapReq =
  withMockNode conwayConfigDir $ \mn ->
    withTempDir "dbsync-test-restart" $ \ledgerDir -> do
      tracer <- quietTracer

      -- ~5 slots per block at activeSlotsCoeff=0.2; epoch length 500.
      -- 200 forged blocks → ~1000 slots → crosses an epoch boundary
      -- during Ingest. Follow then advances another 60 blocks →
      -- ~300 more slots, crossing at least one Follow-cadence epoch
      -- and giving the snapshot writer something to persist.
      _ <- forgeAndPushBlocks mn 200

      preCounts <- withAppSession tracer profile mn ledgerDir $ \_ -> do
        waitForSyncComplete 60
        forgeAndWaitForBlocks mn 60 260 60
        traverse countRows preservedTables

      blocksBefore <- case preCounts of
        (n : _) -> pure n
        []      -> panic "preservedTables empty: no block count to compare"
      blocksBefore `shouldSatisfy` (>= 200)

      case snapReq of
        RequireNoSnapshot -> pure ()
        RequireSnapshot -> do
          entries <- listLedgerSnapshots ledgerDir
          entries `shouldSatisfy` (not . null)

      withAppSessionResume tracer profile mn ledgerDir $ \_ ->
        verifyResume mn preCounts blocksBefore

verifyResume :: MockNode -> [Int] -> Int -> IO ()
verifyResume mn preCounts blocksBefore = do
  waitFor "sync_complete remains true on restart" syncCompleteTrue 30

  -- Every preserved table holds its pre-restart count. A failure
  -- here would have been the dedup-counter cleanup wiping rows.
  postRestartCounts <- traverse countRows preservedTables
  zip preservedTables (zip preCounts postRestartCounts)
    `shouldSatisfy` all (\(_, (pre, post)) -> post == pre)

  -- Forge new blocks and confirm Follow advances. A failure here
  -- would be either: (a) the ledger worker crashing on the first
  -- MsgForward because LedgerDB wasn't restored, or (b) Follow's
  -- INSERT path stalling for another reason.
  let target = blocksBefore + 20
  forgeAndWaitForBlocks mn 20 target 60

-- ---------------------------------------------------------------------------
-- * Mid-epoch natural-gap scenario
-- ---------------------------------------------------------------------------

-- | Restart inside the natural \"snapshot lags PG\" window — the
-- usual state at any non-boundary stop with ledger enabled. The
-- snapshot writer fires once per Follow-cadence epoch boundary;
-- between boundaries the consumer keeps advancing
-- @dbsync_sync_state.last_committed_slot@ on each block while the
-- snapshot stays put at the last boundary. The restart path
-- detects the gap, replays it through the ledger worker via the
-- receiver fan-out, and lets the Follow consumer no-op on each
-- replayed block.
runMidEpochReplayScenario :: IO ()
runMidEpochReplayScenario =
  withMockNode conwayConfigDir $ \mn ->
    withTempDir "dbsync-test-restart-mid-epoch" $ \ledgerDir -> do
      firstLogs <- newIORef []
      let firstTracer = mkTestTracer firstLogs

      -- 200 Ingest blocks (≈1000 slots, two epoch boundaries)
      -- + 130 Follow blocks (≈650 slots, crosses at least one
      -- Follow-cadence epoch). Guarantees the Follow snapshot
      -- writer has fired at least once, and the final block sits
      -- strictly mid-epoch so 'last_committed_slot' is ahead of the
      -- newest snapshot.
      _ <- forgeAndPushBlocks mn 200

      (preBlocks, preCounts, lastCommitted, newestSnapshotSlot) <-
        withAppSession firstTracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 60
          forgeAndWaitForBlocks mn 130 330 90
          blockCount    <- countRows (tdName blockTableDef)
          counts        <- traverse countRows preservedTables
          committedSlot <- readLastCommittedSlot
          snapshotSlot  <- newestSnapshotSlotOrFail ledgerDir
          pure (blockCount, counts, committedSlot, snapshotSlot)

      -- Precondition: 'last_committed_slot' strictly ahead of the
      -- newest snapshot. If this ever stops holding (e.g. snapshot
      -- cadence changes), the test is no longer exercising the
      -- target path.
      lastCommitted `shouldSatisfy` (> newestSnapshotSlot)

      secondLogs <- newIORef []
      let secondTracer = mkTestTracer secondLogs

      withAppSessionResume secondTracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
        waitFor "sync_complete remains true on restart" syncCompleteTrue 60

        -- Block count is unchanged across the restart: Follow's
        -- consumer skips its PG-write path inside the replay
        -- window, so committed rows stay put.
        afterRestartBlocks <- countRows (tdName blockTableDef)
        afterRestartBlocks `shouldBe` preBlocks

        afterReSyncSlot <- readLastCommittedSlot
        afterReSyncSlot `shouldBe` lastCommitted

        postCounts <- traverse countRows preservedTables
        postCounts `shouldBe` preCounts

        -- The chain advances normally past the original tip.
        let target = preBlocks + 20
        forgeAndWaitForBlocks mn 20 target 60

      secondMessages <- collectMessages secondLogs

      -- Pin the snapshot-lag log so the test fails if the
      -- gap-handling branch is silently bypassed.
      secondMessages `shouldSatisfy`
        any (T.isInfixOf ("Snapshot lags PG by "
                            <> show (lastCommitted - newestSnapshotSlot)
                            <> " slots"))

      -- No PG rollback line: confirms we don't delete committed rows.
      secondMessages `shouldSatisfy`
        not . any (T.isInfixOf "Rolling back PG from slot")

-- | Read @dbsync_sync_state.last_committed_slot@ as a 'Word64'.
readLastCommittedSlot :: IO Word64
readLastCommittedSlot = do
  raw <- T.strip <$> queryTestDb
    ( "SELECT COALESCE(" <> tableColumn syncStateTableDef "last_committed_slot"
        <> "::text, '') FROM " <> tdName syncStateTableDef <> " LIMIT 1"
    )
  case readMaybe (T.unpack raw) of
    Just n  -> pure n
    Nothing -> panic $ "last_committed_slot was empty / unparseable: " <> raw

-- | Highest snapshot slot under @ledgerDir@. Panics when the
-- directory is empty — the caller relies on at least one Follow
-- snapshot having landed by the time it's invoked.
newestSnapshotSlotOrFail :: FilePath -> IO Word64
newestSnapshotSlotOrFail ledgerDir = do
  entries <- listLedgerSnapshots ledgerDir
  case List.sortBy (flip compare) (mapMaybe readMaybe entries) of
    (s : _) -> pure s
    []      -> panic
      "newestSnapshotSlotOrFail: no Follow snapshot landed during the\
      \ first session; the mid-epoch replay scenario can't be set up.\
      \ The Follow run may need a longer chain to cross a snapshot\
      \ cadence boundary."

-- | Pull the captured log messages out of the test tracer's IORef.
collectMessages :: IORef [LogMsg] -> IO [Text]
collectMessages ref = reverse . map lmMessage <$> readIORef ref
