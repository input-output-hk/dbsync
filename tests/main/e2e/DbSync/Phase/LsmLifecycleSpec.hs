{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end tests for the ingest-phase LSM session lifecycle.
--
-- The session lives at
-- @\<ledger-dir\>/dbsync-ledger/ingest-lsm/@. Rules pinned by
-- these tests:
--
--   * The directory is created when @IngestChainHistory@ opens the
--     session and receives one compaction per epoch boundary.
--   * 'DbSync.Phase.Ingest.UtxoStore.compactUtxoStore' is
--     delete-then-save-then-reopen, so @snapshots/@ holds exactly
--     one entry once any compaction has run, and the @active/@
--     run count is bounded across many boundaries.
--   * 'DbSync.Phase.Ingest.LsmSession.closeAndDeleteLsmSession'
--     removes the whole directory at the end of
--     @PreparingForVolatileTail@.
--   * Mid-Ingest cancellation only runs 'closeLsmSession' (idempotent
--     close), so the directory survives for a resumed boot.
module DbSync.Phase.LsmLifecycleSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.App.Run (runApp)
import DbSync.Db.Schema.EpochSyncStats (epochSyncStatsTableDef)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( defaultTestProfile
  , mkAppArgsFromMockNode
  , newShutdown
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (execTestDb, queryTestDb)
import DbSync.Test.E2E
  ( conwayConfigDir
  , countIngestLsmActiveRuns
  , ingestLsmExists
  , listIngestLsmSnapshots
  , withAppSession
  , withAppSessionResume
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode
  ( MockNode
  , forgeAndPushBlocks
  , forgeAndPushBlocksWith
  , withMockNode
  )
import DbSync.Test.MockNode.Workload
  ( stressWorkload
  , warmupWorkload
  )
import DbSync.Test.PgAssertions (countRows, tableColumn, waitForTableQueryable)
import DbSync.Trace.Types (AppTracer)

-- | Upper bound on @active/@ run count after at least one
-- 'compactUtxoStore'. The compaction collapses to the snapshot's
-- run shape (a handful of runs for the toy Conway test workload);
-- this leaves headroom for in-flight merges started in the
-- subsequent epoch.
maxActiveRunsAfterCompact :: Int
maxActiveRunsAfterCompact = 64

spec :: Spec
spec = describe "Ingest LSM session lifecycle" $ do

  -- Covers invariants 1, 2 and (implicitly) 4: the directory comes
  -- into existence during Ingest, gets removed when Prep completes,
  -- and Follow still boots after the removal (otherwise
  -- 'waitForSyncComplete' / the running app would fall over).
  it "creates ingest-lsm/ during Ingest and deletes it after Prep completes" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-lsm-lifecycle" $ \ledgerDir -> do
        -- Conway test config: 500-slot epochs, ~5 slots/block, k=10.
        -- 150 forged blocks crosses both the epoch boundary and the
        -- rollback horizon so the Ingest → Prep → Follow handoff
        -- runs end-to-end.
        _ <- forgeAndPushBlocks mn 150

        tracer <- quietTracer
        withAppSession tracer defaultTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 60
          -- Sync is complete: 'runPrepAndMarkComplete' has run, and
          -- 'closeAndDeleteLsmSession' fired at the end of it.
          present <- ingestLsmExists ledgerDir
          present `shouldBe` False

  -- Covers invariant 3: 'compactUtxoStore' is delete-then-save, so
  -- the @snapshots/@ subdirectory holds exactly one entry once at
  -- least one compaction has run. The same compaction also reopens
  -- the active table from the snapshot, so the @active/@ run count
  -- stays small even across many boundaries — pinned here at a
  -- generous ceiling to give the merge schedule headroom.
  it "keeps one snapshot and bounded active run count across boundaries" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-lsm-keep-one" $ \ledgerDir -> do
        _ <- forgeAndPushBlocks mn 5000

        tracer <- quietTracer
        runUntilTwoBoundariesThenCancel tracer mn ledgerDir
        snaps <- listIngestLsmSnapshots ledgerDir
        length snaps `shouldBe` 1
        activeRuns <- countIngestLsmActiveRuns ledgerDir
        activeRuns `shouldSatisfy` (<= maxActiveRunsAfterCompact)

  -- Drives real Conway payment txs through Ingest so the UtxoStore
  -- write path actually fires. One warm-up block grows the live
  -- UTxO set from the genesis-default 10 entries up past 100 so
  -- 'stressWorkload' (which spends 100 distinct inputs per block)
  -- has room to operate. The subsequent stress run produces ~10K
  -- distinct tx hashes, exercising 'recordTx' on every one;
  -- 'compactUtxoStore' at each epoch boundary still has to keep
  -- the active run count bounded.
  --
  -- Block count is sized to stay well within the genesis lovelace
  -- supply once minimum fees are deducted on every spend.
  it "keeps active run count bounded under a stress payment workload" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-lsm-stress" $ \ledgerDir -> do
        _ <- forgeAndPushBlocksWith mn 1 warmupWorkload
        _ <- forgeAndPushBlocksWith mn 100 stressWorkload
        -- Empty filler so the consumer crosses ≥ 2 epoch boundaries
        -- and 'compactUtxoStore' has actually fired at least twice
        -- against the prior stress workload's tx writes.
        _ <- forgeAndPushBlocks mn 5000

        tracer <- quietTracer
        runUntilTwoBoundariesThenCancel tracer mn ledgerDir
        activeRuns <- countIngestLsmActiveRuns ledgerDir
        activeRuns `shouldSatisfy` (<= maxActiveRunsAfterCompact)

  -- Covers invariants 1 and 5: a mid-Ingest cancellation runs only
  -- 'closeLsmSession' (idempotent close), so the directory survives
  -- for the resumed boot. The second-leg Prep then removes it as in
  -- the happy path.
  it "preserves ingest-lsm/ across a mid-Ingest crash and removes it after the resumed Prep" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-lsm-restart" $ \ledgerDir -> do
        _ <- forgeAndPushBlocks mn 5000

        tracer <- quietTracer
        runUntilLsmDirExistsThenCancel tracer mn ledgerDir

        afterCancel <- ingestLsmExists ledgerDir
        afterCancel `shouldBe` True

        withAppSessionResume tracer defaultTestProfile mn ledgerDir $ \_ ->
          waitForSyncComplete 120

        finalPresent <- ingestLsmExists ledgerDir
        finalPresent `shouldBe` False

-- ---------------------------------------------------------------------------
-- Cancellation helpers
-- ---------------------------------------------------------------------------

-- | Start a sync, wait until the LSM directory appears on disk,
-- then cancel. Returns after the app has fully exited so the
-- caller sees the post-cancel filesystem state.
--
-- @openLsmSession@ runs before the consumer processes any block,
-- so the wait resolves well before the consumer can reach Prep.
runUntilLsmDirExistsThenCancel :: AppTracer -> MockNode -> FilePath -> IO ()
runUntilLsmDirExistsThenCancel tracer mn ledgerDir = do
  clearSyncCompleteFlag
  (_, waitSig) <- newShutdown
  let args = mkAppArgsFromMockNode defaultTestProfile mn ledgerDir (Just waitSig)
  withAsync (runApp tracer args) $ \app -> do
    waitFor "ingest-lsm/ dir to appear" (ingestLsmExists ledgerDir) 30
    cancel app

-- | Start a sync, wait until @sync_state@ records two completed
-- epoch boundaries, then cancel.
--
-- The async is intentionally not 'link'ed so the 'AsyncCancelled'
-- raised by 'cancel' stays inside the app instead of re-throwing
-- into the test thread.
runUntilTwoBoundariesThenCancel :: AppTracer -> MockNode -> FilePath -> IO ()
runUntilTwoBoundariesThenCancel tracer mn ledgerDir = do
  clearSyncCompleteFlag
  (_, waitSig) <- newShutdown
  let args = mkAppArgsFromMockNode defaultTestProfile mn ledgerDir (Just waitSig)
  withAsync (runApp tracer args) $ \app -> do
    waitFor "≥ 2 epoch_sync_stats rows AND last_committed_slot set"
      ((&&) <$> twoEpochSyncStatsRows <*> lastCommittedSlotSet)
      60
    waitForTableQueryable (tdName epochSyncStatsTableDef) 30
    cancel app

-- ---------------------------------------------------------------------------
-- PG predicates
-- ---------------------------------------------------------------------------

-- | Clear a stale @sync_complete=true@ flag from a prior run. The
-- standard 'withAppSession' bracket does this; sessions built by
-- hand need to do it too.
clearSyncCompleteFlag :: IO ()
clearSyncCompleteFlag =
  execTestDb
    ( "UPDATE " <> tdName syncStateTableDef
        <> " SET " <> tableColumn syncStateTableDef "sync_complete" <> " = false"
        <> " WHERE " <> tableColumn syncStateTableDef "id" <> " = 1"
    )
    `catch` \(_ :: SomeException) -> pure ()

lastCommittedSlotSet :: IO Bool
lastCommittedSlotSet = do
  raw <- ( T.strip <$> queryTestDb
    ( "SELECT COALESCE(" <> tableColumn syncStateTableDef "last_committed_slot"
        <> "::text, '') FROM " <> tdName syncStateTableDef <> " LIMIT 1"
    )) `catch` \(_ :: SomeException) -> pure ""
  pure (not (T.null raw))

twoEpochSyncStatsRows :: IO Bool
twoEpochSyncStatsRows = do
  n <- countRows (tdName epochSyncStatsTableDef)
    `catch` \(_ :: SomeException) -> pure 0
  pure (n >= 2)
