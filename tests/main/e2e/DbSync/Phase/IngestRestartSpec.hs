{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A mid-Ingest restart must not double-up @epoch_sync_stats@ rows.
--
-- @sync_state@'s @*_id_counter@ columns are written one boundary
-- behind the rows that 'lsCommit' has already flushed. Tables that
-- carry neither @slot_no@ nor @block_id@ and aren't in the
-- dedup-counter list — @epoch_sync_stats@ here — slip through the
-- resume cleanup with their lagging row intact. The next boundary's
-- COPY then re-allocates an id that already exists and Prep fails
-- on the @PRIMARY KEY (id)@ build.
module DbSync.Phase.IngestRestartSpec (spec) where

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
import DbSync.Test.E2E (conwayConfigDir, syncCompleteTrue, withAppSessionResume)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode (MockNode, forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions (countRows, tableColumn, waitForTableQueryable)
import DbSync.Trace.Types (AppTracer)

spec :: Spec
spec = describe "IngestChainHistory restart" $
  it "does not duplicate epoch_sync_stats ids on resume" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-ingest-restart" $ \ledgerDir -> do
        tracer <- quietTracer

        -- Conway test config: 500-slot epochs, ~5 slots/block, k=10.
        -- Enough chain that the consumer can't reach @tip − k@
        -- before we cancel: if Prep runs, sync_complete flips true
        -- and the resume boot takes the fast path that skips the
        -- cleanup we're exercising.
        _ <- forgeAndPushBlocks mn 5000

        midRows <- runMidIngestSession tracer mn ledgerDir
        midRows `shouldSatisfy` (>= 2)

        -- Standard resume bracket. With the bug live this throws
        -- inside Prep when it tries to add the @PRIMARY KEY (id)@
        -- index on @epoch_sync_stats@ ("Key (id)=(N) is
        -- duplicated"); the exception propagates through the linked
        -- async and the test fails with that message. With the
        -- cleanup fixed, Prep succeeds and we fall through to the
        -- @duplicates@ assertion.
        withAppSessionResume tracer defaultTestProfile mn ledgerDir $ \_ ->
          waitForSyncComplete 90

        waitForTableQueryable (tdName epochSyncStatsTableDef) 30
        duplicates <- T.strip <$> queryTestDb
          ( "SELECT COUNT(*) FROM ("
              <> " SELECT id FROM " <> tdName epochSyncStatsTableDef
              <> " GROUP BY id HAVING COUNT(*) > 1"
              <> ") d;"
          )
        duplicates `shouldBe` "0"

-- ---------------------------------------------------------------------------
-- Session 1: stop mid-Ingest
-- ---------------------------------------------------------------------------

-- | Start a fresh sync, wait until two epoch boundaries have
-- committed (so @sync_state@ carries the one-boundary lag), then
-- 'cancel' the @runApp@ async. Returns the @epoch_sync_stats@ count
-- at the cancel point.
--
-- The async is deliberately not 'link'ed: 'cancel' raises
-- 'AsyncCancelled' inside @runApp@, and a 'link' would re-throw it
-- into the test thread.
runMidIngestSession :: AppTracer -> MockNode -> FilePath -> IO Int
runMidIngestSession tracer mn ledgerDir = do
  clearSyncCompleteFlag
  (_, waitSig) <- newShutdown
  let args = mkAppArgsFromMockNode defaultTestProfile mn ledgerDir (Just waitSig)
  withAsync (runApp tracer args) $ \app -> do
    waitFor "≥ 2 epoch_sync_stats rows AND last_committed_slot set"
      ((&&) <$> twoEpochSyncStatsRows <*> lastCommittedSlotSet)
      60

    -- If Prep already ran the resume boot would take the fast path
    -- and the bug never surfaces. Fail loud rather than silent.
    complete <- syncCompleteTrue
    complete `shouldBe` False

    waitForTableQueryable (tdName epochSyncStatsTableDef) 30
    n <- countRows (tdName epochSyncStatsTableDef)
    cancel app
    pure n

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
