{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Two mid-Ingest restart hazards, both on the 'IngestResume'
-- cleanup path.
--
--   * @epoch_sync_stats@ must not double up its ids. @sync_state@'s
--     @*_id_counter@ columns are written one boundary behind the rows
--     that 'lsCommit' has already flushed. Tables that carry neither
--     @slot_no@ nor @block_id@ and aren't in the dedup-counter list
--     slip through the cleanup with their lagging row intact; the next
--     boundary's COPY then re-allocates an existing id and Prep fails
--     on the @PRIMARY KEY (id)@ build.
--   * The epoch-keyed tables must keep the rows already-committed
--     blocks produced. They carry no slot, block or tx anchor, so the
--     cleanup can only scope them by epoch, and blocks at or below
--     @last_committed_slot@ are never re-processed.
module DbSync.Phase.IngestRestartSpec (spec) where

import Cardano.Prelude

import Data.List (lookup)
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.App.Config.Types (DbProfile (..), OptionFlag (..), SyncConfig (..))
import DbSync.App.Run (runApp)
import DbSync.Db.Schema.EpochSyncStats (epochSyncStatsTableDef)
import DbSync.Db.Schema.StakeDelegation (epochStakeTableDef)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( configTableDefs
  , defaultTestConfig
  , ledgerEnabledTestConfig
  , mkAppArgsFromMockNode
  , newShutdown
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )

import DbSync.Test.Database (execTestDb, queryTestDb, teardownSchema)
import DbSync.Test.E2E
  ( conwayConfigDir
  , conwayRewardsConfigDir
  , listLedgerSnapshots
  , syncCompleteTrue
  , withAppSessionResume
  )
import DbSync.Test.EpochRegression
  ( EpochSnapshot
  , completedStakeEpochs
  , epochRegressions
  , snapshotEpochKeyedCounts
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode (MockNode, forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions
  ( countRows
  , readInt
  , tableColumn
  , waitForTableQueryable
  )
import DbSync.Trace.Types (AppTracer)

spec :: Spec
spec = describe "IngestChainHistory restart" $ do
  it "does not duplicate epoch_sync_stats ids on resume" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-ingest-restart" $ \ledgerDir -> do
        tracer <- quietTracer

        -- Conway test config: 500-slot epochs, ~5 slots/block, k=10.
        -- Enough chain that the consumer can't reach @tip − k@
        -- before we cancel: if Prep runs, sync_complete flips true
        -- and the resume boot takes the Follow-restart path that
        -- skips the cleanup we're exercising.
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
        withAppSessionResume tracer defaultTestConfig mn ledgerDir $ \_ ->
          waitForSyncComplete 90

        waitForTableQueryable (tdName epochSyncStatsTableDef) 30
        duplicates <- T.strip <$> queryTestDb
          ( "SELECT COUNT(*) FROM ("
              <> " SELECT id FROM " <> tdName epochSyncStatsTableDef
              <> " GROUP BY id HAVING COUNT(*) > 1"
              <> ") d;"
          )
        duplicates `shouldBe` "0"

  it "keeps the epoch-keyed rows already-committed blocks produced" $
    withMockNode conwayRewardsConfigDir $ \mn ->
      withTempDir "dbsync-test-ingest-restart-epoch" $ \ledgerDir -> do
        tracer <- quietTracer

        -- Same 5000-block reasoning as above: the consumer must still
        -- be mid-Ingest at the cancel point, so the resume takes the
        -- 'IngestResume' cleanup path rather than the Follow one.
        _ <- forgeAndPushBlocks mn 5000

        (preCounts, preCompleted) <- runMidIngestEpochSession tracer mn ledgerDir

        -- Without several committed epochs in the snapshot the
        -- comparison below would pass vacuously.
        stakeEpochsIn preCounts `shouldSatisfy` (>= 2)

        withAppSessionResume tracer stakeLedgerConfig mn ledgerDir $ \_ ->
          waitForSyncComplete 180

        -- The resume re-streams every block above
        -- @last_committed_slot@, so an epoch may legitimately gain
        -- rows. It may never lose them: the cleanup deleting an epoch
        -- that only already-committed blocks produced leaves it short
        -- for the life of the database.
        postCounts <- snapshotEpochKeyedCounts
        epochRegressions preCounts postCounts `shouldBe` []

        postCompleted <- completedStakeEpochs
        filter (`notElem` postCompleted) preCompleted `shouldBe` []

-- | Ledger on so the boundary handler runs, @stake_delegation_ledger@
-- on so it writes @epoch_stake@ / @reward@ / @pot_reward@.
stakeLedgerConfig :: SyncConfig
stakeLedgerConfig = ledgerEnabledTestConfig
  { scDbProfile = (scDbProfile ledgerEnabledTestConfig)
      { pcStakeDelegationLedger = OptionFlag True
      }
  }

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
  let args = mkAppArgsFromMockNode defaultTestConfig mn ledgerDir (Just waitSig)
  withAsync (runApp tracer args) $ \app -> do
    waitFor "≥ 2 epoch_sync_stats rows AND last_committed_slot set"
      ((&&) <$> twoEpochSyncStatsRows <*> lastCommittedSlotSet)
      60

    -- If Prep already ran the resume boot would take the
    -- Follow-restart path and the bug never surfaces. Fail loud
    -- rather than silent.
    complete <- syncCompleteTrue
    complete `shouldBe` False

    waitForTableQueryable (tdName epochSyncStatsTableDef) 30
    n <- countRows (tdName epochSyncStatsTableDef)
    cancel app
    pure n

-- | Sync until @epoch_stake@ covers two epochs, then cancel. Returns
-- the epoch-keyed counts and the completed-progress epochs as they
-- stood at the cancel point.
runMidIngestEpochSession
  :: AppTracer -> MockNode -> FilePath -> IO (EpochSnapshot, [Word64])
runMidIngestEpochSession tracer mn ledgerDir = do
  -- Drop before starting, not just clear the sync-complete flag: the
  -- poll below would otherwise satisfy itself from the previous spec's
  -- rows and snapshot them, long before this session has written
  -- anything.
  teardownSchema (configTableDefs stakeLedgerConfig)
  clearSyncCompleteFlag
  (_, waitSig) <- newShutdown
  let args = mkAppArgsFromMockNode stakeLedgerConfig mn ledgerDir (Just waitSig)
  withAsync (runApp tracer args) $ \app -> do
    waitFor "epoch_stake covers >= 2 epochs AND a resumable snapshot exists"
      (andM [twoStakeEpochs, resumableSnapshotExists ledgerDir])
      240

    complete <- syncCompleteTrue
    complete `shouldBe` False

    counts    <- snapshotEpochKeyedCounts
    completed <- completedStakeEpochs
    cancel app
    pure (counts, completed)

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

-- | Cheap poll predicate — one query rather than the full snapshot,
-- which shells out once per epoch-keyed table.
twoStakeEpochs :: IO Bool
twoStakeEpochs = do
  n <- readInt
    ( "SELECT count(DISTINCT " <> tableColumn epochStakeTableDef "epoch_no"
        <> ") FROM " <> tdName epochStakeTableDef
    ) `catch` \(_ :: SomeException) -> pure 0
  pure (n >= 2)

-- | Distinct epochs carrying @epoch_stake@ rows in a snapshot.
stakeEpochsIn :: EpochSnapshot -> Int
stakeEpochsIn snap =
  length (fromMaybe [] (lookup (tdName epochStakeTableDef) snap))

-- | Whether a ledger-enabled resume is possible yet: boot needs a
-- snapshot at or before @last_committed_slot@ to replay forward from.
--
-- During Ingest the ledger worker runs ahead of the PG consumer, so the
-- earliest snapshots land /above/ the committed slot and cancelling
-- there yields a database boot refuses to resume. The run has to
-- continue until the consumer has passed one of them.
resumableSnapshotExists :: FilePath -> IO Bool
resumableSnapshotExists ledgerDir = do
  mCommitted <- readCommittedSlot
  case mCommitted of
    Nothing -> pure False
    Just committed -> do
      slots <- mapMaybe readMaybe <$> listLedgerSnapshots ledgerDir
      pure (any (<= committed) (slots :: [Word64]))

readCommittedSlot :: IO (Maybe Word64)
readCommittedSlot = do
  raw <- ( T.strip <$> queryTestDb
    ( "SELECT COALESCE(" <> tableColumn syncStateTableDef "last_committed_slot"
        <> "::text, '') FROM " <> tdName syncStateTableDef <> " LIMIT 1"
    )) `catch` \(_ :: SomeException) -> pure ""
  pure (readMaybe (T.unpack raw))

andM :: Monad m => [m Bool] -> m Bool
andM = foldM (\acc act -> if acc then act else pure False) True
