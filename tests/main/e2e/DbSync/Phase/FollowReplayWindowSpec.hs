{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Restart Follow against a stale ledger snapshot and assert the
-- phase does not flip to 'FollowingChainTip' inside the replay
-- window.
--
-- During replay-skip the consumer drains the block queue much faster
-- than the receiver (which is back-pressured by the slower ledger
-- worker). The naive flip predicate (queue empty, applied slot
-- caught the receiver's latest received slot) holds spuriously after
-- only a handful of blocks, even though hundreds-of-thousands of
-- ledger blocks may still need to replay. 'maybeFlipToTip' must
-- ignore that apparent "caught up" state while @slot <= bootSlot@.
module DbSync.Phase.FollowReplayWindowSpec (spec) where

import Cardano.Prelude

import qualified Data.List as List
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef)
import System.Directory (doesDirectoryExist, doesPathExist, listDirectory, removePathForcibly)
import System.FilePath ((</>))

import Test.Hspec (Spec, describe, expectationFailure, it, shouldSatisfy)

import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Trace.Backend (mkTestTracer)
import DbSync.Trace.Types (AppTracer, LogMsg (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestProfile
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E
  ( conwayConfigDir
  , forgeAndWaitForBlocks
  , listLedgerSnapshots
  , syncCompleteTrue
  , waitForLogMatch
  , withAppSession
  , withAppSessionResume
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode (forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions (countRows, tableColumn)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "FollowingVolatileTail replay-window phase flip" $
  it "defers the FollowingChainTip flip until ledger replay completes" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-replay-window" $ \ledgerDir -> do

        firstLogs <- newIORef []
        let firstTracer = mkTestTracer firstLogs :: AppTracer

        -- Same chain shape as FollowReplayOnBootSpec: enough Ingest
        -- blocks to exit naturally into Prep, then enough Follow
        -- blocks to write at least two snapshots so removing the
        -- newest still leaves a survivor below 'last_committed_slot'.
        _ <- forgeAndPushBlocks mn 400

        (preBlocks, lastCommitted, snapshotsAfterFirst) <-
          withAppSession firstTracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
            waitForSyncComplete 90
            forgeAndWaitForBlocks mn 250 650 90
            blockCount      <- countRows (tdName blockTableDef)
            committedSlot   <- readLastCommittedSlot
            snapshotEntries <- listLedgerSnapshots ledgerDir
            pure (blockCount, committedSlot, snapshotEntries)

        snapshotsAfterFirst `shouldSatisfy` (\xs -> length xs >= 2)

        -- Forge a small post-commit tail on the mock node while
        -- dbsync is stopped. On restart this is what gives the
        -- receiver something past 'last_committed_slot' to stream —
        -- without it the chainsync stream stalls at the recorded
        -- tip and 'replay complete' never fires.
        let interSessionBlocks = 20 :: Int
        _ <- forgeAndPushBlocks mn interSessionBlocks

        deletedSlot <- removeNewestSnapshot ledgerDir
        remaining   <- listLedgerSnapshots ledgerDir
        chosenSlot  <- case List.sortBy (flip compare) (mapMaybe parseSnapshotSlot remaining) of
          (s : _) -> pure s
          []      -> panic "removeNewestSnapshot left no surviving snapshot"
        deletedSlot `shouldSatisfy` (> chosenSlot)
        chosenSlot  `shouldSatisfy` (< lastCommitted)

        secondLogs <- newIORef []
        let secondTracer = mkTestTracer secondLogs :: AppTracer

        withAppSessionResume secondTracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
          waitFor "sync_complete remains true on restart" syncCompleteTrue 60

          -- Block until the ledger has replayed past
          -- 'last_committed_slot'. The fix's invariant is that no
          -- phase flip happens before this line fires.
          waitForLogMatch secondLogs "ledger replay completes"
            isReplayComplete
            60

          -- Push the consumer to tip on the post-replay chain so
          -- the queue drains and the (legitimate) phase flip fires.
          let postRestartBlocks = 10 :: Int
              target = preBlocks + interSessionBlocks + postRestartBlocks
          forgeAndWaitForBlocks mn postRestartBlocks target 60

          waitForLogMatch secondLogs "post-replay flip to FollowingChainTip"
            isFlipToChainTip
            30

          -- Ordering invariant: the first
          -- 'FollowingVolatileTail -> FollowingChainTip' transition
          -- must come strictly after the 'replay complete' line. The
          -- pre-fix bug produced a flip on the very first replay-skip
          -- block, which inverts the order.
          msgs <- collectChronological secondLogs
          let replayIx = List.findIndex isReplayComplete msgs
              flipIx   = List.findIndex isFlipToChainTip msgs
          case (replayIx, flipIx) of
            (Just rc, Just fl) ->
              fl `shouldSatisfy` (> rc)
            (Just _, Nothing) ->
              expectationFailure
                "captured no FollowingChainTip flip after replay completed"
            (Nothing, _) ->
              expectationFailure
                "captured no LedgerReplay completion line"

-- ---------------------------------------------------------------------------
-- * Log predicates
-- ---------------------------------------------------------------------------

isReplayComplete :: LogMsg -> Bool
isReplayComplete m =
  lmComponent m == "LedgerReplay"
    && T.isPrefixOf "replay complete" (lmMessage m)

isFlipToChainTip :: LogMsg -> Bool
isFlipToChainTip m =
  lmComponent m == "Phase"
    && T.isInfixOf "FollowingVolatileTail -> FollowingChainTip" (lmMessage m)

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Read @dbsync_sync_state.last_committed_slot@. Panics on NULL —
-- the row is seeded before sync_complete flips to true and the test
-- only reads it after that.
readLastCommittedSlot :: IO Word64
readLastCommittedSlot = do
  raw <- T.strip <$> queryTestDb
    ( "SELECT COALESCE(" <> tableColumn syncStateTableDef "last_committed_slot"
        <> "::text, '') FROM " <> tdName syncStateTableDef <> " LIMIT 1"
    )
  case readMaybe (T.unpack raw) of
    Just n  -> pure n
    Nothing -> panic $ "last_committed_slot was empty / unparseable: " <> raw

-- | Snapshot directories are named by their slot number ('Word64').
parseSnapshotSlot :: FilePath -> Maybe Word64
parseSnapshotSlot = readMaybe

-- | Delete the highest-slot snapshot on disk so the next boot picks
-- the next-newest survivor, forcing the snapshot-lags-PG branch.
-- Returns the deleted slot.
removeNewestSnapshot :: FilePath -> IO Word64
removeNewestSnapshot ledgerDir = do
  let root        = ledgerDir </> "dbsync-ledger"
      headersDir  = root </> "snapshot-headers"
      lsmSnapsDir = root </> "lsm" </> "snapshots"
  entries <- doesDirectoryExist headersDir >>= \case
    True  -> listDirectory headersDir
    False -> pure []
  let slots = List.sortBy (flip compare) (mapMaybe parseSnapshotSlot entries)
  case slots of
    []    -> panic "removeNewestSnapshot: snapshot-headers/ is empty"
    s : _ -> do
      let slotStr     = show s
          headerPath  = headersDir  </> slotStr
          lsmDataPath = lsmSnapsDir </> slotStr
      removePathForcibly headerPath
      lsmExists <- doesPathExist lsmDataPath
      when lsmExists $ removePathForcibly lsmDataPath
      pure s

-- | Return captured 'LogMsg's in chronological order. The tracer
-- prepends, so we reverse on read.
collectChronological :: IORef [LogMsg] -> IO [LogMsg]
collectChronological ref = reverse <$> readIORef ref
