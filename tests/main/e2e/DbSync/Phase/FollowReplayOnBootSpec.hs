{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Boot the Follow restart path against a database that has
-- committed past the latest on-disk ledger snapshot, and assert both
-- invariants of the replay window in a single restart:
--
--   * Count-preservation: committed rows are never rolled back. The
--     snapshot writer is asynchronous, so on shutdown the on-disk
--     snapshot can lag the consumer\'s last PG commit. On the next
--     boot the Follow restart path picks the newest snapshot whose
--     slot has a matching @block.hash@ in PG, loads it into the
--     in-memory @LedgerDB@, and configures a replay window with
--     @last_committed_slot@ as the upper edge. The ledger worker
--     re-applies the gap while Follow\'s consumer skips its PG-write
--     path (the rows are already in PG), so block/dedup counts and
--     @last_committed_slot@ stay put across the restart.
--   * Flip-ordering: the phase must not flip to 'FollowingChainTip'
--     inside the replay window. During replay-skip the consumer
--     drains the queue far ahead of the back-pressured ledger worker,
--     so the naive \"caught up\" predicate holds spuriously.
--     'maybeFlipToTip' must ignore that state while @slot <= bootSlot@;
--     the flip may only fire once replay has completed.
--
-- The deterministic gap is engineered by deleting the newest
-- snapshot\'s header + LSM directory between the two sessions; the
-- next-newest survivor becomes the chosen restart point and is
-- strictly below @last_committed_slot@.
module DbSync.Phase.FollowReplayOnBootSpec (spec) where

import Cardano.Prelude

import qualified Data.List as List
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef)
import System.Directory (doesDirectoryExist, doesPathExist, listDirectory, removePathForcibly)
import System.FilePath ((</>))

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef)
import DbSync.Db.Schema.Core (poolHashTableDef, stakeAddressTableDef)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Trace.Backend (mkTestTracer)
import DbSync.Trace.Types (LogMsg (..))
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
spec = describe "FollowingChainTip restart replay on boot" $
  it "replays the snapshot-to-PG gap without rolling PG back and defers the tip flip until replay completes" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-replay-on-boot" $ \ledgerDir -> do
        firstLogs <- newIORef []
        let firstTracer = mkTestTracer firstLogs

        -- ~5 slots per block at activeSlotsCoeff=0.2, epoch length
        -- 500, k=10. 400 forged blocks gives Ingest enough chain to
        -- exit naturally at tip-k and run Prep, but Ingest itself
        -- never writes a snapshot — 'shouldSnapshotAtEpoch' restricts
        -- the Ingest cadence to epochs divisible by 10. Snapshots
        -- here therefore have to come from Follow, which writes one
        -- per epoch boundary at this profile's near-tip threshold
        -- of 2.
        _ <- forgeAndPushBlocks mn 400

        (preBlocks, preDedupCounts, lastCommitted, snapshotsAfterFirst) <-
          withAppSession firstTracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
            waitForSyncComplete 90
            -- 250 Follow blocks ≈ 1300 slots, enough to cross two
            -- epoch boundaries (~slot 2500, ~slot 3000) and so write
            -- two snapshots; the gap-engineering step below removes
            -- the newer one to drive 'S < L'.
            forgeAndWaitForBlocks mn 250 650 90
            blockCount       <- countRows (tdName blockTableDef)
            dedupCounts      <- traverse countRows dedupTables
            committedSlot    <- readLastCommittedSlot
            snapshotEntries  <- listLedgerSnapshots ledgerDir
            pure (blockCount, dedupCounts, committedSlot, snapshotEntries)

        snapshotsAfterFirst `shouldSatisfy` (\xs -> length xs >= 2)

        -- Force `S < L`: drop the newest snapshot from disk so the
        -- boot logic picks the next-newest, whose slot is strictly
        -- below 'last_committed_slot'. This mimics the production
        -- failure mode where the snapshot writer was killed before
        -- catching up to the consumer's commits.
        deletedSlot <- removeNewestSnapshot ledgerDir
        remaining   <- listLedgerSnapshots ledgerDir
        chosenSlot  <- case List.sortBy (flip compare) (mapMaybe parseSnapshotSlot remaining) of
          (s : _) -> pure s
          []      -> panic "removeNewestSnapshot left no surviving snapshot"
        deletedSlot `shouldSatisfy` (> chosenSlot)
        chosenSlot  `shouldSatisfy` (< lastCommitted)

        secondLogs <- newIORef []
        let secondTracer = mkTestTracer secondLogs

        withAppSessionResume secondTracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
          waitFor "sync_complete remains true on restart" syncCompleteTrue 60

          -- Count-preservation. Read before any new block streams: the
          -- chain tip is still at the recorded commit, so the consumer
          -- is replaying inside the window with its PG-write path
          -- skipped. Committed block/dedup rows and last_committed_slot
          -- must be exactly as the previous session left them.
          afterRestartBlocks <- countRows (tdName blockTableDef)
          afterRestartBlocks `shouldBe` preBlocks

          afterReSyncSlot <- readLastCommittedSlot
          afterReSyncSlot `shouldBe` lastCommitted

          postDedupCounts <- traverse countRows dedupTables
          postDedupCounts `shouldBe` preDedupCounts

          -- Drive the ledger past 'last_committed_slot' so replay
          -- finishes. These blocks are above the replay window, so
          -- Follow writes them and PG advances.
          let interSessionBlocks = 20 :: Int
              postRestartBlocks  = 10 :: Int
              replayTarget       = preBlocks + interSessionBlocks
          forgeAndWaitForBlocks mn interSessionBlocks replayTarget 90
          waitForLogMatch secondLogs "ledger replay completes" isReplayComplete 60

          -- Push the consumer to the live tip so the legitimate flip
          -- to FollowingChainTip fires.
          let target = replayTarget + postRestartBlocks
          forgeAndWaitForBlocks mn postRestartBlocks target 60
          waitForLogMatch secondLogs "post-replay flip to FollowingChainTip" isFlipToChainTip 30

          finalBlocks <- countRows (tdName blockTableDef)
          finalBlocks `shouldSatisfy` (>= target)

        secondMsgs <- collectChronological secondLogs
        let secondTexts = map lmMessage secondMsgs

        -- The gap-handling branch was actually exercised: the chosen
        -- (next-newest) snapshot loaded and the snapshot-lag line
        -- named the exact gap.
        secondTexts `shouldSatisfy`
          any (T.isInfixOf ("Loading ledger snapshot at slot " <> show chosenSlot))
        secondTexts `shouldSatisfy`
          any (T.isInfixOf ("Snapshot lags PG by "
                              <> show (lastCommitted - chosenSlot)
                              <> " slots"))

        -- No PG rollback: a "Rolling back PG from slot" line would mean
        -- committed rows were deleted.
        secondTexts `shouldSatisfy`
          not . any (T.isInfixOf "Rolling back PG from slot")

        -- Flip-ordering: the FollowingVolatileTail -> FollowingChainTip
        -- transition must come strictly after replay completed. The
        -- pre-fix bug flipped on the very first replay-skip block,
        -- which inverts the order.
        let replayIx = List.findIndex isReplayComplete secondMsgs
            flipIx   = List.findIndex isFlipToChainTip secondMsgs
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

-- | Tables whose row counts must survive the restart. Dedup tables
-- (content-keyed) and the slot-keyed @block@ table; none of them ever
-- loses a row under the replay path.
dedupTables :: [Text]
dedupTables = map tdName
  [ addressTableDef
  , slotLeaderTableDef
  , poolHashTableDef
  , stakeAddressTableDef
  ]

-- | Read @dbsync_sync_state.last_committed_slot@ as a 'Word64'. Panics
-- on NULL — the row is seeded right after @sync_complete = true@ is
-- written, and the test only reads it after that.
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

-- | Delete the snapshot at the highest slot found under
-- @ledgerDir\/dbsync-ledger@. Removes both halves of the on-disk
-- representation: the @snapshot-headers\/\<slot\>@ entry (consulted by
-- 'listSnapshots') and the @lsm\/snapshots\/\<slot\>@ entry (consulted
-- by the LSM backend on load).
--
-- Returns the slot number that was deleted so the caller can assert
-- the next-newest survives and is strictly below the test\'s
-- 'last_committed_slot'.
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
      -- The LSM dir is best-effort: if the layout differs across
      -- backend versions, the header removal alone is enough for
      -- 'listSnapshots' to forget the snapshot.
      lsmExists <- doesPathExist lsmDataPath
      when lsmExists $ removePathForcibly lsmDataPath
      pure s

-- | Return captured 'LogMsg's in chronological order. The tracer
-- prepends each new message to the head of the list, so we reverse on
-- read — useful when scanning for two related markers that should
-- appear in a known order.
collectChronological :: IORef [LogMsg] -> IO [LogMsg]
collectChronological ref = reverse <$> readIORef ref
