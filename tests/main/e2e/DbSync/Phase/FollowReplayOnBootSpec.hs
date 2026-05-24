{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Boot the Follow restart path against a database that has
-- committed past the latest on-disk ledger snapshot.
--
-- The snapshot writer is asynchronous; on any shutdown the on-disk
-- snapshot can lag the consumer\'s last PG commit. On the next boot
-- the Follow restart path:
--
--   * Picks the newest snapshot whose slot has a matching
--     @block.hash@ in PG.
--   * Loads it into the in-memory @LedgerDB@.
--   * Configures a replay window with @last_committed_slot@ as the
--     upper edge — the ledger worker re-applies the gap from the
--     receiver\'s fan-out while Follow\'s consumer skips its
--     PG-write path (the rows are already in PG from the previous
--     run).
--   * Intersects chainsync at the snapshot\'s point; the protocol
--     streams each block from @snap_slot + 1@ forward.
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

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef)
import DbSync.Db.Schema.Pool (poolHashTableDef)
import DbSync.Db.Schema.StakeDelegation (stakeAddressTableDef)
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
  , withAppSession
  , withAppSessionResume
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode (forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions (countRows, tableColumn)

spec :: Spec
spec = describe "FollowingChainTip restart replay on boot" $
  it "replays the snapshot-to-PG gap through the ledger worker without rolling PG back" $
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

          -- Block count is unchanged across the restart: Follow's
          -- consumer skips its PG-write path for blocks inside the
          -- replay window, so committed rows stay put.
          afterRestartBlocks <- countRows (tdName blockTableDef)
          afterRestartBlocks `shouldBe` preBlocks

          afterReSyncSlot <- readLastCommittedSlot
          afterReSyncSlot `shouldBe` lastCommitted

          postDedupCounts <- traverse countRows dedupTables
          postDedupCounts `shouldBe` preDedupCounts

          -- Forging past the original tip continues to advance PG.
          let target = preBlocks + 20
          forgeAndWaitForBlocks mn 20 target 90

          finalBlocks <- countRows (tdName blockTableDef)
          finalBlocks `shouldSatisfy` (>= target)

        secondMessages <- collectMessages secondLogs

        -- Pin the chosen snapshot and the snapshot-lag log line so
        -- the test fails if the gap-handling branch isn't actually
        -- exercised.
        secondMessages `shouldSatisfy`
          any (T.isInfixOf ("Loading ledger snapshot at slot " <> show chosenSlot))
        secondMessages `shouldSatisfy`
          any (T.isInfixOf ("Snapshot lags PG by "
                              <> show (lastCommitted - chosenSlot)
                              <> " slots"))

        -- And confirm no PG rollback is performed: a "Rolling back
        -- PG from slot" line would mean we'd deleted committed rows.
        secondMessages `shouldSatisfy`
          not . any (T.isInfixOf "Rolling back PG from slot")

-- | Tables whose row counts must survive the restart. Dedup tables
-- (content-keyed) and the slot-keyed @block@ table; none of them
-- ever loses a row under the replay path.
dedupTables :: [Text]
dedupTables = map tdName
  [ addressTableDef
  , slotLeaderTableDef
  , poolHashTableDef
  , stakeAddressTableDef
  ]

-- | Read @dbsync_sync_state.last_committed_slot@ as a 'Word64'.
-- Panics on the unexpected case where the column is NULL (the row
-- is seeded right after 'sync_complete = true' is written, and the
-- test only reads it after that).
readLastCommittedSlot :: IO Word64
readLastCommittedSlot = do
  raw <- T.strip <$> queryTestDb
    ( "SELECT COALESCE(" <> tableColumn syncStateTableDef "last_committed_slot"
        <> "::text, '') FROM " <> tdName syncStateTableDef <> " LIMIT 1"
    )
  case readMaybe (T.unpack raw) of
    Just n  -> pure n
    Nothing -> panic $ "last_committed_slot was empty / unparseable: " <> raw

-- | Parse a snapshot directory name into its slot number. Consensus
-- writes header dirs named by @dsNumber@ (a 'Word64'); 'Nothing'
-- for any entry that doesn't decode as a number.
parseSnapshotSlot :: FilePath -> Maybe Word64
parseSnapshotSlot = readMaybe

-- | Delete the snapshot at the highest slot found under
-- @ledgerDir\/dbsync-ledger@. Removes both halves of the on-disk
-- representation: the @snapshot-headers\/\<slot\>@ entry (consulted
-- by 'listSnapshots') and the @lsm\/snapshots\/\<slot\>@ entry
-- (consulted by the LSM backend on load).
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
      let slotStr = show s
          headerPath  = headersDir  </> slotStr
          lsmDataPath = lsmSnapsDir </> slotStr
      removePathForcibly headerPath
      -- The LSM dir is best-effort: if the layout differs across
      -- backend versions, the header removal alone is enough for
      -- 'listSnapshots' to forget the snapshot.
      lsmExists <- doesPathExist lsmDataPath
      when lsmExists $ removePathForcibly lsmDataPath
      pure s

-- | Pull the captured log messages out of the test tracer's IORef.
-- The tracer prepends each new message to the head of the list, so
-- we reverse here to get chronological order — useful when scanning
-- for two related markers that should appear in a known order.
collectMessages :: IORef [LogMsg] -> IO [Text]
collectMessages ref = reverse . map lmMessage <$> readIORef ref
