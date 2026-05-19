{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Boot the Follow fast-path against a database that has committed
-- past the latest on-disk ledger snapshot.
--
-- The snapshot writer is asynchronous; on any shutdown the on-disk
-- snapshot can lag the consumer's last PG commit. On the next boot
-- the Fast-Path detects the gap, picks the newest snapshot whose
-- slot has a matching @block.hash@ in PG, and rolls PG back to that
-- point via 'Phase.Following.Rollback.rollbackToPoint'. Once aligned,
-- chainsync re-streams the rolled-back range and Follow re-inserts
-- the rows.
--
-- The deterministic gap is engineered by deleting the newest
-- snapshot's header + LSM directory between the two sessions; the
-- next-newest survivor becomes the chosen restart point.
module DbSync.Phase.FollowRollbackOnBootSpec (spec) where

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
spec = describe "FollowingChainTip fast-path rollback on boot" $
  it "rolls PG back to the chosen snapshot when the snapshot lags last_committed_slot" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-rollback-on-boot" $ \ledgerDir -> do
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
            -- two snapshots we can choose between in the rollback
            -- step below.
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

        -- Capture the second-session logs so we can confirm the
        -- rollback path actually fired. The previous code panicked
        -- here with "snapshot at slot S is behind last_committed_slot
        -- L"; the new path emits a rollback-from-L-to-S line and
        -- proceeds.
        secondLogs <- newIORef []
        let secondTracer = mkTestTracer secondLogs

        withAppSessionResume secondTracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
          waitFor "sync_complete remains true on restart" syncCompleteTrue 60

          -- ChainSync re-delivers the rolled-back range and Follow
          -- re-inserts every block. Wait for the block table to
          -- return to its pre-restart size — the simplest evidence
          -- that the rollback was followed by a successful re-sync.
          waitFor
            (tdName blockTableDef <> " count returns to " <> show preBlocks)
            (do n <- countRows (tdName blockTableDef); pure (n >= preBlocks))
            60

          afterReSyncSlot <- readLastCommittedSlot
          afterReSyncSlot `shouldBe` lastCommitted

          -- Dedup rows survived the cascade; the re-insert path
          -- found them via SELECT-then-nextval rather than allocating
          -- new ids.
          postDedupCounts <- traverse countRows dedupTables
          postDedupCounts `shouldBe` preDedupCounts

          -- Forge new blocks past the original tip; Follow advances.
          let target = preBlocks + 20
          forgeAndWaitForBlocks mn 20 target 90

          finalBlocks <- countRows (tdName blockTableDef)
          finalBlocks `shouldSatisfy` (>= target)

        -- Log-message assertion: the rollback path emitted both the
        -- "snapshot at slot S" load line and the rollback line.
        -- Without these two markers the test could pass even if the
        -- gap-handling branch never ran.
        secondMessages <- collectMessages secondLogs
        secondMessages `shouldSatisfy`
          any (T.isInfixOf ("Loading ledger snapshot at slot " <> show chosenSlot))
        secondMessages `shouldSatisfy`
          any (\m -> "Rolling back PG from slot" `T.isInfixOf` m
                  && ("to snapshot slot " <> show chosenSlot) `T.isInfixOf` m)

-- | Tables whose row counts must survive the rollback cascade.
-- These are dedup tables: content-keyed, not slot-keyed, so the
-- cascade leaves them alone. The re-insert path finds the rows via
-- SELECT-then-nextval instead of allocating new ids.
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
