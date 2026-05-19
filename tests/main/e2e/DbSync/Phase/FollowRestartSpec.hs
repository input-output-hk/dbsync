{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Restart a sync that's already in 'FollowingChainTip' and verify
-- the second run picks up cleanly.
--
-- Targets two regressions:
--
--   * The dedup-counter cleanup must not wipe legitimately-committed
--     rows on a Follow restart ('CleanupMode.FollowRestart').
--   * The ledger fast-path must restore the in-memory 'LedgerDB' from
--     the latest on-disk snapshot at or before
--     @last_committed_slot@ ('loadLedgerSnapshotForFollow').
module DbSync.Phase.FollowRestartSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldSatisfy)

import DbSync.Config.Types (SyncConfig)
import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Pool (poolHashTableDef)
import DbSync.Db.Schema.StakeDelegation (stakeAddressTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (txOutTableDef)
import DbSync.Test.AppHarness
  ( defaultTestProfile
  , ledgerEnabledTestProfile
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
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
import DbSync.Test.PgAssertions (countRows)

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
