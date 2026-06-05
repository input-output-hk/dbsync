{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Asserts a new @epoch_sync_stats@ row lands when Follow crosses
-- an epoch boundary at tip.
--
-- 'ledgerEnabledTestProfile' is used so the worker keeps
-- 'sqvInterpreterVar' fresh; the mock node's LSQ handler does not
-- respond, so a ledger-free boundary crossing would hang on
-- slot-details resolution.
module DbSync.Phase.FollowEpochSyncStatsSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldSatisfy)

import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.EpochSyncStats (epochSyncStatsTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestProfile
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.E2E
  ( conwayConfigDir
  , forgeAndWaitForBlocks
  , withAppSession
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode (forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Follow epoch_sync_stats writes" $
  it "lands an epoch_sync_stats row when an epoch crosses at tip" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-epoch-sync-stats" $ \ledgerDir -> do
        tracer <- quietTracer
        -- 250 blocks ~ slot 1250 (past the second boundary at slot
        -- 1000). Ingest writes its own epoch_sync_stats rows; the
        -- spec snapshots that count and asserts Follow adds at least
        -- one after crossing the next boundary.
        _ <- forgeAndPushBlocks mn 250

        withAppSession tracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineBlocks <- countRows (tdName blockTableDef)
          baselineStats  <- countRows (tdName epochSyncStatsTableDef)

          -- 100 blocks (~500 slots) crosses the next boundary
          -- while in Follow.
          forgeAndWaitForBlocks mn 100 (baselineBlocks + 100) 120

          waitFor
            (tdName epochSyncStatsTableDef <> " count increases after Follow boundary")
            (do n <- countRows (tdName epochSyncStatsTableDef)
                pure (n > baselineStats))
            30

          followStats <- countRows (tdName epochSyncStatsTableDef)
          (followStats - baselineStats) `shouldSatisfy` (>= 1)
