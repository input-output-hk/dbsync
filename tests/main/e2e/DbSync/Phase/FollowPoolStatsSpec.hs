{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Asserts new @pool_stat@ rows land when Follow crosses an epoch
-- boundary at tip.
--
-- 'ledgerEnabledTestProfile' is required: @pool_stat@ is sourced
-- from @apNewEpoch@ (worker-derived) and the mock node's LSQ
-- handler does not respond, so a ledger-free boundary crossing
-- would hang on slot-details resolution.
module DbSync.Phase.FollowPoolStatsSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldSatisfy)

import DbSync.App.Config.Types
  ( SyncConfig (..)
  , SyncOption (..)
  , SyncOptions (..)
  )
import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.Pool (poolStatTableDef)
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
spec = describe "Follow pool_stat writes" $
  it "lands pool_stat rows when an epoch crosses at tip" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-pool-stats" $ \ledgerDir -> do
        tracer <- quietTracer
        -- 250 blocks puts the tip past two Ingest boundaries
        -- (~slot 1250 on the Conway test config). Ingest fills the
        -- baseline pool_stat rows; Follow adds one batch per crossed
        -- boundary thereafter.
        _ <- forgeAndPushBlocks mn 250

        withAppSession tracer poolStatsProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineBlocks <- countRows (tdName blockTableDef)
          baselinePoolStat <- countRows (tdName poolStatTableDef)

          -- 100 more blocks (~500 slots) crosses the next boundary in
          -- Follow.
          forgeAndWaitForBlocks mn 100 (baselineBlocks + 100) 120

          waitFor
            (tdName poolStatTableDef <> " count increases after Follow boundary")
            (do n <- countRows (tdName poolStatTableDef)
                pure (n > baselinePoolStat))
            30

          followPoolStat <- countRows (tdName poolStatTableDef)
          (followPoolStat - baselinePoolStat) `shouldSatisfy` (>= 1)

-- | 'ledgerEnabledTestProfile' with @pool_stats@ flipped on. The
-- default profile leaves it off because the rest of the e2e suite
-- doesn't need pool distribution rows.
poolStatsProfile :: SyncConfig
poolStatsProfile = ledgerEnabledTestProfile
  { scOptions = (scOptions ledgerEnabledTestProfile)
      { pcPoolStats = SyncOption True
      }
  }
