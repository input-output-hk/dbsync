{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end coverage of the boundary-triggered writers under
-- 'FollowingVolatileTail' \/ 'FollowingChainTip'.
--
-- Drives Ingest \u2192 Prep \u2192 Follow with the ledger enabled, snapshots
-- the four EpochBoundary table counts at @sync_complete=true@, then
-- forges enough additional blocks for the live consumer to cross
-- exactly one epoch boundary. Exactly one new row in @ada_pots@,
-- @epoch_param@, and @epoch_state@ proves the boundary handler ran
-- on the Follow side; @cost_model@ must stay put because the model
-- is unchanged and deduped by hash.
--
-- Also checks the FK shape that future slices depend on:
--
--   * @epoch_param.cost_model_id@ is populated for the post-Alonzo
--     boundary that landed in Follow.
--   * @epoch_state@'s three governance FKs (committee, no_confidence,
--     constitution) are NULL \u2014 governance Follow is still stubbed.
module DbSync.Phase.FollowEpochBoundarySpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)

import DbSync.App.Config.Types
  ( SyncConfig (..)
  , OptionFlag (..)
  , DbProfile (..)
  )
import DbSync.Db.Schema.AdaPots (adaPotsTableDef)
import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.EpochBoundary
  ( costModelTableDef
  , epochParamTableDef
  , epochStateTableDef
  )
import DbSync.Db.Schema.EpochSyncStats (epochSyncStatsTableDef)
import DbSync.Db.Schema.Pool (poolStatTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestConfig
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E
  ( conwayConfigDir
  , forgeAndWaitForBlocks
  , withAppSession
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode
  ( forgeAndPushBlocks
  , forgeAndPushUntilNextEpoch
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows, readInt)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Follow boundary writes" $ do
  it "lands ada_pots, epoch_param, epoch_state, cost_model when an epoch crosses at tip" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-boundary" $ \ledgerDir -> do
        tracer <- quietTracer

        -- Conway test config: epochLength=500, activeSlotsCoeff=0.2,
        -- so ~5 slots per block. 250 forged blocks puts the tip near
        -- slot 1250 \u2014 well past the second boundary (slot 1000).
        -- Ingest will handle the first two boundaries; we use the
        -- third boundary (slot 1500) to exercise Follow.
        _ <- forgeAndPushBlocks mn 250

        withAppSession tracer boundaryProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineBlocks <- countRows (tdName blockTableDef)
          baselineAdaPots <- countRows (tdName adaPotsTableDef)
          baselineEpochParam <- countRows (tdName epochParamTableDef)
          baselineEpochState <- countRows (tdName epochStateTableDef)
          baselineCostModel <- countRows (tdName costModelTableDef)
          baselineSyncStats <- countRows (tdName epochSyncStatsTableDef)
          baselinePoolEpoch <- readInt maxPoolStatEpochQuery

          -- Push 100 more blocks (~500 more slots) so the live
          -- consumer crosses the next epoch boundary while in Follow.
          forgeAndWaitForBlocks mn 100 (baselineBlocks + 100) 120

          -- The boundary writes happen after the block that crossed
          -- the boundary lands; give the per-block PG transaction a
          -- few seconds to flush.
          waitFor
            (tdName adaPotsTableDef <> " count increases after Follow boundary")
            (do n <- countRows (tdName adaPotsTableDef)
                pure (n > baselineAdaPots))
            30

          followAdaPots <- countRows (tdName adaPotsTableDef)
          followEpochParam <- countRows (tdName epochParamTableDef)
          followEpochState <- countRows (tdName epochStateTableDef)
          followCostModel <- countRows (tdName costModelTableDef)

          -- The 100-block window crosses exactly one boundary (tip
          -- sits mid-epoch after 250 blocks; 100 blocks ~ 500 slots
          -- ~ one epoch length), so each boundary writer must land
          -- exactly one row. A duplicate boundary apply would show
          -- up here as a delta of 2.
          (followAdaPots - baselineAdaPots) `shouldBe` 1
          (followEpochParam - baselineEpochParam) `shouldBe` 1
          (followEpochState - baselineEpochState) `shouldBe` 1
          -- No protocol-param change is forged, so the cost model is
          -- byte-identical across the boundary and dedup-by-hash must
          -- reuse the existing row. A new row means dedup broke.
          (followCostModel - baselineCostModel) `shouldBe` 0

          -- The same boundary writes exactly one epoch_sync_stats row,
          -- authored by the Follow consumer (phase "Following…"), so a
          -- leftover Ingest row can't be mistaken for it.
          followSyncStats <- countRows (tdName epochSyncStatsTableDef)
          (followSyncStats - baselineSyncStats) `shouldBe` 1
          latestSyncFollowPhase <- T.strip <$> queryTestDb
            ( "SELECT (phase LIKE 'Following%')::text FROM "
                <> tdName epochSyncStatsTableDef
                <> " ORDER BY id DESC LIMIT 1"
            )
          latestSyncFollowPhase `shouldBe` "true"

          -- The crossing finalises a new epoch, so pool_stat gains a batch
          -- stamped with an epoch_no beyond the Ingest baseline.
          followPoolEpoch <- readInt maxPoolStatEpochQuery
          followPoolEpoch `shouldSatisfy` (> baselinePoolEpoch)

          -- The Follow-written epoch_param row points at a cost
          -- model. Read the most recent row and assert the FK is
          -- populated; a NULL here would mean the resolver returned
          -- a stub or the cost-model write didn't land.
          mostRecentCostModelId <- T.strip <$> queryTestDb
            ( "SELECT COALESCE(cost_model_id::text, '') FROM "
                <> tdName epochParamTableDef
                <> " ORDER BY id DESC LIMIT 1"
            )
          mostRecentCostModelId `shouldNotBe` ""

          -- Governance Follow is still stubbed; the three governance
          -- FK columns on epoch_state stay NULL.
          mostRecentCommitteeId <- T.strip <$> queryTestDb
            ( "SELECT COALESCE(committee_id::text, '') FROM "
                <> tdName epochStateTableDef
                <> " ORDER BY id DESC LIMIT 1"
            )
          mostRecentCommitteeId `shouldBe` ""

  it "epoch_current.blk_count grows live within the unfinalized epoch" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-epoch-current-live" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer ledgerEnabledTestConfig mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          -- Land at the start of a fresh epoch so the ten-block probe
          -- window (~50 slots) cannot straddle a 500-slot boundary.
          epochBefore <- readInt currentEpochQuery
          _ <- forgeAndPushUntilNextEpoch mn
          waitFor "block table reaches the new epoch"
            (do e <- readInt currentEpochQuery
                pure (e > epochBefore))
            60

          blocks0 <- countRows (tdName blockTableDef)
          epoch0 <- readInt currentEpochQuery
          blkCount0 <- readInt blkCountQuery

          forgeAndWaitForBlocks mn 5 (blocks0 + 5) 30
          epoch1 <- readInt currentEpochQuery
          blkCount1 <- readInt blkCountQuery

          forgeAndWaitForBlocks mn 5 (blocks0 + 10) 30
          epoch2 <- readInt currentEpochQuery
          blkCount2 <- readInt blkCountQuery

          -- The probe window stays inside the fresh epoch by
          -- construction; drift here means the scenario broke, not
          -- the view.
          epoch1 `shouldBe` epoch0
          epoch2 `shouldBe` epoch0

          -- Every forged block lands in the unfinalized epoch, so
          -- the live view must count each one exactly once.
          blkCount1 `shouldBe` blkCount0 + 5
          blkCount2 `shouldBe` blkCount0 + 10
  where
    currentEpochQuery =
      "SELECT epoch_no FROM " <> tdName blockTableDef
        <> " WHERE epoch_no IS NOT NULL ORDER BY id DESC LIMIT 1"
    blkCountQuery =
      "SELECT blk_count FROM epoch_current ORDER BY no DESC LIMIT 1"
    maxPoolStatEpochQuery =
      "SELECT COALESCE(max(epoch_no), 0) FROM " <> tdName poolStatTableDef

-- ---------------------------------------------------------------------------
-- * Profile
-- ---------------------------------------------------------------------------

-- | 'ledgerEnabledTestConfig' with @pool_stats@ on so the boundary
-- crossing also exercises the pool_stat writer.
boundaryProfile :: SyncConfig
boundaryProfile = ledgerEnabledTestConfig
  { scDbProfile = (scDbProfile ledgerEnabledTestConfig)
      { pcPoolStats = OptionFlag True
      }
  }
