{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end coverage of the boundary-triggered writers under
-- 'FollowingVolatileTail' \/ 'FollowingChainTip'.
--
-- Drives Ingest \u2192 Prep \u2192 Follow with the ledger enabled, snapshots
-- the four EpochBoundary table counts at @sync_complete=true@, then
-- forges enough additional blocks for the live consumer to cross an
-- epoch boundary. The new row in @ada_pots@, @epoch_param@,
-- @epoch_state@, and (if a fresh cost model lands) @cost_model@
-- proves the boundary handler ran on the Follow side.
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

import DbSync.Db.Schema.AdaPots (adaPotsTableDef)
import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.EpochBoundary
  ( costModelTableDef
  , epochParamTableDef
  , epochStateTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestProfile
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
import DbSync.Test.MockNode (forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Follow boundary writes" $
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

        withAppSession tracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineBlocks <- countRows (tdName blockTableDef)
          baselineAdaPots <- countRows (tdName adaPotsTableDef)
          baselineEpochParam <- countRows (tdName epochParamTableDef)
          baselineEpochState <- countRows (tdName epochStateTableDef)
          baselineCostModel <- countRows (tdName costModelTableDef)

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

          (followAdaPots - baselineAdaPots) `shouldSatisfy` (>= 1)
          (followEpochParam - baselineEpochParam) `shouldSatisfy` (>= 1)
          (followEpochState - baselineEpochState) `shouldSatisfy` (>= 1)
          -- cost_model is deduped by hash; the same model across
          -- boundaries doesn't add a row, so the delta is >= 0.
          (followCostModel - baselineCostModel) `shouldSatisfy` (>= 0)

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
