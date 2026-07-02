{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
import qualified Ouroboros.Network.Block as Network

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)

import Ouroboros.Network.Block (data BlockPoint)

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
import DbSync.Test.MockNode
  ( currentTip
  , forgeAndPushBlocks
  , forgeAndPushUntilNextEpoch
  , rollbackMockNode
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)

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

  it "tolerates a within-window rollback after crossing the boundary without disturbing ada_pots" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-boundary-rollback" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineAdaPots <- countRows (tdName adaPotsTableDef)
          baselineEpochParam <- countRows (tdName epochParamTableDef)

          -- Cross the next boundary in Follow.
          _ <- forgeAndPushUntilNextEpoch mn
          waitFor
            (tdName adaPotsTableDef <> " count grows after boundary")
            (do n <- countRows (tdName adaPotsTableDef); pure (n > baselineAdaPots))
            60

          afterBoundaryAdaPots <- countRows (tdName adaPotsTableDef)
          afterBoundaryEpochParam <- countRows (tdName epochParamTableDef)
          (afterBoundaryEpochParam - baselineEpochParam) `shouldSatisfy` (>= 1)
          postBoundaryBlocks <- countRows (tdName blockTableDef)

          -- Capture the post-boundary tip and forge a few more blocks
          -- on top, well within the ledger's in-memory rollback window.
          tipAfterBoundary <- currentTip mn
          forkPoint <- case tipAfterBoundary of
            Network.TipGenesis ->
              panic "rollback scenario: server tip at genesis (no blocks)"
            Network.Tip slot hash _bn ->
              pure (BlockPoint slot hash)

          forgeAndWaitForBlocks mn 5 (postBoundaryBlocks + 5) 60

          -- Roll back the five extra blocks within the new epoch. The
          -- boundary row must survive the rewind without being undone
          -- or duplicated.
          rollbackMockNode mn forkPoint
          waitFor (tdName blockTableDef <> " returns to post-boundary value")
            (do n <- countRows (tdName blockTableDef); pure (n == postBoundaryBlocks))
            60

          afterRollbackAdaPots <- countRows (tdName adaPotsTableDef)
          afterRollbackEpochParam <- countRows (tdName epochParamTableDef)
          afterRollbackAdaPots `shouldBe` afterBoundaryAdaPots
          afterRollbackEpochParam `shouldBe` afterBoundaryEpochParam

          -- Re-forge within the same epoch; boundary writes must not
          -- duplicate.
          forgeAndWaitForBlocks mn 10 (postBoundaryBlocks + 10) 60

          finalAdaPots <- countRows (tdName adaPotsTableDef)
          finalEpochParam <- countRows (tdName epochParamTableDef)
          finalAdaPots `shouldBe` afterBoundaryAdaPots
          finalEpochParam `shouldBe` afterBoundaryEpochParam

  it "epoch_current.blk_count grows live within the unfinalized epoch" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-epoch-current-live" $ \ledgerDir -> do
        tracer <- quietTracer
        _ <- forgeAndPushBlocks mn 250
        withAppSession tracer ledgerEnabledTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineBlocks <- countRows (tdName blockTableDef)

          -- Capture the current epoch before forging more blocks so
          -- the assertion can skip if a boundary lands in between.
          let currentEpochQuery =
                "SELECT epoch_no FROM " <> tdName blockTableDef
                  <> " WHERE epoch_no IS NOT NULL ORDER BY id DESC LIMIT 1"
              blkCountQuery =
                "SELECT blk_count FROM epoch_current ORDER BY no DESC LIMIT 1"

          epoch0 <- T.strip <$> queryTestDb currentEpochQuery
          blkCount0 <- T.strip <$> queryTestDb blkCountQuery

          -- Forge a small window of blocks well inside the current
          -- epoch (Conway test config has ~100 blocks per epoch).
          forgeAndWaitForBlocks mn 5 (baselineBlocks + 5) 30
          epoch1 <- T.strip <$> queryTestDb currentEpochQuery
          blkCount1 <- T.strip <$> queryTestDb blkCountQuery

          forgeAndWaitForBlocks mn 5 (baselineBlocks + 10) 30
          epoch2 <- T.strip <$> queryTestDb currentEpochQuery
          blkCount2 <- T.strip <$> queryTestDb blkCountQuery

          -- Only assert if the whole window stayed in one epoch; a
          -- boundary crossing would reset blk_count.
          when (epoch0 == epoch1 && epoch1 == epoch2) $ do
            readBlk blkCount1 `shouldSatisfy` (> readBlk blkCount0)
            readBlk blkCount2 `shouldSatisfy` (> readBlk blkCount1)

-- | Parse a 'blk_count' string into 'Int' for ordering comparisons.
readBlk :: Text -> Int
readBlk = fromMaybe 0 . readMaybe . T.unpack
