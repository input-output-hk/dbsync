{-# LANGUAGE OverloadedStrings #-}

-- | After a real ledger-off sync that crosses epoch boundaries, assert the
-- recompute-invariants hold: each stored derived value still matches a
-- recomputation from its source tables. Guards against the \#2118 class of
-- silent data drift that the schema fingerprint cannot see.
module DbSync.Phase.RecomputeInvariantsSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy)

import DbSync.Test.AppHarness
  ( defaultTestProfile
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.E2E (conwayConfigDir, withAppSession)
import DbSync.Test.MockNode (forgeAndPushBlocksWith, withMockNode)
import DbSync.Test.MockNode.Workload (mainnetLikeWorkload)
import DbSync.Test.PgAssertions (countRows)
import DbSync.Test.RecomputeInvariants
  ( blockTxCountDriftCount
  , epochFinalizedDriftCount
  , txOutSumDriftCount
  )

spec :: Spec
spec = describe "Recompute invariants" $
  it "stored derived values match a recomputation from their source tables" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-recompute" $ \ledgerDir -> do
        -- 250 payment-tx blocks cross ~2 epoch boundaries, so epoch_finalized
        -- is backfilled and tx/tx_out are populated for the value checks.
        _ <- forgeAndPushBlocksWith mn 250 mainnetLikeWorkload

        tracer <- quietTracer
        withAppSession tracer defaultTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          -- A finalized epoch must exist, otherwise the epoch check is vacuous.
          finalizedEpochs <- countRows "epoch_finalized"
          finalizedEpochs `shouldSatisfy` (> 0)

          epochFinalizedDriftCount `shouldReturn` 0
          blockTxCountDriftCount `shouldReturn` 0
          txOutSumDriftCount `shouldReturn` 0
