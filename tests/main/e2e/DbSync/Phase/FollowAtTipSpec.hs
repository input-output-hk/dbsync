{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | End-to-end tests for the at-tip behaviour of
-- 'FollowingChainTip': the phase flip after the consumer drains the
-- queue, forward progress after an idle period, rollback handling at
-- tip, and the per-block log cadence.
--
-- The four scenarios exercise the same orchestration code as
-- production through 'runApp' against the mock chainsync server.
-- They guard against the silent-hang failure mode where the receiver
-- doesn't wake from 'SendMsgRequestNext' once the consumer has
-- caught up.
module DbSync.Phase.FollowAtTipSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef)
import qualified Ouroboros.Network.Block as Network

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Ouroboros.Network.Block (pattern BlockPoint)

import DbSync.Trace.Backend (mkTestTracer)
import DbSync.Trace.Types (AppTracer, LogMsg (..))
import DbSync.Test.AppHarness
  ( defaultTestProfile
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.E2E
  ( conwayConfigDir
  , forgeAndWaitForBlocks
  , waitForLogMatch
  , withAppSession
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode
  ( MockNode
  , currentTip
  , forgeAndPushBlocks
  , rollbackMockNode
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "FollowingChainTip at-tip behaviour" $ do

  it "flips to FollowingChainTip exactly once when the queue drains" $
    runAtTipScenario $ \_mn logs -> do
      flips <- countLogsMatching logs isFlipToChainTip
      flips `shouldBe` 1

      drops <- countLogsMatching logs isFlipToVolatile
      drops `shouldBe` 0

  it "wakes from at-tip idle and applies each new block" $
    runAtTipScenario $ \mn _logs -> do
      -- Five forge-and-wait cycles with a 3 s idle period between
      -- pushes. Three seconds is well past one receiver round-trip
      -- through the chainsync 'SendMsgRequestNext' wait, so each
      -- cycle starts with the receiver genuinely idle.
      baselineBlocks <- countRows "block"
      for_ [1 .. (5 :: Int)] $ \i -> do
        threadDelay 3_000_000
        _ <- forgeAndPushBlocks mn 1
        let expectedTotal = baselineBlocks + i
        waitFor
          ("block " <> show i <> " lands after at-tip idle")
          (do n <- countRows "block"; pure (n >= expectedTotal))
          15

  it "rollback at FollowingChainTip drops phase back and recovers" $
    runAtTipScenario $ \mn logs -> do
      -- Snapshot the tip before forging the fork blocks. This is the
      -- point we'll roll back to; the three blocks forged on top must
      -- vanish, then a 4-block fork must land and flip phase back.
      tipBefore <- currentTip mn
      forkPoint <- case tipBefore of
        Network.TipGenesis ->
          panic "rollback scenario: server tip at genesis (no blocks)"
        Network.Tip slot hash _bn ->
          pure (BlockPoint slot hash)

      beforeFork <- countRows "block"

      -- Forge the to-be-rolled-back tail and wait for the consumer to
      -- catch up. Per-block "applied block" logs at tip prove the
      -- consumer's last-applied slot has moved past 'forkPoint'.
      forgeAndWaitForBlocks mn 3 (beforeFork + 3) 30
      waitForLogMatch logs "applied-block log after fork tip"
        isAppliedBlockTip
        15

      rollbackMockNode mn forkPoint

      waitForLogMatch logs "phase drop on rollback"
        isFlipToVolatile
        20
      waitForLogMatch logs "rollback marker"
        (\m -> T.isPrefixOf "rollback to " (lmMessage m))
        20

      waitFor "block count returns to pre-fork value"
        (do n <- countRows "block"; pure (n == beforeFork))
        30

      -- Forge a fresh 4-block fork; consumer applies them and we
      -- expect a second flip to FollowingChainTip.
      forgeAndWaitForBlocks mn 4 (beforeFork + 4) 60

      waitFor "second flip to FollowingChainTip"
        (do n <- countLogsMatching logs isFlipToChainTip
            pure (n >= 2))
        20

  it "logs every applied block at tip and does not thrash the phase" $
    runAtTipScenario $ \mn logs -> do
      appliedBefore <- countLogsMatching logs isAppliedBlockTip
      phaseBefore   <- countLogsMatching logs isPhaseTransition

      -- Five single-block pushes with a small gap between each. The
      -- per-block log at 'FollowingChainTip' must fire for every one.
      for_ [1 .. (5 :: Int)] $ \i -> do
        before <- countRows "block"
        _ <- forgeAndPushBlocks mn 1
        waitFor
          ("block " <> show i <> " lands during slow stream")
          (do n <- countRows "block"; pure (n > before))
          15
        threadDelay 1_000_000

      appliedAfter <- countLogsMatching logs isAppliedBlockTip
      phaseAfter   <- countLogsMatching logs isPhaseTransition

      (appliedAfter - appliedBefore) `shouldSatisfy` (>= 5)
      -- The phase didn't oscillate: same count before and after.
      phaseAfter `shouldBe` phaseBefore

-- ---------------------------------------------------------------------------
-- * Shared at-tip bracket
-- ---------------------------------------------------------------------------

-- | Drive Ingest → Prep → Follow, wait for the first
-- 'FollowingVolatileTail -> FollowingChainTip' transition, then hand
-- control to @body@. Every scenario in this spec needs the same
-- preamble; centralising it keeps each @it@ focused on the
-- scenario-specific assertions.
runAtTipScenario :: (MockNode -> IORef [LogMsg] -> IO ()) -> IO ()
runAtTipScenario body =
  withMockNode conwayConfigDir $ \mn ->
    withTempDir "dbsync-test-at-tip" $ \ledgerDir -> do
      logsRef <- newIORef []
      let tracer = mkTestTracer logsRef :: AppTracer

      -- 150 blocks → past k=10 and one Conway-config epoch boundary,
      -- so Ingest exits cleanly into Prep and the receiver is alive
      -- before the chain advances further.
      _ <- forgeAndPushBlocks mn 150

      withAppSession tracer defaultTestProfile mn ledgerDir $ \_app -> do
        waitForSyncComplete 90

        ingestBlocks <- countRows "block"
        forgeAndWaitForBlocks mn 20 (ingestBlocks + 20) 60

        waitForLogMatch logsRef "flip to FollowingChainTip"
          isFlipToChainTip
          30

        body mn logsRef

-- ---------------------------------------------------------------------------
-- * Log predicates
-- ---------------------------------------------------------------------------

-- | A 'FollowingVolatileTail -> FollowingChainTip' transition line
-- from 'setCurrentPhase'.
isFlipToChainTip :: LogMsg -> Bool
isFlipToChainTip m =
  lmComponent m == "Phase"
    && T.isInfixOf "FollowingVolatileTail -> FollowingChainTip" (lmMessage m)

-- | A 'FollowingChainTip -> FollowingVolatileTail' transition line.
-- Fires only on rollback while at tip.
isFlipToVolatile :: LogMsg -> Bool
isFlipToVolatile m =
  lmComponent m == "Phase"
    && T.isInfixOf "FollowingChainTip -> FollowingVolatileTail" (lmMessage m)

-- | Any phase transition line, regardless of direction. Used to
-- assert "phase did not change" by comparing before/after counts.
isPhaseTransition :: LogMsg -> Bool
isPhaseTransition m =
  lmComponent m == "Phase"
    && T.isInfixOf " -> " (lmMessage m)

-- | The per-block log emitted by 'maybeLogProgress' while in
-- 'FollowingChainTip'.
isAppliedBlockTip :: LogMsg -> Bool
isAppliedBlockTip m =
  lmComponent m == "FollowingChainTip"
    && T.isPrefixOf "applied block " (lmMessage m)

-- ---------------------------------------------------------------------------
-- * Log counting
-- ---------------------------------------------------------------------------

-- | Count how many entries in the captured-log 'IORef' satisfy the
-- predicate. O(n); fine for the test-sized log buffers each scenario
-- accumulates.
countLogsMatching :: IORef [LogMsg] -> (LogMsg -> Bool) -> IO Int
countLogsMatching ref p = length . filter p <$> readIORef ref
