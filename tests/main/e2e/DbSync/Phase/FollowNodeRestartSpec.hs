{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end coverage of the chainsync reconnect path: the node
-- drops and returns at the same tip while db-sync keeps running.
--
-- The receiver's subscription worker reconnects on its own and
-- re-intersects at its latest received point. The node answers with a
-- confirming rollback to that point — a protocol artefact — which the
-- receiver must recognise and suppress: no rollback marker reaches the
-- consumer, so no PG rows are deleted and none are re-applied. Blocks
-- forged after the reconnect still land exactly once.
module DbSync.Phase.FollowNodeRestartSpec (spec) where

import Cardano.Prelude

import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
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
  , withAppSession
  )
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode
  ( forgeAndPushBlocks
  , restartMockNode
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Node-restart reconnect resilience" $
  it "resumes at the same tip without rolling back PG or duplicating rows" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-node-restart" $ \ledgerDir -> do
        logs <- newIORef []
        let tracer = mkTestTracer logs :: AppTracer

        -- Seed enough history to run Ingest -> Prep -> Follow.
        _ <- forgeAndPushBlocks mn 150

        withAppSession tracer defaultTestProfile mn ledgerDir $ \_app -> do
          waitForSyncComplete 90

          -- Advance in Follow so the receiver's latest received point
          -- is a live post-Ingest point before the node drops.
          ingestBlocks <- countRows (tdName blockTableDef)
          forgeAndWaitForBlocks mn 20 (ingestBlocks + 20) 60

          blocksBefore  <- countRows (tdName blockTableDef)
          resumeBefore  <- countMatching logs isResumeFromPoint
          confirmBefore <- countMatching logs isConfirmingRollback
          markerBefore  <- countMatching logs isConsumerRollback

          -- Drop the node and bring it back on the same socket with the
          -- same chain. The subscription worker reconnects on its own; the
          -- resume-log delta is the race-free signal that it has, since its
          -- baseline was captured before the restart.
          restartMockNode mn

          -- The reconnect re-intersects at the last received point...
          waitFor "receiver resumes from last received point"
            (gtBaseline logs isResumeFromPoint resumeBefore)
            60

          -- ...and the node's confirming rollback to that point is
          -- taken as a protocol step, not a chain reorganisation.
          waitFor "confirming-intersect rollback recognised"
            (gtBaseline logs isConfirmingRollback confirmBefore)
            60

          -- Blocks forged after the reconnect land on top exactly
          -- once: no re-applied history, no lost blocks.
          forgeAndWaitForBlocks mn 10 (blocksBefore + 10) 60
          blocksAfter <- countRows (tdName blockTableDef)
          blocksAfter `shouldBe` blocksBefore + 10

          -- A confirming rollback delivers no marker, so the consumer
          -- never deleted rows: no "rollback to" line on the restart.
          markerAfter <- countMatching logs isConsumerRollback
          markerAfter `shouldBe` markerBefore

-- ---------------------------------------------------------------------------
-- * Log predicates
-- ---------------------------------------------------------------------------

-- | The receiver logs this on every reconnection where a last
-- received point is known (the resume path).
isResumeFromPoint :: LogMsg -> Bool
isResumeFromPoint m =
  lmComponent m == "ChainSync"
    && T.isPrefixOf "Resuming from last received point" (lmMessage m)

-- | The Debug line for the node's confirming rollback to the chosen
-- intersection point. Capitalised and component-tagged distinctly from
-- the consumer's real-rollback marker.
isConfirmingRollback :: LogMsg -> Bool
isConfirmingRollback m =
  lmComponent m == "ChainSync"
    && T.isInfixOf "confirming intersect" (lmMessage m)

-- | The consumer's real-rollback marker, emitted only when a rollback
-- cascade is about to DELETE rows. Must never fire on a same-tip
-- restart.
isConsumerRollback :: LogMsg -> Bool
isConsumerRollback m = T.isPrefixOf "rollback to " (lmMessage m)

-- ---------------------------------------------------------------------------
-- * Log counting
-- ---------------------------------------------------------------------------

countMatching :: IORef [LogMsg] -> (LogMsg -> Bool) -> IO Int
countMatching ref p = length . filter p <$> readIORef ref

-- | 'waitFor' predicate: the match count has grown past @baseline@.
gtBaseline :: IORef [LogMsg] -> (LogMsg -> Bool) -> Int -> IO Bool
gtBaseline ref p baseline = do
  n <- countMatching ref p
  pure (n > baseline)
