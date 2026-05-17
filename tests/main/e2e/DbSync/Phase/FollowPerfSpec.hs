{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Throughput smoke test for 'FollowingChainTip'.
--
-- Forges enough blocks through Ingest/Prep to hand off to Follow,
-- then times how long Follow takes to drain a fixed batch of new
-- blocks pushed on top. Reports the rate to stderr and asserts a
-- low floor.
--
-- Two caveats keep this spec a smoke test rather than a benchmark:
--
-- * Conway test blocks are empty: each block has 1 block row, 1
--   slot_leader row (or dedup hit), no txs. The per-block PG cost
--   is dominated by a small fixed envelope (a few round-trips), so
--   pipelining gains are small in absolute time on the test PG
--   (Unix socket, ~100 microsec round-trips). The real win for
--   Stage F1 is on tx-bearing blocks (~175 round-trips on a real
--   testnet block, per FOLLOW-PERF.md).
--
-- * The mock chainsync server's @LocalStateQuery@ handler is a
--   stub. If Follow falls through 'getSlotDetailsIO' to the
--   node-querying fast path (e.g. because the observed summary
--   doesn't cover the requested slot), the call blocks
--   indefinitely. Keep the batch small enough that every requested
--   slot is inside the safe-zone window of the seeded interpreter.
--
-- The real perf validation is a manual testnet run.
module DbSync.Phase.FollowPerfSpec (spec) where

import Cardano.Prelude hiding (hPutStrLn)

import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import System.IO (hPutStrLn)

import Test.Hspec (Spec, describe, it, shouldSatisfy)

import DbSync.Test.AppHarness
  ( defaultTestProfile
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.E2E
  ( conwayConfigDir
  , withAppSession
  )
import DbSync.Test.MockNode (forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions (countRows)

spec :: Spec
spec = describe "FollowingChainTip throughput" $ do

  it "drains a fixed batch through Follow above the regression floor" $ do
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-perf-follow" $ \ledgerDir -> do

        -- 150 blocks crosses k=10 + the 500-slot Conway test epoch
        -- so Ingest exits cleanly and hands off to Follow.
        _ <- forgeAndPushBlocks mn 150
        tracer <- quietTracer
        withAppSession tracer defaultTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 60

          ingestBlocks <- countRows "block"
          -- Small batch so every requested slot is inside the
          -- safe-zone window of the seeded interpreter; otherwise
          -- 'getSlotDetailsIO' falls through to the node's LSQ
          -- handler which is a stub in the mock environment and
          -- blocks indefinitely. ~40 blocks fits comfortably for
          -- the @activeSlotsCoeff = 0.2@ Conway test config.
          let perfBatch  = 40
              timeoutSec = 30 :: Int

          -- Push the batch into the mock server; receiver pulls
          -- async. Time only the Follow drain to keep forging
          -- noise out of the measurement.
          _ <- forgeAndPushBlocks mn perfBatch
          startedAt <- getCurrentTime
          drained <- drainUpTo (ingestBlocks + perfBatch) timeoutSec
          endedAt <- getCurrentTime

          let elapsed     = realToFrac (diffUTCTime endedAt startedAt) :: Double
              blocksDone  = drained - ingestBlocks
              rate        =
                if elapsed > 0
                  then fromIntegral blocksDone / elapsed
                  else 0 :: Double

          -- Goes to stderr so the operator running the suite sees
          -- the absolute number, not just pass/fail. Useful for
          -- before/after comparisons across the Stage F1 refactor.
          hPutStrLn stderr $
            T.unpack $ mconcat
              [ "  [FollowPerf] "
              , T.pack (show blocksDone), "/", T.pack (show perfBatch)
              , " blocks in "
              , fmtSeconds elapsed, "s = "
              , fmtRate rate, " blk/s"
              ]

          -- Empty-block per-block PG cost is sub-millisecond on
          -- Unix socket (~600 blk/s observed post-pipelining). The
          -- 50 blk/s floor catches a catastrophic regression — e.g.
          -- per-row @Conn.use@ creep that would slow this to ~5
          -- blk/s — while leaving headroom for CI noise.
          rate `shouldSatisfy` (> 50.0)

-- | Poll @count(*) FROM block@ until it reaches @minTotal@ or the
-- timeout elapses. Returns the most recent observed count rather
-- than panicking on timeout, so the caller can compute a partial
-- rate and the test still produces a useful number.
drainUpTo :: Int -> Int -> IO Int
drainUpTo minTotal timeoutSec = do
  start <- getCurrentTime
  go start
  where
    go startedAt = do
      n <- countRows "block"
      if n >= minTotal
        then pure n
        else do
          now <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime now startedAt) :: Double
          if elapsed >= fromIntegral timeoutSec
            then pure n
            else do
              threadDelay 200_000
              go startedAt

fmtSeconds :: Double -> Text
fmtSeconds d = T.pack (showFFloat (Just 2) d "")

fmtRate :: Double -> Text
fmtRate r
  | r < 10    = T.pack (showFFloat (Just 2) r "")
  | r < 1000  = T.pack (showFFloat (Just 1) r "")
  | otherwise = T.pack (showFFloat (Just 0) r "")
