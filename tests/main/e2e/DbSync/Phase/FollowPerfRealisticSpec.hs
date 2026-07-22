{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Follow-drain coverage on /realistic/ blocks, with a throughput
-- report.
--
-- This spec forges blocks closer to mainnet's mid-traffic shape
-- (~10 payment txs per block, each consuming one bulk-genesis UTxO
-- and producing target plus change outputs) and times the Follow
-- drain on that. The per-block PG work now includes:
--
--   * One @assignBlockId@ + one @resolveSlotLeader@ + ~10
--     @allocateAllIds@ pre-allocations.
--   * ~10 @resolveAddressIdBuffered@ calls (cache-hit after the
--     first within a block; cross-block, addresses already exist in
--     PG so the resolve is a SELECT only).
--   * ~10 @resolveInputValues@ calls (per-input lookup against the
--     tx_out heap).
--   * ~20 INSERTs queued into the buffered writer + one pipeline
--     flush.
--   * BEGIN, the flush, COMMIT.
--
-- That's the per-block path a Follow throughput regression would
-- exercise. The shape is intentionally light by mainnet standards
-- (single-decimal txs per block; no multi-asset; no delegations) so
-- CI runtime stays bounded.
--
-- The CI gate is functional only: every pushed block drains and
-- every built tx lands. Wall-clock throughput is machine-dependent,
-- so it is reported to stderr for before\/after comparison rather
-- than asserted; the real perf validation is a manual testnet run.
module DbSync.Phase.FollowPerfRealisticSpec (spec) where

import Cardano.Prelude hiding (hPutStrLn)

import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import System.IO (hPutStrLn)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Core (blockTableDef, txTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (txOutTableDef)
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
import DbSync.Test.MockChain
  ( RealisticBlockShape (..)
  , buildRealisticTxs
  , mainnetAverageShape
  )
import DbSync.Test.MockNode
  ( forgeAndPush
  , forgeAndPushBlocks
  , mnChain
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)

spec :: Spec
spec = describe "FollowingChainTip throughput on realistic blocks" $ do

  it "drains a realistic batch completely, reporting throughput to stderr" $ do
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-perf-realistic" $ \ledgerDir -> do

        -- Cross k=10 + the 500-slot Conway test epoch with empty
        -- filler blocks so Ingest exits cleanly. Same pre-stage as
        -- the empty-block 'FollowPerfSpec'; the realistic work all
        -- happens in the timed batch below.
        _ <- forgeAndPushBlocks mn 150

        tracer <- quietTracer
        withAppSession tracer defaultTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 60

          ingestBlocks <- countRows (tdName blockTableDef)
          ingestTxs    <- countRows (tdName txTableDef)
          ingestOuts   <- countRows (tdName txOutTableDef)

          -- Timed batch sizing. 20 blocks at the default shape gives
          -- 200 payment txs and ~400 tx_out rows — enough volume to
          -- make the wall-clock measurement meaningful while staying
          -- inside the safe-zone window of the seeded interpreter
          -- (same constraint that caps 'FollowPerfSpec' at ~40
          -- empty blocks).
          let realisticBlocks = 20
              shape           = mainnetAverageShape
              timeoutSec      = 120 :: Int

          -- Forge realistic blocks one by one. Each call rebuilds
          -- the tx list against the live ledger state, so the next
          -- block's UTxO indices resolve against the post-previous-
          -- block state.
          for_ [1 .. realisticBlocks] $ \_ -> do
            txs <- buildRealisticTxs (mnChain mn) shape
            _   <- forgeAndPush mn txs
            pure ()

          startedAt <- getCurrentTime
          drained   <- drainUpTo (ingestBlocks + realisticBlocks) timeoutSec
          endedAt   <- getCurrentTime

          finalTxs  <- countRows (tdName txTableDef)
          finalOuts <- countRows (tdName txOutTableDef)

          let elapsed    = realToFrac (diffUTCTime endedAt startedAt) :: Double
              blocksDone = drained   - ingestBlocks
              txsDone    = finalTxs  - ingestTxs
              outsDone   = finalOuts - ingestOuts
              rate       =
                if elapsed > 0
                  then fromIntegral blocksDone / elapsed
                  else 0 :: Double
              expectedTxsMin = realisticBlocks * rbsPaymentTxCount shape

          -- Goes to stderr so the operator running the suite sees
          -- the absolute number, the actual row counts, and the
          -- shape-vs-rate ratio. Useful for before/after comparison
          -- across the next perf-fix iteration.
          hPutStrLn stderr $
            T.unpack $ mconcat
              [ "  [FollowPerfRealistic] "
              , T.pack (show blocksDone), "/", T.pack (show realisticBlocks)
              , " blocks ("
              , T.pack (show txsDone),  " txs, "
              , T.pack (show outsDone), " outputs"
              , ") in "
              , fmtSeconds elapsed, "s = "
              , fmtRate rate, " blk/s"
              ]

          -- Every realistic block we pushed must have landed.
          drained `shouldBe` ingestBlocks + realisticBlocks
          -- And produced at least the txs we built; allows for slack
          -- if a future shape adds optional certs / mints.
          txsDone `shouldSatisfy` (>= expectedTxsMin)

-- | Poll @count(*) FROM block@ until it reaches @minTotal@ or the
-- timeout elapses. Returns the most recent observed count rather
-- than panicking on timeout, so the caller can compute a partial
-- rate and the test still produces a useful number in the
-- regression case.
drainUpTo :: Int -> Int -> IO Int
drainUpTo minTotal timeoutSec = do
  start <- getCurrentTime
  go start
  where
    go startedAt = do
      n <- countRows (tdName blockTableDef)
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
