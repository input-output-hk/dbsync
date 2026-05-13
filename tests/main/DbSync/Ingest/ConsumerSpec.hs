{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for 'DbSync.Ingest.Consumer.advanceReplay' and the
-- ledger-replay drain helper.
module DbSync.Ingest.ConsumerSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Control.Concurrent.STM (isEmptyTBQueue, newTBQueueIO, writeTBQueue)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as SMaybe
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToDiffTime)
import System.Timeout (timeout)

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import qualified DbSync.Era.Shelley.Generic.StakeDist as Generic
import DbSync.Ingest.Consumer
  ( ReplayAdvance (..)
  , ReplayLog (..)
  , ReplayLogState (..)
  , ReplayProgress (..)
  , advanceReplay
  , drainAppliedQueue
  , progressLogInterval
  )
import DbSync.Ledger.Types (ApplyResult (..), emptyDepositsMap)
import DbSync.StateQuery (SlotDetails (..))

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Wall-clock anchor for tests that don't care about absolute time.
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)

-- | Replay end watermark used across cases.
endSlot :: SlotNo
endSlot = SlotNo 1_000_000

-- | A 'ReplayProgress' anchored at @t0@ with @n@ blocks already
-- observed. 'rpLastLogTime' equals 'rpStartTime' so the next
-- progress trigger requires waiting at least 'progressLogInterval'.
mkProgress :: Word64 -> ReplayProgress
mkProgress n =
  ReplayProgress
    { rpStartTime     = t0
    , rpBlocksApplied = n
    , rpLastLogTime   = t0
    }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  advanceReplaySpec
  drainAppliedQueueSpec

advanceReplaySpec :: Spec
advanceReplaySpec = describe "DbSync.Ingest.Consumer.advanceReplay" $ do

  describe "no replay configured (boundary = Nothing)" $ do
    it "is a no-op on NoReplay" $
      advanceReplay (SlotNo 100) Nothing t0 NoReplay
        `shouldBe` ReplayAdvance NoReplay ReplayLogNothing

    it "preserves ReplayPending without firing a log" $
      advanceReplay (SlotNo 100) Nothing t0 ReplayPending
        `shouldBe` ReplayAdvance ReplayPending ReplayLogNothing

    it "preserves InReplay without firing a log" $ do
      let p = mkProgress 5
      advanceReplay (SlotNo 100) Nothing t0 (InReplay p)
        `shouldBe` ReplayAdvance (InReplay p) ReplayLogNothing

  describe "ReplayPending → InReplay (first replay block)" $ do
    it "transitions on a block at or before the boundary" $ do
      let result =
            advanceReplay (SlotNo 800_000) (Just endSlot) t0 ReplayPending
          expected =
            ReplayAdvance
              ( InReplay
                  ReplayProgress
                    { rpStartTime     = t0
                    , rpBlocksApplied = 1
                    , rpLastLogTime   = t0
                    }
              )
              ReplayLogNothing
      result `shouldBe` expected

    it "transitions on a block exactly at the boundary" $
      raNewState
        ( advanceReplay endSlot (Just endSlot) t0 ReplayPending
        )
        `shouldBe`
          InReplay
            ReplayProgress
              { rpStartTime     = t0
              , rpBlocksApplied = 1
              , rpLastLogTime   = t0
              }

    it "skips straight to NoReplay when the first block is past the boundary" $
      advanceReplay (SlotNo 1_500_000) (Just endSlot) t0 ReplayPending
        `shouldBe` ReplayAdvance NoReplay ReplayLogNothing

  describe "InReplay (subsequent replay blocks)" $ do
    it "increments rpBlocksApplied without logging when under the interval" $ do
      let p = mkProgress 5
          tQuick = addUTCTime 1 t0   -- 1s later, under the 5s interval
          result =
            advanceReplay (SlotNo 800_001) (Just endSlot) tQuick (InReplay p)
      result
        `shouldBe`
          ReplayAdvance
            ( InReplay
                ReplayProgress
                  { rpStartTime     = t0
                  , rpBlocksApplied = 6
                  , rpLastLogTime   = t0
                  }
            )
            ReplayLogNothing

    it "fires ReplayLogProgress once the interval elapses, and resets rpLastLogTime" $ do
      let p = mkProgress 100
          tNext = addUTCTime progressLogInterval t0
          result =
            advanceReplay (SlotNo 800_001) (Just endSlot) tNext (InReplay p)
      result
        `shouldBe`
          ReplayAdvance
            ( InReplay
                ReplayProgress
                  { rpStartTime     = t0
                  , rpBlocksApplied = 101
                  , rpLastLogTime   = tNext
                  }
            )
            (ReplayLogProgress 101)

    it "does not fire a second progress log until another interval has elapsed" $ do
      let p = mkProgress 100
          tFirst = addUTCTime progressLogInterval t0
          -- After firing once, rpLastLogTime = tFirst.
          ReplayAdvance s1 _ =
            advanceReplay (SlotNo 800_001) (Just endSlot) tFirst (InReplay p)
          tSoonAfter = addUTCTime 1 tFirst
          result =
            advanceReplay (SlotNo 800_002) (Just endSlot) tSoonAfter s1
      raLog result `shouldBe` ReplayLogNothing

  describe "InReplay → NoReplay (replay completion)" $ do
    it "fires ReplayLogComplete with total elapsed time on first non-replay block" $ do
      let p = mkProgress 172_562
          tEnd = addUTCTime 17.9 t0
          result =
            advanceReplay (SlotNo 1_000_001) (Just endSlot) tEnd (InReplay p)
      result
        `shouldBe`
          ReplayAdvance NoReplay (ReplayLogComplete 172_562 17.9)

    it "does NOT increment rpBlocksApplied for the post-replay block" $ do
      let p = mkProgress 50
          tEnd = addUTCTime 6 t0
          ReplayAdvance _ logEv =
            advanceReplay (SlotNo 1_000_001) (Just endSlot) tEnd (InReplay p)
      logEv `shouldBe` ReplayLogComplete 50 6   -- "50", not "51"

  describe "NoReplay (terminal)" $
    it "stays NoReplay even if a replay-bound block somehow arrives" $ do
      -- Scenario: caller already exited replay; a chain rollback or
      -- ordering glitch sends a slot we previously considered
      -- in-window. The state machine refuses to re-enter replay.
      let result =
            advanceReplay (SlotNo 500_000) (Just endSlot) t0 NoReplay
      result `shouldBe` ReplayAdvance NoReplay ReplayLogNothing

-- ---------------------------------------------------------------------------
-- drainAppliedQueue
-- ---------------------------------------------------------------------------

-- | Pops exactly one entry per call and never blocks while data is queued.
drainAppliedQueueSpec :: Spec
drainAppliedQueueSpec = describe "DbSync.Ingest.Consumer.drainAppliedQueue" $ do

  it "pops exactly one entry per call" $ do
    q <- newTBQueueIO 100
    atomically $ writeTBQueue q sampleApplyResult
    atomically $ writeTBQueue q sampleApplyResult
    drainAppliedQueue q
    atomically (isEmptyTBQueue q) `shouldReturn` False
    drainAppliedQueue q
    atomically (isEmptyTBQueue q) `shouldReturn` True

  it "fully drains a queue populated with N apply results without blocking" $ do
    -- Replay scenario: the worker has applied N blocks ahead, the
    -- consumer must drain N times to avoid back-pressure deadlock.
    q <- newTBQueueIO 100
    let n = 50 :: Int
    replicateM_ n $ atomically $ writeTBQueue q sampleApplyResult
    -- Hard deadline so a regression fails fast instead of hanging CI.
    result <- timeout 1_000_000 $ replicateM_ n $ drainAppliedQueue q
    result `shouldSatisfy` isJust
    atomically (isEmptyTBQueue q) `shouldReturn` True

  it "drains a fully-saturated queue (worker at the back-pressure cap)" $ do
    -- Production cap is 100; the exact deadlock case the fix prevents.
    let cap = 100 :: Int
    q <- newTBQueueIO (fromIntegral cap)
    replicateM_ cap $ atomically $ writeTBQueue q sampleApplyResult
    result <- timeout 1_000_000 $ replicateM_ cap $ drainAppliedQueue q
    result `shouldSatisfy` isJust
    atomically (isEmptyTBQueue q) `shouldReturn` True

-- | Stand-in 'ApplyResult'; the drain never inspects fields.
sampleApplyResult :: ApplyResult
sampleApplyResult =
  ApplyResult
    { apPrices          = SMaybe.Nothing
    , apGovExpiresAfter = SMaybe.Nothing
    , apPoolsRegistered = Set.empty
    , apNewEpoch        = SMaybe.Nothing
    , apDeposits        = SMaybe.Nothing
    , apSlotDetails     = sampleSlotDetails
    , apStakeSlice      = Generic.NoSlices
    , apEvents          = []
    , apGovActionState  = Nothing
    , apDepositsMap     = emptyDepositsMap
    }

sampleSlotDetails :: SlotDetails
sampleSlotDetails =
  SlotDetails
    { sdSlotTime    = epochZero
    , sdCurrentTime = epochZero
    , sdEpochNo     = EpochNo 0
    , sdSlotNo      = SlotNo 0
    , sdEpochSlot   = 0
    , sdEpochSize   = EpochSize 21600
    }
  where
    epochZero = UTCTime (toEnum 0) (secondsToDiffTime 0)
