{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for 'DbSync.Trace.Replay'.
module DbSync.Trace.ReplaySpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Slot (SlotNo (..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToDiffTime)

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Trace.Replay
  ( ReplayAdvance (..)
  , ReplayLog (..)
  , ReplayLogState (..)
  , ReplayProgress (..)
  , advanceReplay
  , progressLogInterval
  )

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
spec = describe "DbSync.Trace.Replay.advanceReplay" $ do

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
