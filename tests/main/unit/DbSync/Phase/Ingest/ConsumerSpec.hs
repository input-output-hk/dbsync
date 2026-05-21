{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for 'DbSync.Phase.Ingest.Consumer'.
module DbSync.Phase.Ingest.ConsumerSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Control.Concurrent.STM (newTVarIO)
import qualified Control.Concurrent.STM as STM
import Data.IORef (newIORef, writeIORef)

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, secondsToDiffTime)

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import DbSync.Phase.Ingest.Consumer
  ( ReplayAdvance (..)
  , ReplayLog (..)
  , ReplayLogState (..)
  , ReplayProgress (..)
  , advanceReplay
  , ingestRollbackPanicMessage
  , progressLogInterval
  , renderBoundaryPercent
  , rollbackBoundaryReached
  )

import qualified Data.Text as T

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
  rollbackBoundarySpec
  renderBoundaryPercentSpec
  ingestRollbackPanicSpec

advanceReplaySpec :: Spec
advanceReplaySpec = describe "DbSync.Phase.Ingest.Consumer.advanceReplay" $ do

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

rollbackBoundarySpec :: Spec
rollbackBoundarySpec = describe "DbSync.Phase.Ingest.Consumer.rollbackBoundaryReached" $ do
  it "returns False when no block has been processed" $ do
    lastRef     <- newIORef Nothing
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

  it "returns False when the receiver hasn't seen a tip yet" $ do
    lastRef     <- newIORef (Just (50, 50, BS.empty))
    boundaryVar <- newTVarIO Nothing
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

  it "returns False when neither ref is set" $ do
    lastRef     <- newIORef Nothing
    boundaryVar <- newTVarIO Nothing
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

  it "returns False when the last block is below the boundary" $ do
    lastRef     <- newIORef (Just (1_000, 100, BS.empty))
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

  it "returns True when the last block equals the boundary" $ do
    lastRef     <- newIORef (Just (1_000, 200, BS.empty))
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` True

  it "returns True when the last block is past the boundary" $ do
    lastRef     <- newIORef (Just (1_000, 250, BS.empty))
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` True

  it "reflects updates to either ref" $ do
    lastRef     <- newIORef (Just (1_000, 100, BS.empty))
    boundaryVar <- newTVarIO (Just (BlockNo 200))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False
    -- New boundary arrives; same last block still in front of it.
    STM.atomically $ STM.writeTVar boundaryVar (Just (BlockNo 90))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` True
    -- Boundary moves back; last block still ahead.
    writeIORef lastRef (Just (1_000, 89, BS.empty))
    rollbackBoundaryReached lastRef boundaryVar `shouldReturn` False

renderBoundaryPercentSpec :: Spec
renderBoundaryPercentSpec = describe "DbSync.Phase.Ingest.Consumer.renderBoundaryPercent" $ do
  let k = 2160  -- mainnet security parameter

  it "renders empty when the rollback boundary is not yet known" $
    renderBoundaryPercent Nothing k (Just 100) `shouldBe` ""

  it "renders empty when no block has been processed yet" $
    renderBoundaryPercent (Just (BlockNo 9_000_000)) k Nothing `shouldBe` ""

  it "renders empty when both inputs are missing" $
    renderBoundaryPercent Nothing k Nothing `shouldBe` ""

  it "renders 0% at genesis with a real tip" $
    renderBoundaryPercent (Just (BlockNo 9_000_000)) k (Just 0)
      `shouldBe` " | (~0.00%)"

  it "renders ~50% halfway to tip" $
    -- tip = 9_000_000 + 2160 = 9_002_160; half = 4_501_080
    renderBoundaryPercent (Just (BlockNo 9_000_000)) k (Just 4_501_080)
      `shouldBe` " | (~50.00%)"

  it "approaches 100% just below tip but never reaches it during Ingest" $ do
    -- At the rollback boundary we exit Ingest; pct = boundary / (boundary+k)
    let pct = renderBoundaryPercent (Just (BlockNo 9_000_000)) k (Just 9_000_000)
    pct `shouldBe` " | (~99.98%)"

  it "clamps to 100% when current exceeds tip" $
    -- Defensive: receiver might publish a stale boundary while consumer races ahead.
    renderBoundaryPercent (Just (BlockNo 100)) k (Just 999_999)
      `shouldBe` " | (~100.00%)"

  it "renders 100% when current equals tip exactly" $
    renderBoundaryPercent (Just (BlockNo 1000)) k (Just (1000 + k))
      `shouldBe` " | (~100.00%)"

ingestRollbackPanicSpec :: Spec
ingestRollbackPanicSpec = describe "DbSync.Phase.Ingest.Consumer.ingestRollbackPanicMessage" $ do
  it "names the offending rollback point in the panic text" $ do
    let msg = ingestRollbackPanicMessage ("some-rollback-point" :: Text)
    T.isInfixOf "IngestChainHistory" msg `shouldBe` True
    T.isInfixOf "some-rollback-point"  msg `shouldBe` True
    T.isInfixOf "k-safety violation"   msg `shouldBe` True


