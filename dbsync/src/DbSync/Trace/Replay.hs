{-# LANGUAGE OverloadedStrings #-}

-- | Pure state machine for the replay-progress log, shared by the
-- Ingest and Follow resume paths.
--
-- A resume can land on a snapshot below @last_committed_slot@. The
-- ledger then replays the blocks in between while the consumer skips
-- its PG-write path, because those rows are already in PG. This
-- module decides when that silent window emits a progress line.
module DbSync.Trace.Replay
  ( -- * State
    ReplayLogState (..)
  , ReplayProgress (..)
  , ReplayAdvance (..)
  , ReplayLog (..)

    -- * Stepping the state machine
  , advanceReplay
  , progressLogInterval

    -- * Rendering helpers
  , renderReplayPercent
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (SlotNo (..))
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)

-- ---------------------------------------------------------------------------
-- * State
-- ---------------------------------------------------------------------------

-- | Drives the @LedgerReplay@ log channel across a replay window.
data ReplayLogState
  = NoReplay
    -- ^ No replay configured, or the window has been exited.
  | ReplayPending
    -- ^ Replay configured; no block observed yet.
  | InReplay !ReplayProgress
    -- ^ Inside the replay window; counters drive log cadence.
  deriving stock (Eq, Show)

-- | Block counter and log-cadence timestamps carried inside 'InReplay'.
data ReplayProgress = ReplayProgress
  { rpStartTime     :: !UTCTime
  , rpBlocksApplied :: !Word64
  , rpLastLogTime   :: !UTCTime
  }
  deriving stock (Eq, Show)

-- | Result of advancing 'ReplayLogState' for one received block.
data ReplayAdvance = ReplayAdvance
  { raNewState :: !ReplayLogState
  , raLog      :: !ReplayLog
  }
  deriving stock (Eq, Show)

-- | Log directive produced by 'advanceReplay'. The caller emits the
-- trace; keeping the decision pure makes it trivial to unit-test.
data ReplayLog
  = ReplayLogNothing
  | ReplayLogProgress !Word64
    -- ^ Emit a progress line — \"applied @N@ blocks so far\".
  | ReplayLogComplete !Word64 !NominalDiffTime
    -- ^ Emit a completion line — \"@N@ blocks replayed in @T@s\".
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Stepping the state machine
-- ---------------------------------------------------------------------------

-- | Wall-clock cadence between progress lines. Five seconds keeps
-- short replays silent while still flagging liveness on long ones.
progressLogInterval :: NominalDiffTime
progressLogInterval = 5

-- | Advance the state machine for one received block. A 'Nothing'
-- boundary means no replay is configured.
advanceReplay
  :: SlotNo
  -> Maybe SlotNo
  -> UTCTime
  -> ReplayLogState
  -> ReplayAdvance
advanceReplay _    Nothing  _   s =
  ReplayAdvance s ReplayLogNothing
advanceReplay slot (Just bs) now s =
  let inReplay = slot <= bs
  in case s of
       NoReplay ->
         ReplayAdvance NoReplay ReplayLogNothing
       ReplayPending
         | inReplay  ->
             let p = ReplayProgress
                       { rpStartTime     = now
                       , rpBlocksApplied = 1
                       , rpLastLogTime   = now
                       }
             in ReplayAdvance (InReplay p) ReplayLogNothing
         | otherwise ->
             -- First block is already past the boundary: a zero-block
             -- window. Go straight to 'NoReplay' and log nothing.
             ReplayAdvance NoReplay ReplayLogNothing
       InReplay p
         | inReplay ->
             let p' = p { rpBlocksApplied = rpBlocksApplied p + 1 }
                 elapsedSinceLog = diffUTCTime now (rpLastLogTime p)
             in if elapsedSinceLog >= progressLogInterval
                  then ReplayAdvance
                         (InReplay p' { rpLastLogTime = now })
                         (ReplayLogProgress (rpBlocksApplied p'))
                  else ReplayAdvance (InReplay p') ReplayLogNothing
         | otherwise ->
             let totalElapsed = diffUTCTime now (rpStartTime p)
             in ReplayAdvance NoReplay
                  (ReplayLogComplete (rpBlocksApplied p) totalElapsed)

-- ---------------------------------------------------------------------------
-- * Rendering helpers
-- ---------------------------------------------------------------------------

-- | Render a progress percentage of the form @\" [37%]\"@, or the
-- empty string when the bounds are missing or the window has zero
-- width. Measures /slots/, not /blocks/: Cardano slots can be empty,
-- so the total block count is unknown up front.
renderReplayPercent :: Maybe SlotNo -> Maybe SlotNo -> SlotNo -> Text
renderReplayPercent (Just (SlotNo start)) (Just (SlotNo endBound)) (SlotNo cur)
  | endBound > start =
      let span'   = endBound - start
          done
            | cur > endBound = span'
            | cur > start = cur - start
            | otherwise = 0
          pct     = (done * 100) `div` span'
      in " [" <> show pct <> "%]"
renderReplayPercent _ _ _ = ""
