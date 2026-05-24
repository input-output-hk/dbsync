{-# LANGUAGE OverloadedStrings #-}

-- | Shared replay-progress state machine.
--
-- When a resume lands on a snapshot below @last_committed_slot@ the
-- in-RAM ledger has to replay every block in between before normal
-- processing can resume. During that window the consumer skips its
-- PG-write path (rows are already in PG) and the ledger worker
-- advances the ledger via the chainsync receiver\'s fan-out.
--
-- This module is the pure decision layer for the user-facing progress
-- log: a small state machine, advanced once per received block, that
-- decides when to emit \"applied @N@ blocks\" / \"replay complete\"
-- lines. Effects are pushed to the caller: the consumer mutates an
-- @IORef ReplayLogState@ and emits any indicated trace.
--
-- Used by both the Ingest 'BootResume' path (catching up from the
-- previous epoch boundary\'s snapshot) and the Follow restart path
-- (catching up from a snapshot that lagged the consumer\'s commits).
-- Identical log shape on both.
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

-- | State machine driving the @LedgerReplay@ log channel during a
-- replay window. Surfaces liveness of an otherwise silent catch-up:
-- inside the window the consumer skips its normal per-block trace.
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

-- | Advance the replay-log state machine given the just-arrived
-- block\'s slot, the resume boundary (@'Nothing'@ = no replay) and
-- the current wall-clock time. Pure; the caller mutates the IORef
-- and emits any indicated trace.
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
             -- First block already past the boundary — degenerate
             -- replay window of zero blocks. Skip straight to
             -- 'NoReplay' without firing any log.
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

-- | Render a slot-progress percentage of the form @\" [37%]\"@.
-- Empty string when bounds are missing or the window has zero
-- width. Uses /slot/ progress, not /block/ progress, since Cardano
-- slots can be empty so the total block count is unknown up front.
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
