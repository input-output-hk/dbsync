{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Consumer in-epoch pulse — a periodic Debug-level liveness log
-- emitted from a background sampler thread.
--
-- Same disabled/enabled gating shape as 'DbSync.Trace.Watchdog':
-- when the configured minimum severity is above 'Debug' the whole
-- subsystem short-circuits to a no-op. The hot-path 'bumpPulse'
-- call from the consumer is then a single constructor match and
-- nothing else.
--
-- When enabled, the sampler wakes every 'pulseInterval' seconds and
-- emits one line carrying:
--
--   * blocks processed since the last sample, rendered as both a
--     count and a rate;
--   * receiver queue depth (in / capacity);
--   * the fullest loader-stream queue (table name, current, capacity);
--   * the current consumer note from the 'Watchdog' (set via
--     'setConsumerNote').
--
-- The combination is enough to localise the steady-state idle
-- stretches observed inside an epoch: a near-zero rate with the
-- receiver queue at capacity points at downstream loader-stream
-- backpressure (PG-side); a near-zero rate with an empty receiver
-- queue points at upstream node starvation.
module DbSync.Trace.Pulse
  ( -- * Types
    Pulse (..)
  , PulseState (..)
  , HasPulse (..)

    -- * Construction
  , newPulse

    -- * Hot-path bump
  , bumpPulse

    -- * Sampler
  , runPulseIO
  , pulseInterval
  ) where

import Cardano.Prelude

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TBQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Text.Printf (printf)

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (Watchdog, readConsumerNote)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Pulse handle. 'PulseDisabled' is constructed when the configured
-- minimum severity is above 'Debug'; every hot-path call then
-- pattern-matches and returns immediately.
data Pulse
  = PulseDisabled
  | PulseEnabled !PulseState

-- | Internal state for an enabled 'Pulse'. The single mutable cell
-- is the rolling per-interval block counter; the sampler thread
-- read-and-resets it.
data PulseState = PulseState
  { psBlocks :: !(IORef Int)
  }

-- | Access the 'Pulse' handle from env. Symmetric to 'HasWatchdog'.
class HasPulse env where
  getPulse :: env -> Pulse

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Enabled iff the configured minimum severity admits 'Debug'.
newPulse :: Severity -> IO Pulse
newPulse minSeverity
  | minSeverity > Debug = pure PulseDisabled
  | otherwise           = PulseEnabled . PulseState <$> newIORef 0

-- ---------------------------------------------------------------------------
-- Hot-path bump
-- ---------------------------------------------------------------------------

-- | Record one block as processed by the consumer. No-op when the
-- 'Pulse' is disabled.
bumpPulse :: Pulse -> IO ()
bumpPulse PulseDisabled        = pure ()
bumpPulse (PulseEnabled st) =
  atomicModifyIORef' (psBlocks st) $ \ !n -> (n + 1, ())

-- ---------------------------------------------------------------------------
-- Sampler
-- ---------------------------------------------------------------------------

-- | Sampler interval in seconds. Twice the cadence of the watchdog
-- so the pulse can catch transitions inside a 70s epoch that the
-- coarser watchdog interval would smooth over.
pulseInterval :: Int
pulseInterval = 2

-- | Background sampler loop. Exits immediately when the 'Pulse' is
-- disabled, so the caller's enclosing 'withAsync' has a no-op child.
runPulseIO
  :: AppTracer
  -> Pulse
  -> Watchdog
  -> TBQueue a       -- ^ Receiver \/ consumer block queue
  -> Int             -- ^ Block queue capacity (for the @recvQ=X\/cap@ render)
  -> LoaderStream    -- ^ Loader stream (for per-table queue depths)
  -> IO ()
runPulseIO _      PulseDisabled                _  _      _   _  = pure ()
runPulseIO tracer (PulseEnabled st) wd blockQ recvCap ls = do
  start <- getCurrentTime
  loop start
  where
    loop !lastTime = do
      threadDelay (pulseInterval * 1_000_000)
      now    <- getCurrentTime
      blocks <- atomicModifyIORef' (psBlocks st) $ \n -> (0, n)
      recvQ  <- atomically (STM.lengthTBQueue blockQ)
      depths <- lsQueueDepths ls
      note   <- readConsumerNote wd
      let elapsed = diffUTCTime now lastTime
      emitPulse tracer blocks elapsed recvQ recvCap depths note
      loop now

-- | Render and emit a single pulse line.
emitPulse
  :: AppTracer
  -> Int                            -- ^ Blocks processed in interval
  -> NominalDiffTime                -- ^ Wall-clock interval length
  -> Natural                        -- ^ Receiver queue current depth
  -> Int                            -- ^ Receiver queue capacity
  -> [(Text, Natural, Natural)]     -- ^ Per-table loader queue (name, current, cap)
  -> Text                           -- ^ Current consumer note
  -> IO ()
emitPulse tracer blocks elapsed recvQ recvCap depths note =
  traceWith tracer $ LogMsg Debug "ConsumerPulse" line Nothing
  where
    rate :: Double
    rate
      | elapsed > 0 = fromIntegral blocks / realToFrac elapsed
      | otherwise   = 0

    line =
      Text.pack (printf "%d blk in %.2fs" blocks (realToFrac elapsed :: Double))
        <> " (" <> Text.pack (show (round rate :: Int)) <> " blk/s)"
        <> " | recvQ=" <> Text.pack (show recvQ)
                      <> "/" <> Text.pack (show recvCap)
        <> " | loaderQ peak=" <> renderPeak depths
        <> " | step=" <> note

-- | Render the fullest loader queue as @"name:cur/cap"@. Empty list
-- (no tables) is rendered as @"n/a"@. Comparison is by fraction of
-- capacity so the table closest to saturation wins, not just the
-- one with the biggest absolute depth.
renderPeak :: [(Text, Natural, Natural)] -> Text
renderPeak []       = "n/a"
renderPeak (x : xs) =
  let (n, cur, cap) = foldl' pickFuller x xs
  in n <> ":" <> Text.pack (show cur) <> "/" <> Text.pack (show cap)
  where
    pickFuller a@(_, ca, capA) b@(_, cb, capB)
      | fracOf ca capA >= fracOf cb capB = a
      | otherwise                        = b
    fracOf :: Natural -> Natural -> Double
    fracOf _ 0 = 0
    fracOf c k = fromIntegral c / fromIntegral k
