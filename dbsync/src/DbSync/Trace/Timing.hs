{-# LANGUAGE OverloadedStrings #-}

-- | Timing helpers that wrap an 'IO' action with start/end trace
-- lines bracketing wall-clock duration. 'timed' carries the row
-- count returned by the action; 'timed_' is for actions that
-- don't return one.
--
-- Both emit at 'Info' so the per-step duration is visible at the
-- default log level — operators chasing a long-running phase
-- always want to know which sub-step is in flight, independent of
-- whether the watchdog / per-epoch diagnostics (which gate on
-- 'Debug') are enabled.
module DbSync.Trace.Timing
  ( timedTrace
  , timedTrace_
  , withHeartbeat

    -- * Formatting helpers
  , fmtDuration
  , fmtRows
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import qualified Data.Text as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)

import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Run an action while a sidecar thread emits a heartbeat trace on
-- the supplied component every @intervalSeconds@ until the action
-- returns. Each heartbeat appends the elapsed wall-clock so a stalled
-- operation reads as a stalled timer.
--
-- Use for opaque long-running calls with no internal progress hook
-- — e.g. the boot-time LSM snapshot load or any DDL that may take
-- minutes on a mainnet-shaped DB. The heartbeat runs at 'Info' so
-- it's visible at the default log level.
withHeartbeat
  :: AppTracer
  -> Text   -- ^ Component label (the @[Info] <Component>:@ tag)
  -> Text   -- ^ Message prefix; the elapsed-time suffix is appended
  -> Int    -- ^ Seconds between heartbeats
  -> IO a
  -> IO a
withHeartbeat tracer component prefix intervalSeconds action = do
  start <- getCurrentTime
  withAsync (heartbeat start) $ \_ -> action
  where
    heartbeat start = forever $ do
      threadDelay (intervalSeconds * 1_000_000)
      now <- getCurrentTime
      traceWith tracer $ LogMsg Info component
        ( prefix <> " (" <> fmtDuration (realToFrac (diffUTCTime now start))
            <> " elapsed)"
        ) Nothing

-- | Run an action returning a row count; emit @"<label>: starting"@
-- before and @"<label>: <N> rows in <T>"@ after.
timedTrace :: AppTracer -> Text -> Text -> IO Int64 -> IO Int64
timedTrace tracer component label action = do
  emitTrace tracer component (label <> ": starting")
  start <- getCurrentTime
  rows  <- action
  end   <- getCurrentTime
  emitTrace tracer component $
    label <> ": " <> fmtRows rows <> " rows in "
      <> fmtDuration (realToFrac (diffUTCTime end start))
  pure rows

-- | Like 'timedTrace' but for actions without a row count.
timedTrace_ :: AppTracer -> Text -> Text -> IO a -> IO a
timedTrace_ tracer component label action = do
  emitTrace tracer component (label <> ": starting")
  start <- getCurrentTime
  a     <- action
  end   <- getCurrentTime
  emitTrace tracer component $
    label <> ": complete in "
      <> fmtDuration (realToFrac (diffUTCTime end start))
  pure a

emitTrace :: AppTracer -> Text -> Text -> IO ()
emitTrace tracer component msg =
  traceWith tracer $ LogMsg Info component msg Nothing

-- ---------------------------------------------------------------------------
-- * Formatting
-- ---------------------------------------------------------------------------

-- | Render seconds as @1.23s@ / @2m 7s@ / @1h 14m@.
fmtDuration :: Double -> Text
fmtDuration secs
  | secs < 60    = Text.pack (printf "%.2fs" secs)
  | secs < 3600  =
      let t = round secs :: Int
      in show (t `div` 60) <> "m " <> show (t `mod` 60) <> "s"
  | otherwise    =
      let t = round secs :: Int
      in show (t `div` 3600) <> "h " <> show ((t `mod` 3600) `div` 60) <> "m"

-- | Comma-separate large row counts so they read at a glance.
fmtRows :: Int64 -> Text
fmtRows n
  | n < 0     = "-" <> fmtRows (abs n)
  | n < 1000  = show n
  | otherwise =
      let s :: [Char]
          s = show (n :: Int64)
          len = length s
          (prefix, rest) = splitAt (len `mod` 3) s
          chunks = chunksOf3 rest
          allChunks = if null prefix then chunks else prefix : chunks
      in Text.pack (intercalate "," allChunks)
  where
    chunksOf3 :: [a] -> [[a]]
    chunksOf3 [] = []
    chunksOf3 xs = let (h, t) = splitAt 3 xs in h : chunksOf3 t
