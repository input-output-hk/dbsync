-- | Timing helpers that wrap an action with start/end trace lines
-- bracketing wall-clock duration. 'timed' carries the row count
-- returned by the action; 'timed_' is for actions that don't.
--
-- Both emit at 'Info' so the per-step duration is visible at the
-- default log level — operators chasing a long-running phase always
-- want to know which sub-step is in flight, independent of whether
-- the watchdog / per-epoch diagnostics (which gate on 'Debug') are
-- enabled.
--
-- Each helper has two flavours:
--
--   * @withHeartbeat@ / @timedTrace@ / @timedTrace_@ — polymorphic
--     over an 'AppM env' that satisfies 'HasTracer'. New code.
--   * @*IO@ siblings — take the tracer explicitly. For boot code
--     that hasn't built an env yet.
module DbSync.Trace.Timing
  ( timedTrace
  , timedTrace_
  , withHeartbeat
  , timedTraceIO
  , timedTraceIO_
  , withHeartbeatIO

    -- * Formatting helpers
  , fmtDuration
  , fmtRows
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Tracer (traceWith)
import qualified Data.Text as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)

import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Env-aware variants
-- ---------------------------------------------------------------------------

-- | Run an action while a sidecar thread emits a heartbeat trace
-- every @intervalSeconds@. Each heartbeat appends elapsed wall-clock
-- so a stalled operation reads as a stalled timer.
withHeartbeat
  :: (HasTracer env, MonadReader env m, MonadUnliftIO m)
  => Text     -- ^ Component label
  -> Text     -- ^ Message prefix; the elapsed-time suffix is appended
  -> Int      -- ^ Seconds between heartbeats
  -> m a
  -> m a
withHeartbeat component prefix intervalSeconds action = do
  tracer <- asks getTracer
  withRunInIO $ \run ->
    withHeartbeatIO tracer component prefix intervalSeconds (run action)

-- | Run an action returning a row count; emit @"<label>: starting"@
-- before and @"<label>: <N> rows in <T>"@ after.
timedTrace
  :: (HasTracer env, MonadReader env m, MonadIO m)
  => Text -> Text -> m Int64 -> m Int64
timedTrace component label action = do
  tracer <- asks getTracer
  liftIO $ emitTrace tracer component (label <> ": starting")
  start <- liftIO getCurrentTime
  rows  <- action
  end   <- liftIO getCurrentTime
  liftIO $ emitTrace tracer component $
    label <> ": " <> fmtRows rows <> " rows in "
      <> fmtDuration (realToFrac (diffUTCTime end start))
  pure rows

-- | Like 'timedTrace' but for actions without a row count.
timedTrace_
  :: (HasTracer env, MonadReader env m, MonadIO m)
  => Text -> Text -> m a -> m a
timedTrace_ component label action = do
  tracer <- asks getTracer
  liftIO $ emitTrace tracer component (label <> ": starting")
  start <- liftIO getCurrentTime
  a     <- action
  end   <- liftIO getCurrentTime
  liftIO $ emitTrace tracer component $
    label <> ": complete in "
      <> fmtDuration (realToFrac (diffUTCTime end start))
  pure a

-- ---------------------------------------------------------------------------
-- * Explicit-tracer variants (for boot code without an env)
-- ---------------------------------------------------------------------------

withHeartbeatIO :: AppTracer -> Text -> Text -> Int -> IO a -> IO a
withHeartbeatIO tracer component prefix intervalSeconds action = do
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

timedTraceIO :: AppTracer -> Text -> Text -> IO Int64 -> IO Int64
timedTraceIO tracer component label action = do
  emitTrace tracer component (label <> ": starting")
  start <- getCurrentTime
  rows  <- action
  end   <- getCurrentTime
  emitTrace tracer component $
    label <> ": " <> fmtRows rows <> " rows in "
      <> fmtDuration (realToFrac (diffUTCTime end start))
  pure rows

timedTraceIO_ :: AppTracer -> Text -> Text -> IO a -> IO a
timedTraceIO_ tracer component label action = do
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
