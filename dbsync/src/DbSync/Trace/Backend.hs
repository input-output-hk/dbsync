-- | Tracer backend constructors.
--
-- Provides concrete tracer implementations: stderr (human-readable),
-- null (silent), and test (capture to IORef). The stderr tracer
-- prepends a UTC timestamp and serialises writes through an 'MVar'
-- lock so concurrent threads can't interleave their bytes on the
-- buffered handle.
module DbSync.Trace.Backend
  ( -- * Tracer constructors
    mkStdErrTracer
  , mkNullTracer
  , mkTestTracer

    -- * Phase-aware filter
  , withPhaseFilter
  ) where

import Cardano.Prelude hiding (hPutStrLn)

import Control.Tracer (Tracer (..), nullTracer)
import Data.IORef (IORef, modifyIORef')
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import DbSync.Db.Phase (SyncPhase, isFollowPath)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import System.IO (hPutStrLn)

-- * Tracer constructors

-- | Human-readable tracer to stderr, filtered by minimum severity.
-- Format: [2025-01-15 14:30:45.123 UTC] [Info] Component: message
mkStdErrTracer :: Severity -> IO AppTracer
mkStdErrTracer minSeverity = do
  lock <- newMVar ()
  pure $ Tracer $ \msg ->
    when (lmSeverity msg >= minSeverity) $ do
      ts <- formatTimestamp
      writeLine lock stderr (ts <> " " <> formatLogMsg msg)

-- | Silent tracer — discards all messages.
mkNullTracer :: AppTracer
mkNullTracer = nullTracer

-- | Test tracer — captures messages into an IORef for assertions.
-- No timestamps — tests check message content, not timing.
mkTestTracer :: IORef [LogMsg] -> AppTracer
mkTestTracer ref = Tracer $ \msg ->
  modifyIORef' ref (msg :)

-- * Phase-aware filter

-- | Drop Debug messages unless we're in a Follow phase. Info+ always
-- passes.
withPhaseFilter :: IO SyncPhase -> AppTracer -> AppTracer
withPhaseFilter readPhase inner =
  Tracer $ \msg ->
    if lmSeverity msg >= Info
      then traceWith inner msg
      else do
        phase <- readPhase
        when (isFollowPath phase) $ traceWith inner msg
  where
    traceWith (Tracer f) = f

-- * Internal

-- | Atomically write a single line to the handle. The 'MVar' makes
-- the whole @hPutStrLn@ critical-section visible to one thread at a
-- time so concurrent loggers can't interleave their bytes on the
-- buffered handle.
writeLine :: MVar () -> Handle -> [Char] -> IO ()
writeLine lock h line = withMVar lock $ \_ -> hPutStrLn h line

-- | Format current time as [YYYY-MM-DD HH:MM:SS.sss UTC]
formatTimestamp :: IO [Char]
formatTimestamp = do
  now <- getCurrentTime
  pure $ "[" <> formatTime defaultTimeLocale "%F %T%3Q %Z" now <> "]"

formatLogMsg :: LogMsg -> [Char]
formatLogMsg msg =
  "[" <> show (lmSeverity msg) <> "] "
    <> toS (lmComponent msg) <> ": "
    <> toS (lmMessage msg)
