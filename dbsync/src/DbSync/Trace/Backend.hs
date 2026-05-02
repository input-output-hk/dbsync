-- | Tracer backend constructors.
--
-- Provides concrete tracer implementations: stderr (human-readable),
-- JSON (structured), null (silent), and test (capture to IORef).
-- All production tracers include UTC timestamps.
--
-- Production handle-based tracers ('mkStdErrTracer', 'mkJsonTracer')
-- serialise writes through a per-tracer 'MVar' lock so concurrent
-- threads can't interleave their bytes on the buffered handle.
module DbSync.Trace.Backend
  ( -- * Tracer constructors
    mkStdErrTracer
  , mkJsonTracer
  , mkNullTracer
  , mkTestTracer
  ) where

import Cardano.Prelude hiding (hPutStrLn)

import Control.Tracer (Tracer (..), nullTracer)
import Data.IORef (IORef, modifyIORef')
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
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

-- | JSON lines tracer to a file handle, filtered by minimum severity.
mkJsonTracer :: Severity -> Handle -> IO AppTracer
mkJsonTracer minSeverity h = do
  lock <- newMVar ()
  pure $ Tracer $ \msg ->
    when (lmSeverity msg >= minSeverity) $ do
      ts <- formatTimestamp
      writeLine lock h (ts <> " " <> formatLogMsg msg)  -- TODO: proper JSON encoding

-- | Silent tracer — discards all messages.
mkNullTracer :: AppTracer
mkNullTracer = nullTracer

-- | Test tracer — captures messages into an IORef for assertions.
-- No timestamps — tests check message content, not timing.
mkTestTracer :: IORef [LogMsg] -> AppTracer
mkTestTracer ref = Tracer $ \msg ->
  modifyIORef' ref (msg :)

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
