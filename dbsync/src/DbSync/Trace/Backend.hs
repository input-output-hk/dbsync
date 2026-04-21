-- | Tracer backend constructors.
--
-- Provides concrete tracer implementations: stderr (human-readable),
-- JSON (structured), null (silent), and test (capture to IORef).
-- All production tracers include UTC timestamps.
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
import System.IO (Handle, hPutStrLn, stderr)

-- * Tracer constructors

-- | Human-readable tracer to stderr, filtered by minimum severity.
-- Format: [2025-01-15 14:30:45.123 UTC] [Info] Component: message
mkStdErrTracer :: Severity -> IO AppTracer
mkStdErrTracer minSeverity = pure $ Tracer $ \msg ->
  when (lmSeverity msg >= minSeverity) $ do
    ts <- formatTimestamp
    hPutStrLn stderr (ts <> " " <> formatLogMsg msg)

-- | JSON lines tracer to a file handle, filtered by minimum severity.
mkJsonTracer :: Severity -> Handle -> IO AppTracer
mkJsonTracer minSeverity h = pure $ Tracer $ \msg ->
  when (lmSeverity msg >= minSeverity) $ do
    ts <- formatTimestamp
    hPutStrLn h (ts <> " " <> formatLogMsg msg)  -- TODO: proper JSON encoding

-- | Silent tracer — discards all messages.
mkNullTracer :: AppTracer
mkNullTracer = nullTracer

-- | Test tracer — captures messages into an IORef for assertions.
-- No timestamps — tests check message content, not timing.
mkTestTracer :: IORef [LogMsg] -> AppTracer
mkTestTracer ref = Tracer $ \msg ->
  modifyIORef' ref (msg :)

-- * Internal

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
