{-# LANGUAGE OverloadedStrings #-}

-- | Tracer backend constructors.
--
-- Provides concrete tracer implementations: stderr (human-readable),
-- JSON (structured), null (silent), and test (capture to IORef).
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
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import System.IO (Handle, hPutStrLn, stderr)

-- * Tracer constructors

-- | Human-readable tracer to stderr, filtered by minimum severity.
mkStdErrTracer :: Severity -> IO AppTracer
mkStdErrTracer minSeverity = pure $ Tracer $ \msg ->
  when (lmSeverity msg >= minSeverity) $
    hPutStrLn stderr (formatLogMsg msg)

-- | JSON lines tracer to a file handle, filtered by minimum severity.
mkJsonTracer :: Severity -> Handle -> IO AppTracer
mkJsonTracer minSeverity h = pure $ Tracer $ \msg ->
  when (lmSeverity msg >= minSeverity) $
    hPutStrLn h (formatLogMsg msg)  -- TODO: proper JSON encoding

-- | Silent tracer — discards all messages.
mkNullTracer :: AppTracer
mkNullTracer = nullTracer

-- | Test tracer — captures messages into an IORef for assertions.
mkTestTracer :: IORef [LogMsg] -> AppTracer
mkTestTracer ref = Tracer $ \msg ->
  modifyIORef' ref (msg :)

-- * Internal

formatLogMsg :: LogMsg -> [Char]
formatLogMsg msg =
  "[" <> show (lmSeverity msg) <> "] "
    <> toS (lmComponent msg) <> ": "
    <> toS (lmMessage msg)
