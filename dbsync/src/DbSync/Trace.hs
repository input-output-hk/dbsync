{-# LANGUAGE OverloadedStrings #-}

-- | Structured logging convenience functions.
--
-- Re-exports 'AppTracer' and 'LogMsg' from "DbSync.Trace.Types",
-- plus convenience functions that work in any monad with 'HasTracer'.
module DbSync.Trace
  ( -- * Re-exports
    module DbSync.Trace.Types

    -- * Accessor class
  , HasTracer (..)

    -- * Convenience logging functions
  , logInfo
  , logWarning
  , logError
  , logDebug
  ) where

import Cardano.Prelude

import Control.Monad.Reader (MonadReader, asks)
import Control.Tracer (traceWith)
import DbSync.Trace.Types
import GHC.Stack (callStack)

-- | Access the tracer from any environment. Implemented per-env.
class HasTracer env where
  getTracer :: env -> AppTracer

-- * Convenience logging functions

-- | Log an informational message.
logInfo :: (MonadReader env m, HasTracer env, MonadIO m) => Text -> Text -> m ()
logInfo component msg = do
  tracer <- asks getTracer
  liftIO $ traceWith tracer (LogMsg Info component msg Nothing)

-- | Log a warning with source location.
logWarning :: (MonadReader env m, HasTracer env, MonadIO m, HasCallStack) => Text -> Text -> m ()
logWarning component msg = do
  tracer <- asks getTracer
  let srcInfo = captureCallSiteFromStack callStack
  liftIO $ traceWith tracer (LogMsg Warning component msg (Just srcInfo))

-- | Log an error with source location.
logError :: (MonadReader env m, HasTracer env, MonadIO m, HasCallStack) => Text -> Text -> m ()
logError component msg = do
  tracer <- asks getTracer
  let srcInfo = captureCallSiteFromStack callStack
  liftIO $ traceWith tracer (LogMsg Error component msg (Just srcInfo))

-- | Log a debug message.
logDebug :: (MonadReader env m, HasTracer env, MonadIO m) => Text -> Text -> m ()
logDebug component msg = do
  tracer <- asks getTracer
  liftIO $ traceWith tracer (LogMsg Debug component msg Nothing)

-- * Internal

captureCallSiteFromStack :: CallStack -> SrcInfo
captureCallSiteFromStack cs = case getCallStack cs of
  (_, loc) : _ ->
    SrcInfo
      { siFunction = show (srcLocStartLine loc)
      , siModule   = toS (srcLocModule loc)
      , siFile     = toS (srcLocFile loc)
      , siLine     = srcLocStartLine loc
      }
  [] ->
    SrcInfo
      { siFunction = "<unknown>"
      , siModule   = "<unknown>"
      , siFile     = "<unknown>"
      , siLine     = 0
      }
