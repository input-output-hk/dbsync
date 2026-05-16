{-# LANGUAGE OverloadedStrings #-}

-- | Structured logging types for the application.
--
-- Defines the core logging types: 'Severity', 'LogMsg', 'SrcInfo',
-- and the 'AppTracer' type alias used throughout the application.
module DbSync.Trace.Types
  ( -- * Types
    Severity (..)
  , LogMsg (..)
  , SrcInfo (..)
  , AppTracer

    -- * Severity parsing
  , severityFromText

    -- * Source-location capture
  , captureCallSite

    -- * Thread-exit logging
  , logThreadExit
  ) where

import Cardano.Prelude

import qualified Data.Text as Text

import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Tracer (Tracer, traceWith)

-- * Types

-- | Log severity levels, ordered from least to most severe.
data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving stock (Eq, Ord, Show, Bounded, Enum)

-- | Source location captured from HasCallStack.
-- Populated automatically for Warning and Error level messages.
data SrcInfo = SrcInfo
  { siFunction :: !Text
  , siModule   :: !Text
  , siFile     :: !Text
  , siLine     :: !Int
  }
  deriving stock (Eq, Show)

-- | Structured log message with severity, component, and optional source location.
data LogMsg = LogMsg
  { lmSeverity  :: !Severity
  , lmComponent :: !Text        -- ^ "IngestChainHistory", "CopyWriter", etc.
  , lmMessage   :: !Text
  , lmSrcInfo   :: !(Maybe SrcInfo)  -- ^ populated for Warning and Error
  }
  deriving stock (Show)

-- | The tracer type used throughout the application — contra-tracer.
type AppTracer = Tracer IO LogMsg

-- | Parse the profile's @logging.level@ string. Case-insensitive;
-- unrecognised values fall back to 'Info' so a typo doesn't break
-- the boot.
severityFromText :: Text -> Severity
severityFromText t = case Text.toLower (Text.strip t) of
  "debug"   -> Debug
  "info"    -> Info
  "warning" -> Warning
  "warn"    -> Warning
  "error"   -> Error
  _         -> Info

-- | Log a background-thread exit. 'AsyncCancelled' is the normal
-- shutdown signal — log at 'Info' so it doesn't pollute the
-- operator's view of real failures. Any other exception is a real
-- crash and logs at 'Error'.
logThreadExit :: Text -> SomeException -> AppTracer -> IO ()
logThreadExit component e tracer = case fromException e of
  Just AsyncCancelled ->
    traceWith tracer $ LogMsg Info component
      "stopped (cancelled during shutdown)" Nothing
  Nothing ->
    traceWith tracer $ LogMsg Error component
      ("crashed: " <> show e) Nothing

-- | Extract the top frame of a 'CallStack' into 'SrcInfo'.
--
-- The function name comes from the first element of @getCallStack@'s
-- tuple (the name of the function that pushed the frame), not from
-- the 'SrcLoc' — which is what the previous in-line copies of this
-- helper mistakenly used.
captureCallSite :: CallStack -> SrcInfo
captureCallSite cs = case getCallStack cs of
  (fn, loc) : _ ->
    SrcInfo
      { siFunction = toS fn
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
