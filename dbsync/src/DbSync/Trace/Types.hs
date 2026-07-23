{-# LANGUAGE OverloadedStrings #-}

-- | Structured logging types for the application.
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

    -- * IO-level emission
  , logInfoIO
  , logWarnIO
  , logErrorIO
  ) where

import Cardano.Prelude

import qualified Data.Text as Text

import Control.Tracer (Tracer, traceWith)

-- * Types

-- | Log severity levels, ordered from least to most severe.
data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving stock (Eq, Ord, Show, Bounded, Enum)

-- | Source location for an 'AppError', captured from the throwing
-- helper's call site.
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
  , lmComponent :: !Text        -- ^ "IngestChainHistory", "LoaderStream", etc.
  , lmMessage   :: !Text
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

-- | Emit an 'Info' log line through an 'AppTracer'. Used by
-- IO-level helpers (boot, schema setup, resource shutdown) that
-- don't have a phase env in scope.
logInfoIO :: AppTracer -> Text -> Text -> IO ()
logInfoIO tracer component msg =
  traceWith tracer (LogMsg Info component msg)

-- | 'Warning'-level companion to 'logInfoIO'.
logWarnIO :: AppTracer -> Text -> Text -> IO ()
logWarnIO tracer component msg =
  traceWith tracer (LogMsg Warning component msg)

-- | 'Error'-level companion to 'logInfoIO'.
logErrorIO :: AppTracer -> Text -> Text -> IO ()
logErrorIO tracer component msg =
  traceWith tracer (LogMsg Error component msg)

-- | Extract the top frame of a 'CallStack' into 'SrcInfo'.
--
-- The function name comes from the first element of @getCallStack@'s
-- tuple (the name of the function that pushed the frame), not from
-- the 'SrcLoc'.
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
