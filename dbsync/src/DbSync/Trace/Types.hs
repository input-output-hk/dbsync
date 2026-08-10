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

data LogMsg = LogMsg
  { lmSeverity  :: !Severity
  , lmComponent :: !Text  -- ^ "IngestChainHistory", "LoaderStream", etc.
  , lmMessage   :: !Text  -- ^ Rendered line, without severity or component.
  }
  deriving stock (Show)

type AppTracer = Tracer IO LogMsg

-- | Parse the config's @logging.level@ string. Case-insensitive. An
-- unknown value falls back to 'Info', so a typo cannot break the boot.
severityFromText :: Text -> Severity
severityFromText t = case Text.toLower (Text.strip t) of
  "debug"   -> Debug
  "info"    -> Info
  "warning" -> Warning
  "warn"    -> Warning
  "error"   -> Error
  _         -> Info

-- | For boot, schema setup and shutdown code that has no phase env.
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

-- | Extract the top frame of a 'CallStack'. The function name comes
-- from the first element of the @getCallStack@ tuple, which names the
-- function that pushed the frame; the 'SrcLoc' does not carry it.
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
