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
  ) where

import Cardano.Prelude

import Control.Tracer (Tracer)

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
