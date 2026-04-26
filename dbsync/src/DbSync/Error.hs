{-# LANGUAGE OverloadedStrings #-}

-- | Application-wide error types with source location tracking.
--
-- Every 'AppError' constructor carries 'SrcInfo' so that log messages
-- and error reports include the source location where the error was raised.
-- Errors propagate through 'AppM' via 'throwIO' — no 'ExceptT' in the stack.
module DbSync.Error
  ( -- * Types
    AppError (..)

    -- * Throwing
  , throwAppError
  , captureCallSite
  ) where

import Cardano.Prelude

import DbSync.Trace.Types (SrcInfo (..))

-- * Types

-- | Application-wide error type. Every constructor carries source location.
data AppError
  = AppDatabaseError   !SrcInfo !Text   -- ^ PostgreSQL connection or query failure
  | AppCheckpointError !SrcInfo !Text   -- ^ Checkpoint read/write failure
  | AppLedgerError     !SrcInfo !Text   -- ^ Ledger state application failure
  | AppBlockError      !SrcInfo !Text   -- ^ Block parsing failure
  | AppSchemaError     !SrcInfo !Text   -- ^ Schema generation or migration failure
  | AppNetworkError    !SrcInfo !Text   -- ^ ChainSync connection failure
  | AppInternalError   !SrcInfo !Text   -- ^ Programmer error (wrong phase, etc.)
  deriving stock (Show)

instance Exception AppError

-- * Throwing

-- | Throw an 'AppError', automatically capturing the call site.
throwAppError :: (HasCallStack, MonadIO m) => (SrcInfo -> Text -> AppError) -> Text -> m a
throwAppError ctor msg = liftIO $ throwIO (ctor (captureCallSite callStack) msg)

-- | Extract the caller's source location from the GHC call stack.
captureCallSite :: CallStack -> SrcInfo
captureCallSite cs = case getCallStack cs of
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
