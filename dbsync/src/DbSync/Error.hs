{-# LANGUAGE OverloadedStrings #-}

-- | Application-wide error types with source location tracking.
--
-- Every 'AppError' constructor carries 'SrcInfo' so that log messages
-- and error reports include the source location where the error was raised.
-- Errors propagate through 'AppM' via 'throwIO' — no 'ExceptT' in the stack.
--
-- == Picking a thrower
--
-- Prefer the per-kind helpers ('throwDb', 'throwCheckpoint',
-- 'throwLedger', 'throwBlock', 'throwSchema', 'throwNetwork',
-- 'throwInternal') over the generic 'throwAppError': they read more
-- naturally at the call site and use 'withFrozenCallStack' so the
-- captured 'SrcInfo' points at the caller rather than at the helper
-- itself.
--
-- Use 'rethrowAs' at integration boundaries to wrap a third-party
-- exception in an 'AppError' with the right component tag plus the
-- original exception's display string.
module DbSync.Error
  ( -- * Types
    AppError (..)

    -- * Throwing — generic
  , throwAppError

    -- * Throwing — per kind
  , throwDb
  , throwCheckpoint
  , throwLedger
  , throwBlock
  , throwSchema
  , throwNetwork
  , throwInternal

    -- * Wrapping foreign exceptions
  , rethrowAs
  ) where

import Cardano.Prelude

import qualified Control.Exception as Exception
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Data.Text as Text

import DbSync.Trace.Types (SrcInfo, captureCallSite)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- * Throwing — generic
-- ---------------------------------------------------------------------------

-- | Throw an 'AppError', automatically capturing the call site.
--
-- Prefer the per-kind helpers below where possible — they read more
-- naturally and pass the constructor for you.
throwAppError :: (HasCallStack, MonadIO m) => (SrcInfo -> Text -> AppError) -> Text -> m a
throwAppError ctor msg = liftIO $ throwIO (ctor (captureCallSite callStack) msg)

-- ---------------------------------------------------------------------------
-- * Throwing — per kind
-- ---------------------------------------------------------------------------

-- Each of these uses 'withFrozenCallStack' so 'throwAppError' sees the
-- caller's 'CallStack' rather than the wrapper's, ensuring the captured
-- 'SrcInfo' identifies the actual error site in db-sync code rather
-- than this module.

-- | Raise an 'AppDatabaseError' with the caller's source location.
throwDb :: (HasCallStack, MonadIO m) => Text -> m a
throwDb msg = withFrozenCallStack (throwAppError AppDatabaseError msg)

-- | Raise an 'AppCheckpointError' with the caller's source location.
throwCheckpoint :: (HasCallStack, MonadIO m) => Text -> m a
throwCheckpoint msg = withFrozenCallStack (throwAppError AppCheckpointError msg)

-- | Raise an 'AppLedgerError' with the caller's source location.
throwLedger :: (HasCallStack, MonadIO m) => Text -> m a
throwLedger msg = withFrozenCallStack (throwAppError AppLedgerError msg)

-- | Raise an 'AppBlockError' with the caller's source location.
throwBlock :: (HasCallStack, MonadIO m) => Text -> m a
throwBlock msg = withFrozenCallStack (throwAppError AppBlockError msg)

-- | Raise an 'AppSchemaError' with the caller's source location.
throwSchema :: (HasCallStack, MonadIO m) => Text -> m a
throwSchema msg = withFrozenCallStack (throwAppError AppSchemaError msg)

-- | Raise an 'AppNetworkError' with the caller's source location.
throwNetwork :: (HasCallStack, MonadIO m) => Text -> m a
throwNetwork msg = withFrozenCallStack (throwAppError AppNetworkError msg)

-- | Raise an 'AppInternalError' with the caller's source location.
--
-- Reserved for programmer-error cases (\"this branch should be
-- unreachable\", \"called in the wrong phase\", etc.).
throwInternal :: (HasCallStack, MonadIO m) => Text -> m a
throwInternal msg = withFrozenCallStack (throwAppError AppInternalError msg)

-- ---------------------------------------------------------------------------
-- * Wrapping foreign exceptions
-- ---------------------------------------------------------------------------

-- | Run @action@; if it throws any 'SomeException', rethrow it as the
-- chosen 'AppError' kind with the supplied @context@ prepended to the
-- original exception's display string.
--
-- Use this at boundaries with third-party libraries (LSM, libpq,
-- network) so the caught exception inherits db-sync's component tag
-- and 'SrcInfo' rather than surfacing as a bare @SomeException@.
--
-- Synchronous exceptions only — async exceptions like
-- 'AsyncCancelled' propagate untouched, matching @safe-exceptions@'s
-- handling philosophy.
rethrowAs
  :: (HasCallStack, MonadUnliftIO m)
  => (SrcInfo -> Text -> AppError)
  -> Text
  -> m a
  -> m a
rethrowAs ctor context action =
  withFrozenCallStack $
    withRunInIO $ \run ->
      run action `Exception.catch` \(e :: Exception.SomeException) -> do
        case Exception.fromException e :: Maybe Exception.SomeAsyncException of
          Just _  -> Exception.throwIO e
          Nothing -> throwIO $
            ctor
              (captureCallSite callStack)
              (context <> ": " <> Text.pack (Exception.displayException e))
