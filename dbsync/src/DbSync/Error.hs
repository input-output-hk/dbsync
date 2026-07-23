{-# LANGUAGE OverloadedStrings #-}

-- | Application-wide error type with source-location tracking.
--
-- Every 'AppError' constructor carries 'SrcInfo'. Errors propagate
-- through 'AppM' via 'throwIO' — there is no 'ExceptT' in the stack.
-- Prefer the per-kind throwers over 'throwAppError' for readability;
-- use 'rethrowAs' at boundaries with third-party libraries.
module DbSync.Error
  ( -- * Types
    AppError (..)
  , BlockAnnotation (..)
  , renderBlockAnnotation

    -- * Throwing — generic
  , throwAppError

    -- * Throwing — per kind
  , throwDb
  , throwSyncState
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
import Control.Exception.Annotation (ExceptionAnnotation (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import DbSync.Trace.Types (SrcInfo, captureCallSite)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Application-wide error type.
data AppError
  = AppDatabaseError   !SrcInfo !Text   -- ^ PostgreSQL connection or query failure
  | AppSyncStateError !SrcInfo !Text   -- ^ Sync-state row read/write failure
  | AppLedgerError     !SrcInfo !Text   -- ^ Ledger state application failure
  | AppBlockError      !SrcInfo !Text   -- ^ Block parsing failure
  | AppSchemaError     !SrcInfo !Text   -- ^ Schema generation or migration failure
  | AppNetworkError    !SrcInfo !Text   -- ^ ChainSync connection failure
  | AppInternalError   !SrcInfo !Text   -- ^ Programmer error (wrong phase, etc.)
  deriving stock (Show)

instance Exception AppError

-- ---------------------------------------------------------------------------
-- * Block context annotation
-- ---------------------------------------------------------------------------

-- | Attached with 'Control.Exception.annotateIO' around per-block
-- processing so a crash names the block in flight. Rendered by
-- "DbSync.Error.Render".
data BlockAnnotation = BlockAnnotation
  { baSlot    :: !Word64
  , baBlockNo :: !Word64
  , baHash    :: !ByteString
  }

instance ExceptionAnnotation BlockAnnotation where
  displayExceptionAnnotation = toS . renderBlockAnnotation

renderBlockAnnotation :: BlockAnnotation -> Text
renderBlockAnnotation (BlockAnnotation slot blockNo hash) =
  "while processing block " <> show blockNo
    <> " (slot " <> show slot
    <> ", hash " <> TE.decodeUtf8 (Base16.encode (BS.take 8 hash)) <> "…)"

-- ---------------------------------------------------------------------------
-- * Throwing — generic
-- ---------------------------------------------------------------------------

-- | Throw an 'AppError', capturing the call site.
throwAppError :: (HasCallStack, MonadIO m) => (SrcInfo -> Text -> AppError) -> Text -> m a
throwAppError ctor msg = liftIO $ throwIO (ctor (captureCallSite callStack) msg)

-- ---------------------------------------------------------------------------
-- * Throwing — per kind
-- ---------------------------------------------------------------------------

-- Each helper freezes the call stack so the captured 'SrcInfo' points
-- at the caller, not at this module.

throwDb :: (HasCallStack, MonadIO m) => Text -> m a
throwDb msg = withFrozenCallStack (throwAppError AppDatabaseError msg)

throwSyncState :: (HasCallStack, MonadIO m) => Text -> m a
throwSyncState msg = withFrozenCallStack (throwAppError AppSyncStateError msg)

throwLedger :: (HasCallStack, MonadIO m) => Text -> m a
throwLedger msg = withFrozenCallStack (throwAppError AppLedgerError msg)

throwBlock :: (HasCallStack, MonadIO m) => Text -> m a
throwBlock msg = withFrozenCallStack (throwAppError AppBlockError msg)

throwSchema :: (HasCallStack, MonadIO m) => Text -> m a
throwSchema msg = withFrozenCallStack (throwAppError AppSchemaError msg)

throwNetwork :: (HasCallStack, MonadIO m) => Text -> m a
throwNetwork msg = withFrozenCallStack (throwAppError AppNetworkError msg)

-- | Reserved for programmer-error cases (unreachable branches, called
-- in the wrong phase, etc.).
throwInternal :: (HasCallStack, MonadIO m) => Text -> m a
throwInternal msg = withFrozenCallStack (throwAppError AppInternalError msg)

-- ---------------------------------------------------------------------------
-- * Wrapping foreign exceptions
-- ---------------------------------------------------------------------------

-- | Run @action@; rethrow any synchronous 'SomeException' as the
-- chosen 'AppError' kind, prepending @context@ to the original
-- exception's display string. Async exceptions propagate untouched.
rethrowAs
  :: (HasCallStack, MonadUnliftIO m)
  => (SrcInfo -> Text -> AppError)
  -> Text
  -> m a
  -> m a
rethrowAs ctor context action =
  withFrozenCallStack $
    withRunInIO $ \run ->
      run action `Exception.catchNoPropagate` \(ewc :: Exception.ExceptionWithContext Exception.SomeException) ->
        let Exception.ExceptionWithContext _ e = ewc
        in case Exception.fromException e :: Maybe Exception.SomeAsyncException of
             -- Async exceptions propagate untouched, keeping their context.
             Just _  -> Exception.rethrowIO ewc
             -- Wrap synchronous failures, nesting the original as the cause.
             Nothing ->
               Exception.annotateIO (Exception.WhileHandling (Exception.toException ewc)) $
                 throwIO $
                   ctor
                     (captureCallSite callStack)
                     (context <> ": " <> Text.pack (Exception.displayException e))
