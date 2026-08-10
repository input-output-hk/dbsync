-- | Human-readable rendering of crashes and 'AppError's for the log.
--
-- The one-line summary comes from the 'AppError': kind, source
-- location, message. The block context, IPE backtrace and nested
-- cause come from the exception's 'ExceptionContext' annotations.
module DbSync.Error.Render
  ( renderAppError
  , renderCrash
  , logThreadExit
  ) where

import Cardano.Prelude

import Control.Concurrent.Async (AsyncCancelled (..), ExceptionInLinkedThread (..))
import qualified Control.Exception as Exception
import Control.Exception.Backtrace (Backtraces, displayBacktraces)
import Control.Exception.Context (getExceptionAnnotations)
import Control.Tracer (traceWith)
import qualified Data.Text as Text

import DbSync.Error (AppError (..), BlockAnnotation, renderBlockAnnotation)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..), SrcInfo (..))

-- ---------------------------------------------------------------------------
-- * AppError summary
-- ---------------------------------------------------------------------------

-- | One-line summary: @\<kind\> at \<file\>:\<line\> (\<function\>): \<message\>@.
renderAppError :: AppError -> Text
renderAppError ae =
  label <> " at " <> siFile si <> ":" <> show (siLine si)
    <> " (" <> siFunction si <> "): " <> msg
  where
    (label, si, msg) = appErrorParts ae

appErrorParts :: AppError -> (Text, SrcInfo, Text)
appErrorParts = \case
  AppDatabaseError  si m -> ("database error", si, m)
  AppSyncStateError si m -> ("sync-state error", si, m)
  AppLedgerError    si m -> ("ledger error", si, m)
  AppBlockError     si m -> ("block error", si, m)
  AppSchemaError    si m -> ("schema error", si, m)
  AppNetworkError   si m -> ("network error", si, m)
  AppInternalError  si m -> ("internal error", si, m)

-- ---------------------------------------------------------------------------
-- * Crash rendering
-- ---------------------------------------------------------------------------

-- | Render a crashed thread's (or the top-level) exception: a one-line
-- summary followed by indented block context, IPE backtrace, and the
-- summary of any 'WhileHandling' cause.
renderCrash :: Exception.SomeException -> Text
renderCrash outer =
  Text.intercalate "\n" (summary : map ("    " <>) body)
  where
    e       = peelLinked outer
    ctx     = Exception.someExceptionContext e
    summary = summaryOf e
    body    = blockLines <> backtraceLines <> causeLines

    blockLines =
      map renderBlockAnnotation (getExceptionAnnotations ctx :: [BlockAnnotation])
    backtraceLines =
      concatMap (filter (not . Text.null) . Text.lines . toS . displayBacktraces)
        (getExceptionAnnotations ctx :: [Backtraces])
    causeLines =
      [ "caused while handling: " <> summaryOf orig
      | Exception.WhileHandling orig <- getExceptionAnnotations ctx :: [Exception.WhileHandling]
      ]

-- | A single line naming an exception: an 'AppError' renders through
-- 'renderAppError'; anything else falls back to the first line of its
-- 'displayException'.
summaryOf :: Exception.SomeException -> Text
summaryOf se = case Exception.fromException peeled of
  Just ae -> renderAppError ae
  Nothing -> firstLine (toS (Exception.displayException peeled))
  where
    peeled = peelLinked se

-- | Unwrap async's 'ExceptionInLinkedThread' layers so the real error —
-- and its context — surfaces instead of the propagation wrapper.
peelLinked :: Exception.SomeException -> Exception.SomeException
peelLinked se = case Exception.fromException se of
  Just (ExceptionInLinkedThread _ inner) -> peelLinked inner
  Nothing                                -> se

firstLine :: Text -> Text
firstLine t = case Text.lines t of
  (l : _) -> l
  []      -> t

-- ---------------------------------------------------------------------------
-- * Thread-exit logging
-- ---------------------------------------------------------------------------

-- | Log a background-thread exit. 'AsyncCancelled' is the normal
-- shutdown signal and logs at 'Info'; any other exception is a real
-- crash, rendered and logged at 'Error'.
logThreadExit :: Text -> Exception.SomeException -> AppTracer -> IO ()
logThreadExit component e tracer = case Exception.fromException e of
  Just AsyncCancelled ->
    traceWith tracer $ LogMsg Info component "stopped (cancelled during shutdown)"
  Nothing ->
    traceWith tracer $ LogMsg Error component ("crashed: " <> renderCrash e)
