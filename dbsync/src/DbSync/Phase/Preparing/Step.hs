{-# LANGUAGE OverloadedStrings #-}

-- | Uniform step logging for the post-load pass.
--
-- Every step in 'DbSync.Phase.Preparing.Run' and its sub-modules
-- emits the same line pattern at 'Info':
--
-- > Starting  | <kind>    | <name>
-- > Running   | <kind>    | <name> | <elapsed>
-- > Completed | <kind>    | <name> | <duration> | ✓
-- > Failed    | <kind>    | <name> | <duration> | ✗
-- > Skipped   | <kind>    | <name> | <reason>
--
-- A @Running@ line repeats once a minute so a long step reads as
-- progress rather than a hang. The kind column is padded so the name
-- column aligns across the pass.
module DbSync.Phase.Preparing.Step
  ( StepKind (..)
  , step
  , stepRows
  , stepSkipped
  , prepComponent
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Tracer (traceWith)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Timing (fmtCount, fmtDuration)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Component name used in trace lines emitted by the post-load
-- pass and its sub-modules.
prepComponent :: Text
prepComponent = "PreparingForVolatileTail"

-- | What kind of work a step performs. Rendered as the second
-- column of every step log line.
data StepKind
  = PhaseStep     -- ^ The outer post-load pass itself.
  | TuningStep    -- ^ Session GUC application.
  | IndexStep     -- ^ @CREATE INDEX@ / @DROP INDEX@.
  | ResolveStep   -- ^ FK resolution (table rebuilds + residual UPDATE).
  | AnalyzeStep   -- ^ @ANALYZE@ passes.
  | BackfillStep  -- ^ Backfill UPDATEs / INSERT…SELECT fills.
  | CleanupStep   -- ^ Post-backfill truncates.
  | FlipStep      -- ^ @ALTER TABLE … SET LOGGED@ heap rewrites.
  | SequenceStep  -- ^ Sequence attach / reset.

kindLabel :: StepKind -> Text
kindLabel k = Text.justifyLeft 8 ' ' $ case k of
  PhaseStep    -> "phase"
  TuningStep   -> "tuning"
  IndexStep    -> "index"
  ResolveStep  -> "resolve"
  AnalyzeStep  -> "analyze"
  BackfillStep -> "backfill"
  CleanupStep  -> "cleanup"
  FlipStep     -> "flip"
  SequenceStep -> "sequence"

-- | Run an action bracketed by @Starting@ / @Completed@ lines; a
-- throwing action emits @Failed@ (with the elapsed time and a cross)
-- and rethrows.
step
  :: (HasTracer env, MonadReader env m, MonadUnliftIO m)
  => StepKind -> Text -> m a -> m a
step kind name action =
  stepWith kind name (const "") (void' action)
  where
    void' act = (\a -> (a, ())) <$> act

-- | Like 'step' for actions returning a row count; the @Completed@
-- line carries the count after the checkmark.
stepRows
  :: (HasTracer env, MonadReader env m, MonadUnliftIO m)
  => StepKind -> Text -> m Int64 -> m Int64
stepRows kind name action =
  stepWith kind name (\n -> " | " <> fmtCount n <> " rows") (dup <$> action)
  where
    dup n = (n, n)

-- | Shared bracket: the action returns its result plus a value the
-- completion-suffix renderer consumes.
stepWith
  :: (HasTracer env, MonadReader env m, MonadUnliftIO m)
  => StepKind -> Text -> (b -> Text) -> m (a, b) -> m a
stepWith kind name suffix action = do
  tracer <- asks getTracer
  let prefix = kindLabel kind <> " | " <> name
  liftIO $ emit tracer ("Starting  | " <> prefix)
  start  <- liftIO getCurrentTime
  result <- withRunInIO $ \run ->
    withAsync (heartbeat tracer prefix start) $ \_ ->
      try @SomeException (run action)
  end    <- liftIO getCurrentTime
  let dur = fmtDuration (realToFrac (diffUTCTime end start))
  case result of
    Left err -> do
      liftIO $ emit tracer
        ("Failed    | " <> prefix <> " | " <> dur <> " | \x2717")
      liftIO $ throwIO err
    Right (a, b) -> do
      liftIO $ emit tracer
        ("Completed | " <> prefix <> " | " <> dur <> " | \x2713" <> suffix b)
      pure a

-- | Emit a single @Skipped@ line for work the enabled extractor
-- set makes inapplicable; the reason names the missing table.
stepSkipped
  :: (HasTracer env, MonadReader env m, MonadIO m)
  => StepKind -> Text -> Text -> m ()
stepSkipped kind name reason = do
  tracer <- asks getTracer
  liftIO $ emit tracer
    ("Skipped   | " <> kindLabel kind <> " | " <> name <> " | " <> reason)

-- | One @Running@ line per minute until the step's action returns
-- and 'withAsync' cancels the loop.
heartbeat :: AppTracer -> Text -> UTCTime -> IO ()
heartbeat tracer prefix start = forever $ do
  threadDelay 60000000
  now <- getCurrentTime
  let dur = fmtDuration (realToFrac (diffUTCTime now start))
  emit tracer ("Running   | " <> prefix <> " | " <> dur)

emit :: AppTracer -> Text -> IO ()
emit tracer msg = traceWith tracer $ LogMsg Info prepComponent msg
