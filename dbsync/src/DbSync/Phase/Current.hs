-- | Mutable holder for the running 'SyncPhase'.
--
-- One ref lives on 'CoreEnv' and is the source of truth for every
-- observer. Only the orchestrator and the Follow loop write to it.
module DbSync.Phase.Current
  ( CurrentPhase
  , newCurrentPhase
  , readCurrentPhase
  , readCurrentPhaseSTM
  , setCurrentPhase

    -- * Env accessor
  , HasCurrentPhase (..)
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Tracer (traceWith)

import DbSync.Phase.Type (SyncPhase, renderPhase)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (LogMsg (..), Severity (..))

-- | Newtype rather than a raw 'TVar' so callers cannot bypass
-- 'setCurrentPhase'\'s logging.
newtype CurrentPhase = CurrentPhase (TVar SyncPhase)

newCurrentPhase :: SyncPhase -> IO CurrentPhase
newCurrentPhase p = CurrentPhase <$> newTVarIO p

readCurrentPhase :: CurrentPhase -> IO SyncPhase
readCurrentPhase (CurrentPhase v) = readTVarIO v

readCurrentPhaseSTM :: CurrentPhase -> STM SyncPhase
readCurrentPhaseSTM (CurrentPhase v) = readTVar v

-- | Write the new phase and log one line on a real transition. A
-- no-op when the value is already current.
setCurrentPhase
  :: (HasTracer env, MonadReader env m, MonadIO m)
  => CurrentPhase -> SyncPhase -> m ()
setCurrentPhase (CurrentPhase v) next = do
  tracer <- asks getTracer
  prev <- liftIO $ atomically $ do
    cur <- readTVar v
    when (cur /= next) (writeTVar v next)
    pure cur
  when (prev /= next) $
    liftIO $ traceWith tracer $ LogMsg Info "Phase"
      ("phase " <> renderPhase prev <> " -> " <> renderPhase next)
      Nothing

-- ---------------------------------------------------------------------------
-- * Env accessor
-- ---------------------------------------------------------------------------

-- | Read the running phase from the env. 'IO' because the value
-- lives behind a 'TVar' on 'CoreEnv' and changes across the run.
class HasCurrentPhase env where
  getCurrentPhase :: env -> IO SyncPhase
