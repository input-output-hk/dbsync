-- | Mutable holder for the current 'SyncPhase'.
--
-- One ref lives on 'CoreEnv' and is the source of truth for every
-- observer. Only the orchestrator and the Follow loop write to it.
module DbSync.Phase.Ref
  ( SyncPhaseRef
  , newSyncPhaseRef
  , readSyncPhase
  , readSyncPhaseSTM
  , setSyncPhase

    -- * Env accessor
  , HasSyncPhase (..)
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Tracer (traceWith)

import DbSync.Db.Phase (SyncPhase, renderSyncPhase)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Newtype rather than a raw 'TVar' so callers cannot bypass
-- 'setSyncPhase'\'s logging.
newtype SyncPhaseRef = SyncPhaseRef (TVar SyncPhase)

newSyncPhaseRef :: SyncPhase -> IO SyncPhaseRef
newSyncPhaseRef p = SyncPhaseRef <$> newTVarIO p

readSyncPhase :: SyncPhaseRef -> IO SyncPhase
readSyncPhase (SyncPhaseRef v) = readTVarIO v

readSyncPhaseSTM :: SyncPhaseRef -> STM SyncPhase
readSyncPhaseSTM (SyncPhaseRef v) = readTVar v

-- | Write the new phase and log one line on a real transition. A
-- no-op when the value is already current.
setSyncPhase :: AppTracer -> SyncPhaseRef -> SyncPhase -> IO ()
setSyncPhase tracer (SyncPhaseRef v) next = do
  prev <- atomically $ do
    cur <- readTVar v
    when (cur /= next) (writeTVar v next)
    pure cur
  when (prev /= next) $
    traceWith tracer $ LogMsg Info "Phase"
      ("phase " <> renderSyncPhase prev <> " -> " <> renderSyncPhase next)
      Nothing

-- ---------------------------------------------------------------------------
-- * Env accessor
-- ---------------------------------------------------------------------------

-- | Read the live lifecycle phase from the env. 'IO' because the
-- value lives behind a 'TVar' on 'CoreEnv' and changes across the
-- run.
class HasSyncPhase env where
  getSyncPhase :: env -> IO SyncPhase
