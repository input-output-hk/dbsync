-- | Application monad stack.
--
-- 'AppM' is a thin @ReaderT env IO@ newtype used throughout db-sync.
-- The phase-specific aliases ('CoreM', 'IngestM', 'FollowM',
-- 'LedgerM') name the env each phase uses; everything else carries
-- 'HasXxx' constraints and works in any matching env.
--
-- 'AppM' derives 'MonadUnliftIO' so 'bracket', 'catch',
-- 'withAsync', and friends work directly without manual @runAppM@
-- ceremony.
module DbSync.AppM
  ( AppM (..)
  , runAppM
  , CoreM
  , IngestM
  , FollowM
  , LedgerM

    -- * Constraint synonyms
  , LoggingM
  , CheckpointM
  , DbConnM
  , ExtractorC
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)

import DbSync.Checkpoint.SyncState (HasControlConnection)
import DbSync.Db.Transaction (HasHasqlConnection)
import DbSync.Env (CoreEnv, FollowEnv, IngestEnv)
import DbSync.Extractor (HasExtractors)
import DbSync.Ledger.Types (LedgerEnv)
import DbSync.Resolver (HasResolver)
import DbSync.Trace (HasTracer)
import DbSync.Env (HasNetwork)
import DbSync.Writer (HasWriter)

-- | The core application monad: @ReaderT env IO@.
newtype AppM env a = AppM {unAppM :: ReaderT env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env, MonadUnliftIO)

-- | Run an 'AppM' action with the given environment.
runAppM :: env -> AppM env a -> IO a
runAppM env (AppM m) = runReaderT m env

-- | Core phase: shared configuration + tracer + metrics.
type CoreM = AppM CoreEnv

-- | IngestChainHistory phase: bulk-load env with COPY writer, dedup
-- maps, ledger subsystem handle, etc.
type IngestM = AppM IngestEnv

-- | FollowingChainTip phase: lighter env for steady-state INSERTs.
type FollowM = AppM FollowEnv

-- | LedgerWorker / snapshot subsystem: only valid when the ledger
-- feature is enabled (callers pattern-match on 'HasLedgerEnv' at the
-- boundary, then run the action via @runAppM lenv@).
type LedgerM = AppM LedgerEnv

-- ---------------------------------------------------------------------------
-- Constraint synonyms
--
-- Bundles of constraints used together repeatedly. Keep the set
-- small — only collapse when the same combination shows up more
-- than a handful of times.
-- ---------------------------------------------------------------------------

-- | Anything that needs an env-bound tracer plus 'MonadIO'.
type LoggingM env m =
  (HasTracer env, MonadReader env m, MonadIO m)

-- | Writes against the @sync_state@ control connection, logged.
type CheckpointM env m =
  ( HasTracer env
  , HasControlConnection env
  , MonadReader env m
  , MonadIO m
  )

-- | DB-write operations against the per-phase hasql connection.
type DbConnM env m =
  ( HasHasqlConnection env
  , MonadReader env m
  , MonadIO m
  )

-- | Standard extractor surface: resolver + writer + chain network.
type ExtractorC env =
  ( HasResolver env
  , HasWriter env
  , HasNetwork env
  , HasExtractors env
  )
