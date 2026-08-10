-- | Application monad stack.
--
-- 'AppM' is a thin @ReaderT env IO@ newtype. The phase-specific
-- aliases ('CoreM', 'IngestM', 'FollowM', 'LedgerM') name the env
-- each phase uses; everything else carries 'HasXxx' constraints
-- and works in any matching env.
module DbSync.AppM
  ( AppM (..)
  , runAppM
  , CoreM
  , IngestM
  , FollowM
  , LedgerM

    -- * Constraint synonyms
  , LoggingM
  , SyncStateM
  , DbConnM
  , ExtractorC
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)

import DbSync.App.Env (CoreEnv, FollowEnv, HasNetwork, IngestEnv)
import DbSync.Db.Transaction (HasHasqlConnection)
import DbSync.Extractor (HasExtractors)
import DbSync.Resolver (HasResolver)
import DbSync.SyncState.Row (HasControlConnection)
import DbSync.Trace (HasTracer)
import DbSync.Worker.Ledger.Types (LedgerEnv)
import DbSync.Writer (HasWriter)

newtype AppM env a = AppM {unAppM :: ReaderT env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env, MonadUnliftIO)

runAppM :: env -> AppM env a -> IO a
runAppM env (AppM m) = runReaderT m env

-- | Core phase: shared configuration + tracer + metrics.
type CoreM = AppM CoreEnv

-- | IngestChainHistory phase: bulk-load env (COPY writer, dedup maps, ledger subsystem handle).
type IngestM = AppM IngestEnv

-- | FollowingChainTip phase: lighter env for steady-state INSERTs.
type FollowM = AppM FollowEnv

-- | LedgerWorker / snapshot subsystem. Only valid when the ledger
-- feature is enabled.
type LedgerM = AppM LedgerEnv

-- ---------------------------------------------------------------------------
-- * Constraint synonyms
-- ---------------------------------------------------------------------------

-- | An env-bound tracer plus 'MonadIO'.
type LoggingM env m =
  (HasTracer env, MonadReader env m, MonadIO m)

-- | Writes against the @sync_state@ control connection, logged.
type SyncStateM env m =
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
