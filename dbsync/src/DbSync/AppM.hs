-- | Application monad stack.
--
-- 'AppM' is a thin @ReaderT env IO@ newtype used throughout db-sync.
-- The phase-specific aliases ('CoreM', 'IngestM', 'FollowM', 'LedgerM')
-- are also defined here so there is a single place to look for the
-- monad shape. They all share the underlying 'ReaderT env IO' machinery
-- via the @newtype@-derived instances.
--
-- 'AppM' derives 'MonadUnliftIO' so that exception-aware operations
-- like 'bracket', 'catch', 'withAsync', and 'try' work directly inside
-- AppM without manual @runAppM@ ceremony.
module DbSync.AppM
  ( AppM (..)
  , runAppM
  , CoreM
  , IngestM
  , FollowM
  , LedgerM
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)

import DbSync.Env (CoreEnv, FollowEnv, IngestEnv)
import DbSync.Ledger.Types (LedgerEnv)

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
