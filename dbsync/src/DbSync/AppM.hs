-- | Application monad stack.
--
-- 'AppM' is a thin @ReaderT env IO@ newtype used throughout db-sync.
-- Phase-specific type aliases ('IngestM', 'FollowM') are defined at
-- use sites where the concrete environment types are in scope.
module DbSync.AppM
  ( AppM (..)
    -- TODO: type IngestM = AppM IngestEnv  (defined at use sites)
    -- TODO: type FollowM = AppM FollowEnv  (defined at use sites)
  , runAppM
  ) where

import Cardano.Prelude

import Control.Monad.Reader (MonadReader, ReaderT (..), runReaderT)

-- | The core application monad: @ReaderT env IO@.
newtype AppM env a = AppM {unAppM :: ReaderT env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env)

-- Note: Phase-specific aliases live at use sites to avoid circular imports:
--   type IngestM = AppM IngestEnv   (see DbSync.Env for IngestEnv)
--   type FollowM = AppM FollowEnv  (see DbSync.Env for FollowEnv)

-- | Run an 'AppM' action with the given environment.
runAppM :: env -> AppM env a -> IO a
runAppM env (AppM m) = runReaderT m env
