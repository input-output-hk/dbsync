-- | hasql session runners that surface failures as typed
-- 'AppDatabaseError' values via 'throwDb'.
--
-- Every helper takes a short label (component or call-site name)
-- that is prepended to the underlying driver error message, so a
-- caught 'AppDatabaseError' identifies where the failure originated.
-- The captured 'SrcInfo' (via 'HasCallStack' + 'withFrozenCallStack')
-- points at the caller, not at this module.
module DbSync.Db.Run
  ( useConn
  , usePoolSession
  ) where

import Cardano.Prelude

import qualified Data.Text as Text
import qualified Hasql.Connection as Conn
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Sess

import DbSync.Error (throwDb)

-- | Run a 'Sess.Session' on the given connection. Raises
-- 'AppDatabaseError' with @\<label\>: \<driver error\>@ on failure.
useConn
  :: (HasCallStack, MonadIO m)
  => Text
  -> Conn.Connection
  -> Sess.Session a
  -> m a
useConn label conn sess = do
  result <- liftIO (Conn.use conn sess)
  case result of
    Right a  -> pure a
    Left err -> withFrozenCallStack (throwDb (label <> ": " <> Text.pack (show err)))

-- | Run a 'Sess.Session' on a pool. Raises 'AppDatabaseError' with
-- @\<label\>: \<pool usage error\>@ on failure.
usePoolSession
  :: (HasCallStack, MonadIO m)
  => Text
  -> Pool.Pool
  -> Sess.Session a
  -> m a
usePoolSession label pool sess = do
  result <- liftIO (Pool.use pool sess)
  case result of
    Right a  -> pure a
    Left err -> withFrozenCallStack (throwDb (label <> ": " <> Text.pack (show err)))
