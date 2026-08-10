-- | PostgreSQL transaction bracket for hasql connections.
module DbSync.Db.Transaction
  ( HasHasqlConnection (..)
  , withTransaction
  , withTransactionOn
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.Transaction (beginSql, commitSql, rollbackSql)

-- | Per-phase hasql connection used for INSERTs and the rollback
-- cascade. Implemented by 'FollowEnv'.
class HasHasqlConnection env where
  getHasqlConnection :: env -> Conn.Connection

-- | Self-instance so boot and test code can run
-- 'HasHasqlConnection'-polymorphic helpers via @runAppM conn ...@
-- without a phase env.
instance HasHasqlConnection Conn.Connection where
  getHasqlConnection = identity

-- | Run @action@ between @BEGIN@ and @COMMIT@ on the env's
-- connection. Rolls back on exception; swallows a failed rollback
-- so the original exception isn't masked.
withTransaction
  :: (HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => m a
  -> m a
withTransaction action = do
  conn <- asks getHasqlConnection
  withTransactionOn conn action

-- | As 'withTransaction', but for call sites with no
-- 'HasHasqlConnection' env in scope.
withTransactionOn
  :: MonadUnliftIO m
  => Conn.Connection
  -> m a
  -> m a
withTransactionOn conn action =
  withRunInIO $ \run -> do
    runSql conn beginSql
    result <- run action `onException` rollbackQuiet conn
    runSql conn commitSql
    pure result

runSql :: Conn.Connection -> Text -> IO ()
runSql conn sql = useConn ("withTransaction: " <> sql) conn (Sess.script sql)

rollbackQuiet :: Conn.Connection -> IO ()
rollbackQuiet conn =
  void (Conn.use conn (Sess.script rollbackSql))
    `catch` \(_ :: SomeException) -> pure ()
