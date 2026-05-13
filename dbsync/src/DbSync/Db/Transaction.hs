-- | PostgreSQL transaction bracket for hasql connections.
--
-- Wraps an 'IO' action between @BEGIN@ and @COMMIT@; on any
-- exception the transaction is rolled back and the exception
-- re-thrown. Used by the 'FollowingChainTip' loop to make each
-- per-block apply atomic with its 'sync_state' advance.
module DbSync.Db.Transaction
  ( withTransaction
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

-- | Run @action@ inside a PG transaction on @conn@.
--
-- Commits on success; rolls back and re-throws on any exception. If
-- the rollback itself fails (the connection is already in a broken
-- state, for example) that failure is swallowed so the original
-- exception isn't masked.
withTransaction :: Conn.Connection -> IO a -> IO a
withTransaction conn action = do
  runSql "BEGIN"
  result <- action `onException` rollbackQuiet
  runSql "COMMIT"
  pure result
  where
    runSql :: Text -> IO ()
    runSql sql = do
      r <- Conn.use conn (Sess.script sql)
      case r of
        Right () -> pure ()
        Left e   -> panic $ "withTransaction: " <> sql <> ": " <> show e

    rollbackQuiet :: IO ()
    rollbackQuiet =
      void (Conn.use conn (Sess.script "ROLLBACK"))
        `catch` \(_ :: SomeException) -> pure ()
