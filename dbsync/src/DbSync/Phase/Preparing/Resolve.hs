{-# LANGUAGE OverloadedStrings #-}

-- | Run the post-load FK resolution UPDATEs against an open hasql
-- connection. SQL lives in 'DbSync.Db.Statement.Resolve'.
module DbSync.Phase.Preparing.Resolve
  ( resolveForeignKeys
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.AppM (LoggingM)
import DbSync.Db.Statement.Resolve
  ( resolveCollateralTxInStmt
  , resolveConsumedByTxIdStmt
  , resolveReferenceTxInStmt
  , resolveTxInStmt
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Trace.Timing (timedTrace)

-- | Execute the four resolution UPDATEs in dependency order. Each is
-- timed and logged separately. Returns the total rows touched.
resolveForeignKeys
  :: (LoggingM env m, HasHasqlConnection env)
  => m Int64
resolveForeignKeys = do
  n1 <- timedTrace "PreparingForVolatileTail" "resolve tx_in.tx_out_id" $
          runStmt resolveTxInStmt
  n2 <- timedTrace "PreparingForVolatileTail" "resolve collateral_tx_in.tx_out_id" $
          runStmt resolveCollateralTxInStmt
  n3 <- timedTrace "PreparingForVolatileTail" "resolve reference_tx_in.tx_out_id" $
          runStmt resolveReferenceTxInStmt
  n4 <- timedTrace "PreparingForVolatileTail" "resolve tx_out.consumed_by_tx_id" $
          runStmt resolveConsumedByTxIdStmt
  pure (n1 + n2 + n3 + n4)

runStmt
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Stmt.Statement () Int64 -> m Int64
runStmt stmt = do
  conn <- asks getHasqlConnection
  result <- liftIO $ Conn.use conn (Sess.statement () stmt)
  case result of
    Right n -> pure n
    Left  e -> panic $ "Phase.Preparing.Resolve: " <> show e
