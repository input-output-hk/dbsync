{-# LANGUAGE OverloadedStrings #-}

-- | Run the post-load FK resolution UPDATEs against an open hasql
-- connection.
--
-- The SQL lives in 'DbSync.Db.Statement.Resolve'; this module is the
-- thin connection-side runner.
module DbSync.Phase.PreparingForChainTip.Resolve
  ( resolveForeignKeys
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Statement.Resolve
  ( resolveCollateralTxInStmt
  , resolveConsumedByTxIdStmt
  , resolveReferenceTxInStmt
  , resolveTxInStmt
  )

-- | Execute the four resolution UPDATEs in dependency order.
-- Returns the total rows touched. Panics on session error so the
-- top-level orchestrator can convert to the project's 'AppError'.
resolveForeignKeys :: Conn.Connection -> IO Int64
resolveForeignKeys conn = do
  n1 <- runStmt conn resolveTxInStmt
  n2 <- runStmt conn resolveCollateralTxInStmt
  n3 <- runStmt conn resolveReferenceTxInStmt
  n4 <- runStmt conn resolveConsumedByTxIdStmt
  pure (n1 + n2 + n3 + n4)

runStmt :: Conn.Connection -> Stmt.Statement () Int64 -> IO Int64
runStmt conn stmt = do
  result <- Conn.use conn (Sess.statement () stmt)
  case result of
    Right n -> pure n
    Left  e -> panic $ "PreparingForChainTip.Resolve: " <> show e
