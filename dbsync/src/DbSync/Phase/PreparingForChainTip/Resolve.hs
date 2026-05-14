{-# LANGUAGE OverloadedStrings #-}

-- | Run the post-load FK resolution UPDATEs against an open hasql
-- connection. SQL lives in 'DbSync.Db.Statement.Resolve'.
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
import DbSync.Trace.Timing (timedTrace)
import DbSync.Trace.Types (AppTracer)

-- | Execute the four resolution UPDATEs in dependency order. Each is
-- timed and logged separately. Returns the total rows touched.
resolveForeignKeys :: AppTracer -> Conn.Connection -> IO Int64
resolveForeignKeys tracer conn = do
  n1 <- timedTrace tracer "PreparingForChainTip" "resolve tx_in.tx_out_id" $
          runStmt conn resolveTxInStmt
  n2 <- timedTrace tracer "PreparingForChainTip" "resolve collateral_tx_in.tx_out_id" $
          runStmt conn resolveCollateralTxInStmt
  n3 <- timedTrace tracer "PreparingForChainTip" "resolve reference_tx_in.tx_out_id" $
          runStmt conn resolveReferenceTxInStmt
  n4 <- timedTrace tracer "PreparingForChainTip" "resolve tx_out.consumed_by_tx_id" $
          runStmt conn resolveConsumedByTxIdStmt
  pure (n1 + n2 + n3 + n4)

runStmt :: Conn.Connection -> Stmt.Statement () Int64 -> IO Int64
runStmt conn stmt = do
  result <- Conn.use conn (Sess.statement () stmt)
  case result of
    Right n -> pure n
    Left  e -> panic $ "PreparingForChainTip.Resolve: " <> show e
