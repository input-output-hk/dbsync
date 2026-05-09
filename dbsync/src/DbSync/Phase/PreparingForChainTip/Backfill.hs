{-# LANGUAGE OverloadedStrings #-}

-- | Run the post-load tx-column backfill UPDATEs against an open
-- hasql connection.
--
-- The SQL lives in 'DbSync.Db.Statement.Backfill'; this module is
-- the thin connection-side runner.
module DbSync.Phase.PreparingForChainTip.Backfill
  ( backfillTxColumns
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Statement.Backfill
  ( backfillPhaseTwoDepositStmt
  , backfillPhaseTwoFeeStmt
  , backfillValidContractDepositStmt
  )

-- | Execute the three backfill UPDATEs. Must run after
-- 'DbSync.Phase.PreparingForChainTip.Resolve.resolveForeignKeys' so
-- that @tx_in.tx_out_id@ and @collateral_tx_in.tx_out_id@ are
-- populated. Returns the total rows touched.
backfillTxColumns :: Conn.Connection -> IO Int64
backfillTxColumns conn = do
  n1 <- runStmt conn backfillPhaseTwoFeeStmt
  n2 <- runStmt conn backfillPhaseTwoDepositStmt
  n3 <- runStmt conn backfillValidContractDepositStmt
  pure (n1 + n2 + n3)

runStmt :: Conn.Connection -> Stmt.Statement () Int64 -> IO Int64
runStmt conn stmt = do
  result <- Conn.use conn (Sess.statement () stmt)
  case result of
    Right n -> pure n
    Left  e -> panic $ "PreparingForChainTip.Backfill: " <> show e
