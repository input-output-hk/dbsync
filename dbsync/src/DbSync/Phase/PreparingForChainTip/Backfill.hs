{-# LANGUAGE OverloadedStrings #-}

-- | Post-load tx-column backfill UPDATEs and deposit-pending flush.
-- SQL lives in 'DbSync.Db.Statement.Backfill' /
-- 'DbSync.Db.Statement.EpochParamPending'.
module DbSync.Phase.PreparingForChainTip.Backfill
  ( backfillTxColumns
  , applyDepositPending
  , truncateDepositPending
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Statement.Backfill
  ( backfillByronFeeStmt
  , backfillPhaseTwoDepositStmt
  , backfillPhaseTwoFeeStmt
  , backfillValidContractDepositStmt
  )
import DbSync.Db.Statement.EpochParamPending
  ( applyPoolUpdateDepositStmt
  , applyStakeRegistrationDepositStmt
  , truncateEpochParamPendingStmt
  )
import DbSync.Trace.Timing (timedTrace)
import DbSync.Trace.Types (AppTracer)

-- | Execute the four backfill UPDATEs. Must run after
-- 'DbSync.Phase.PreparingForChainTip.Resolve.resolveForeignKeys' so
-- that @tx_in.tx_out_id@ / @collateral_tx_in.tx_out_id@ are
-- populated.
backfillTxColumns :: AppTracer -> Conn.Connection -> IO Int64
backfillTxColumns tracer conn = do
  n1 <- timedTrace tracer "PreparingForChainTip" "backfill phase-2 tx.fee" $
          runRowsAffected conn backfillPhaseTwoFeeStmt
  n2 <- timedTrace tracer "PreparingForChainTip" "backfill Byron tx.fee" $
          runRowsAffected conn backfillByronFeeStmt
  n3 <- timedTrace tracer "PreparingForChainTip" "backfill phase-2 tx.deposit" $
          runRowsAffected conn backfillPhaseTwoDepositStmt
  n4 <- timedTrace tracer "PreparingForChainTip" "backfill valid-contract tx.deposit" $
          runRowsAffected conn backfillValidContractDepositStmt
  pure (n1 + n2 + n3 + n4)

-- | Fill the two ledger-derived deposit columns from
-- @epoch_param_pending@. Both UPDATEs filter on @deposit IS NULL@
-- so they never overwrite an extractor-written value (Conway+
-- inline stake-registration deposits).
applyDepositPending :: AppTracer -> Conn.Connection -> IO Int64
applyDepositPending tracer conn = do
  n1 <- timedTrace tracer "PreparingForChainTip" "apply pool_update.deposit" $
          runRowsAffected conn applyPoolUpdateDepositStmt
  n2 <- timedTrace tracer "PreparingForChainTip" "apply stake_registration.deposit" $
          runRowsAffected conn applyStakeRegistrationDepositStmt
  pure (n1 + n2)

-- | @TRUNCATE epoch_param_pending@ once the two 'applyDepositPending'
-- UPDATEs have run.
truncateDepositPending :: Conn.Connection -> IO ()
truncateDepositPending conn = do
  result <- Conn.use conn (Sess.statement () truncateEpochParamPendingStmt)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "PreparingForChainTip.Backfill: " <> show e

runRowsAffected :: Conn.Connection -> Stmt.Statement () Int64 -> IO Int64
runRowsAffected conn stmt = do
  result <- Conn.use conn (Sess.statement () stmt)
  case result of
    Right n -> pure n
    Left  e -> panic $ "PreparingForChainTip.Backfill: " <> show e
