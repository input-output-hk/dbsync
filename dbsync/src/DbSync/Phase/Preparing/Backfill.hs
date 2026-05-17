{-# LANGUAGE OverloadedStrings #-}

-- | Post-load tx-column backfill UPDATEs and deposit-pending flush.
-- SQL lives in 'DbSync.Db.Statement.Backfill' /
-- 'DbSync.Db.Statement.EpochParamPending'.
module DbSync.Phase.Preparing.Backfill
  ( backfillTxColumns
  , applyDepositPending
  , truncateDepositPending
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.AppM (LoggingM)
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
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Trace.Timing (timedTrace)

-- | Execute the four backfill UPDATEs. Must run after
-- 'DbSync.Phase.Preparing.Resolve.resolveForeignKeys' so
-- that @tx_in.tx_out_id@ / @collateral_tx_in.tx_out_id@ are
-- populated.
backfillTxColumns
  :: (LoggingM env m, HasHasqlConnection env)
  => m Int64
backfillTxColumns = do
  n1 <- timedTrace "PreparingForVolatileTail" "backfill phase-2 tx.fee" $
          runRowsAffected backfillPhaseTwoFeeStmt
  n2 <- timedTrace "PreparingForVolatileTail" "backfill Byron tx.fee" $
          runRowsAffected backfillByronFeeStmt
  n3 <- timedTrace "PreparingForVolatileTail" "backfill phase-2 tx.deposit" $
          runRowsAffected backfillPhaseTwoDepositStmt
  n4 <- timedTrace "PreparingForVolatileTail" "backfill valid-contract tx.deposit" $
          runRowsAffected backfillValidContractDepositStmt
  pure (n1 + n2 + n3 + n4)

-- | Fill the two ledger-derived deposit columns from
-- @epoch_param_pending@. Both UPDATEs filter on @deposit IS NULL@
-- so they never overwrite an extractor-written value (Conway+
-- inline stake-registration deposits).
applyDepositPending
  :: (LoggingM env m, HasHasqlConnection env)
  => m Int64
applyDepositPending = do
  n1 <- timedTrace "PreparingForVolatileTail" "apply pool_update.deposit" $
          runRowsAffected applyPoolUpdateDepositStmt
  n2 <- timedTrace "PreparingForVolatileTail" "apply stake_registration.deposit" $
          runRowsAffected applyStakeRegistrationDepositStmt
  pure (n1 + n2)

-- | @TRUNCATE epoch_param_pending@ once the two 'applyDepositPending'
-- UPDATEs have run.
truncateDepositPending
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => m ()
truncateDepositPending = do
  conn <- asks getHasqlConnection
  result <- liftIO $ Conn.use conn (Sess.statement () truncateEpochParamPendingStmt)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "Phase.Preparing.Backfill: " <> show e

runRowsAffected
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Stmt.Statement () Int64 -> m Int64
runRowsAffected stmt = do
  conn <- asks getHasqlConnection
  result <- liftIO $ Conn.use conn (Sess.statement () stmt)
  case result of
    Right n -> pure n
    Left  e -> panic $ "Phase.Preparing.Backfill: " <> show e
