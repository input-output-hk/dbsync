{-# LANGUAGE OverloadedStrings #-}

-- | Post-load column fills: the tx-column UPDATEs, the @redeemer@
-- rebuild, and the deposit-pending flush. SQL lives in
-- 'DbSync.Db.Statement.Worker.Backfill' /
-- 'DbSync.Db.Statement.Worker.RedeemerScriptHash' /
-- 'DbSync.Db.Statement.Worker.EpochParamPending'.
module DbSync.Phase.Preparing.Backfill
  ( backfillTxColumns
  , rebuildSpendScriptHash
  , applyDepositPending
  , truncateDepositPending
  , backfillEpochFinalized
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.Worker.Backfill
  ( backfillByronFeeStmt
  , backfillPhaseTwoDepositStmt
  , backfillPhaseTwoFeeStmt
  , backfillValidContractDepositStmt
  )
import DbSync.Db.Statement.EpochView (backfillEpochFinalizedStmt)
import DbSync.Db.Statement.Worker.RedeemerScriptHash (rebuildSpendScriptHashScript)
import DbSync.Db.Statement.Worker.EpochParamPending
  ( applyPoolUpdateDepositStmt
  , applyStakeRegistrationDepositStmt
  , truncateEpochParamPendingStmt
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Phase.Preparing.Step (StepKind (..), step, stepRows)
import DbSync.Trace (HasTracer (..))

-- | Execute the four backfill UPDATEs. Must run after
-- 'DbSync.Phase.Preparing.Resolve.resolveInputTxOutIds' so
-- that @tx_in.tx_out_id@ / @collateral_tx_in.tx_out_id@ are
-- populated.
backfillTxColumns
  :: (HasTracer env, HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => m Int64
backfillTxColumns = do
  n1 <- stepRows BackfillStep "tx.fee (phase-2 failed txs)" $
          runRowsAffected backfillPhaseTwoFeeStmt
  n2 <- stepRows BackfillStep "tx.fee (Byron txs)" $
          runRowsAffected backfillByronFeeStmt
  n3 <- stepRows BackfillStep "tx.deposit (phase-2 failed txs)" $
          runRowsAffected backfillPhaseTwoDepositStmt
  n4 <- stepRows BackfillStep "tx.deposit (valid-contract txs)" $
          runRowsAffected backfillValidContractDepositStmt
  pure (n1 + n2 + n3 + n4)

-- | Rebuild @redeemer@ with @script_hash@ filled for spend redeemers
-- from the payment credential of the output each one unlocks. Must run
-- after 'DbSync.Phase.Preparing.Resolve.resolveInputTxOutIds' so
-- @tx_in.tx_out_id@ identifies the spent output, and before the flip,
-- which attaches the id sequence the rebuild drops.
rebuildSpendScriptHash
  :: (HasTracer env, HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => m ()
rebuildSpendScriptHash =
  step ResolveStep "redeemer.script_hash (table rebuild)" $ do
    conn <- asks getHasqlConnection
    useConn "Phase.Preparing.Backfill.rebuildSpendScriptHash" conn
      (Sess.script rebuildSpendScriptHashScript)

-- | Fill the two ledger-derived deposit columns from
-- @epoch_param_pending@. Both UPDATEs filter on @deposit IS NULL@
-- so they never overwrite an extractor-written value (Conway+
-- inline stake-registration deposits).
applyDepositPending
  :: (HasTracer env, HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => m Int64
applyDepositPending = do
  n1 <- stepRows BackfillStep "pool_update.deposit (from epoch_param_pending)" $
          runRowsAffected applyPoolUpdateDepositStmt
  n2 <- stepRows BackfillStep "stake_registration.deposit (from epoch_param_pending)" $
          runRowsAffected applyStakeRegistrationDepositStmt
  pure (n1 + n2)

-- | @TRUNCATE epoch_param_pending@ once the two 'applyDepositPending'
-- UPDATEs have run.
truncateDepositPending
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => m ()
truncateDepositPending = do
  conn <- asks getHasqlConnection
  useConn "Phase.Preparing.Backfill.truncateDepositPending" conn
    (Sess.statement () truncateEpochParamPendingStmt)

-- | One-shot @INSERT … SELECT@ that fills @epoch_finalized@ from
-- every closed epoch in @block@. Run by 'Phase.Preparing.Run' when
-- the @epoch@ extractor is enabled. The current (open) epoch stays
-- in @epoch_current@'s domain.
--
-- Must run after the production index build: the statement upserts
-- via @ON CONFLICT ("no")@, which requires the unique index on
-- @epoch_finalized.no@.
backfillEpochFinalized
  :: (HasTracer env, HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => m ()
backfillEpochFinalized =
  step BackfillStep "epoch_finalized (aggregate closed epochs)" $ do
    conn <- asks getHasqlConnection
    useConn "Phase.Preparing.Backfill.backfillEpochFinalized" conn
      (Sess.statement () backfillEpochFinalizedStmt)

runRowsAffected
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Stmt.Statement () Int64 -> m Int64
runRowsAffected stmt = do
  conn <- asks getHasqlConnection
  useConn "Phase.Preparing.Backfill.runRowsAffected" conn (Sess.statement () stmt)
