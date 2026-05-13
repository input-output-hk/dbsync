{-# LANGUAGE OverloadedStrings #-}

-- | Run the post-load tx-column backfill UPDATEs against an open
-- hasql connection.
--
-- The SQL lives in 'DbSync.Db.Statement.Backfill' and
-- 'DbSync.Db.Statement.EpochParamPending'; this module is the thin
-- connection-side runner.
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

-- | Execute the four backfill UPDATEs. Must run after
-- 'DbSync.Phase.PreparingForChainTip.Resolve.resolveForeignKeys' so
-- that @tx_in.tx_out_id@ and @collateral_tx_in.tx_out_id@ are
-- populated. Returns the total rows touched.
backfillTxColumns :: Conn.Connection -> IO Int64
backfillTxColumns conn = do
  n1 <- runRowsAffected conn backfillPhaseTwoFeeStmt
  n2 <- runRowsAffected conn backfillByronFeeStmt
  n3 <- runRowsAffected conn backfillPhaseTwoDepositStmt
  n4 <- runRowsAffected conn backfillValidContractDepositStmt
  pure (n1 + n2 + n3 + n4)

-- | Fill the two ledger-derived deposit columns from
-- @epoch_param_pending@. The table is populated by the ledger
-- worker through 'DbSync.Ingest.Consumer.flushPendingDeposits' at
-- each epoch boundary during Ingest.
--
-- Both UPDATEs filter on @deposit IS NULL@ so they are idempotent
-- and never overwrite a value the extractor already wrote (Conway+
-- inline stake-registration deposits, which are not in the
-- pending table at all).
applyDepositPending :: Conn.Connection -> IO Int64
applyDepositPending conn = do
  n1 <- runRowsAffected conn applyPoolUpdateDepositStmt
  n2 <- runRowsAffected conn applyStakeRegistrationDepositStmt
  pure (n1 + n2)

-- | @TRUNCATE epoch_param_pending@. Called once the two
-- 'applyDepositPending' UPDATEs have run. The table stays in the
-- schema for a future Follow → Ingest re-entry.
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
