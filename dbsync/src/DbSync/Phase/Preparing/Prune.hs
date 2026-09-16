{-# LANGUAGE OverloadedStrings #-}

-- | The @utxo.strategy: "prune"@ pass: delete spent outputs so the
-- database holds the UTxO set instead of every output ever made.
-- SQL lives in 'DbSync.Db.Statement.Worker.Prune'.
module DbSync.Phase.Preparing.Prune
  ( pruneConsumedOutputs
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Run (useConn)
import DbSync.Db.Schema.MultiAsset (maTxOutTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (collateralTxOutTableDef)
import DbSync.Db.Statement.Worker.Prune
  ( deleteConsumedMaTxOutStmt
  , deleteConsumedTxOutStmt
  , queryPruneWatermarkStmt
  , truncateCollateralTxOutStmt
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Phase.Preparing.Step (StepKind (..), step, stepRows, stepSkipped)
import DbSync.Trace (HasTracer (..))

-- | Delete every consumed output. No safety window: the handoff tip is
-- already @k@ blocks back, so no fork can reach these rows.
--
-- Ordering is load-bearing — after the fee and deposit backfills,
-- which read the values this removes; before the flip and index
-- build, so neither touches a doomed row.
pruneConsumedOutputs
  :: (HasTracer env, HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => [TableDef] -> m Int64
pruneConsumedOutputs tables = do
  mWatermark <- runStmt 0 queryPruneWatermarkStmt
  case mWatermark of
    Nothing -> do
      stepSkipped CleanupStep "prune consumed tx_out" "no blocks"
      pure 0
    Just watermark -> do
      when (hasTable (tdName maTxOutTableDef)) $
        void $ stepRows CleanupStep "prune ma_tx_out (consumed parents)" $
          runStmt watermark deleteConsumedMaTxOutStmt
      deleted <- stepRows CleanupStep "prune consumed tx_out" $
        runStmt watermark deleteConsumedTxOutStmt
      when (hasTable (tdName collateralTxOutTableDef)) $
        step CleanupStep "truncate collateral_tx_out" $
          runStmt () truncateCollateralTxOutStmt
      pure deleted
  where
    hasTable name = any ((== name) . tdName) tables

runStmt
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => a -> Stmt.Statement a b -> m b
runStmt param stmt = do
  conn <- asks getHasqlConnection
  useConn "Phase.Preparing.Prune" conn (Sess.statement param stmt)
