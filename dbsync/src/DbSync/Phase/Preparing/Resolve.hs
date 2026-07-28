{-# LANGUAGE OverloadedStrings #-}

-- | Run the post-load FK resolution against an open hasql connection.
-- SQL lives in 'DbSync.Db.Statement.Worker.Resolve'.
module DbSync.Phase.Preparing.Resolve
  ( resolveForeignKeys
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Hasql.Session as Sess

import DbSync.App.Config.Types (SyncConfig (..), DbProfile (..), UtxoOption (..))
import DbSync.App.Env (HasConfig (..))
import DbSync.Db.Run (useConn)
import DbSync.Db.Schema.Init (analyzeSql)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , referenceTxInTableDef
  , txInTableDef
  )
import DbSync.Db.Statement.Worker.Resolve
  ( resolveCollateralTxInScript
  , resolveConsumedByTxIdStmt
  , resolveReferenceTxInScript
  , resolveTxInScript
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Phase.Preparing.Step (StepKind (..), step, stepRows)
import DbSync.Trace (HasTracer (..))

-- | Rebuild the three input tables with resolved @tx_out_id@, then
-- fill the consumed-by residual when 'uoConsumedByTxId' is on (the
-- per-epoch worker handles the bulk during Ingest; this catches
-- cache-misses).
--
-- The rebuilt tables are ANALYZEd before the residual UPDATE plans
-- against them: a freshly created table has no statistics at all,
-- and the UPDATE's join order degrades badly on default estimates.
resolveForeignKeys
  :: (HasTracer env, HasHasqlConnection env, HasConfig env, MonadReader env m, MonadUnliftIO m)
  => m ()
resolveForeignKeys = do
  utxoOpts <- asks (pcUtxo . scDbProfile . getConfig)
  step ResolveStep "tx_in.tx_out_id (table rebuild)" $
    runScript resolveTxInScript
  step ResolveStep "collateral_tx_in.tx_out_id (table rebuild)" $
    runScript resolveCollateralTxInScript
  step ResolveStep "reference_tx_in.tx_out_id (table rebuild)" $
    runScript resolveReferenceTxInScript
  step AnalyzeStep "rebuilt input tables" $
    for_ rebuiltTables (runScript . analyzeSql . tdName)
  when (uoConsumedByTxId utxoOpts) $
    void $ stepRows ResolveStep "tx_out.consumed_by_tx_id (residual)" $ do
      conn <- asks getHasqlConnection
      useConn "Phase.Preparing.Resolve" conn
        (Sess.statement () resolveConsumedByTxIdStmt)

-- | The tables the CTAS resolves replace; fresh heaps with no
-- planner statistics until the ANALYZE step above runs.
rebuiltTables :: [TableDef]
rebuiltTables = [txInTableDef, collateralTxInTableDef, referenceTxInTableDef]

runScript
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Text -> m ()
runScript sql = do
  conn <- asks getHasqlConnection
  useConn "Phase.Preparing.Resolve" conn (Sess.script sql)
