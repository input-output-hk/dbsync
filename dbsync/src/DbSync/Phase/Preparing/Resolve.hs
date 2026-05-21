{-# LANGUAGE OverloadedStrings #-}

-- | Run the post-load FK resolution against an open hasql connection.
-- SQL lives in 'DbSync.Db.Statement.Resolve'.
module DbSync.Phase.Preparing.Resolve
  ( resolveForeignKeys
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.AppM (LoggingM)
import DbSync.Config.Types (SyncConfig (..), SyncOptions (..), UtxoOption (..))
import DbSync.Db.Statement.Resolve
  ( resolveCollateralTxInScript
  , resolveConsumedByTxIdStmt
  , resolveReferenceTxInScript
  , resolveTxInScript
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Env (HasConfig (..))
import DbSync.Trace.Timing (timedTrace_, timedTrace)

-- | CTAS the three input tables, then fill the consumed-by
-- residual when 'uoConsumedByTxId' is on (the per-epoch worker
-- handles the bulk during Ingest; this catches cache-misses).
resolveForeignKeys
  :: (LoggingM env m, HasHasqlConnection env, HasConfig env)
  => m ()
resolveForeignKeys = do
  utxoOpts <- asks (pcUtxo . scOptions . getConfig)
  timedTrace_ "PreparingForVolatileTail" "resolve tx_in.tx_out_id (CTAS)" $
    runScript resolveTxInScript
  timedTrace_ "PreparingForVolatileTail" "resolve collateral_tx_in.tx_out_id (CTAS)" $
    runScript resolveCollateralTxInScript
  timedTrace_ "PreparingForVolatileTail" "resolve reference_tx_in.tx_out_id (CTAS)" $
    runScript resolveReferenceTxInScript
  when (uoConsumedByTxId utxoOpts) $ do
    _ <- timedTrace "PreparingForVolatileTail" "resolve tx_out.consumed_by_tx_id" $ do
      conn <- asks getHasqlConnection
      result <- liftIO $ Conn.use conn (Sess.statement () resolveConsumedByTxIdStmt)
      case result of
        Right n -> pure n
        Left  e -> panic $ "Phase.Preparing.Resolve: " <> show e
    pure ()

runScript
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Text -> m ()
runScript sql = do
  conn <- asks getHasqlConnection
  result <- liftIO $ Conn.use conn (Sess.script sql)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "Phase.Preparing.Resolve: " <> show e
