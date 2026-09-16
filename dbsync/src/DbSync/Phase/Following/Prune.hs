{-# LANGUAGE OverloadedStrings #-}

-- | Epoch-boundary prune under @utxo.strategy: "prune"@. Keeps the
-- Follow-phase database from re-accumulating the spent outputs that
-- 'DbSync.Phase.Preparing.Prune' removed at the handoff.
module DbSync.Phase.Following.Prune
  ( pruneConsumedAtBoundary
  , pruneSafeBlockDepth
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.Worker.Prune
  ( deleteConsumedMaTxOutStmt
  , deleteConsumedTxOutStmt
  , queryPruneWatermarkStmt
  )
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | How far behind the tip a consumed output must be before it can be
-- deleted, as a multiple of the security parameter. A rollback reaches
-- at most @k@ blocks, so @2k@ leaves a full @k@ of margin.
pruneSafeBlockDepth :: Word64 -> Word64
pruneSafeBlockDepth k = 2 * k

-- | Delete outputs consumed outside the rollback window.
--
-- Runs after the crossing block's transaction commits, not inside it:
-- the delete is unbounded in size, and a failure here must not undo a
-- committed block. It is idempotent, so a skipped boundary is picked
-- up by the next one.
pruneConsumedAtBoundary :: AppTracer -> Conn.Connection -> Word64 -> IO ()
pruneConsumedAtBoundary tracer conn k = do
  mWatermark <- runStmt (pruneSafeBlockDepth k) queryPruneWatermarkStmt
  for_ mWatermark $ \watermark -> do
    assets  <- runStmt watermark deleteConsumedMaTxOutStmt
    outputs <- runStmt watermark deleteConsumedTxOutStmt
    when (outputs > 0 || assets > 0) $
      traceWith tracer $ LogMsg Info "Prune" $
        "pruned " <> show outputs <> " consumed tx_out, "
          <> show assets <> " ma_tx_out"
  where
    runStmt :: a -> Stmt.Statement a b -> IO b
    runStmt param stmt =
      useConn "Phase.Following.Prune" conn (Sess.statement param stmt)
