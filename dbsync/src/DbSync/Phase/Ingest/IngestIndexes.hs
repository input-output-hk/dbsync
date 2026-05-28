{-# LANGUAGE OverloadedStrings #-}

-- | Build the per-epoch resolver's working indexes on the
-- still-UNLOGGED Ingest tables.
--
-- @lsm-tree@ and the COPY path want unindexed heaps for throughput,
-- but the per-epoch address-resolver worker
-- ('DbSync.Worker.TxOut') issues bulk @UPDATE tx_out@ /
-- @UPDATE collateral_tx_out@ / @SELECT address@ statements that match
-- by id (PK) or @raw_hash@. Without indexes those degrade to a hash
-- join against the full heap; cost grows linearly with chain history
-- and produces the long CPU-idle stretches an operator sees at epoch
-- boundaries late in 'IngestChainHistory' (the
-- @awaitTxOutDrained (epoch N-1)@ stall).
--
-- Indexes built here:
--
--   * @tx_out_pkey_idx@ — used by the two bulk @UPDATE tx_out@.
--   * @collateral_tx_out_pkey_idx@ — used by the bulk
--     @UPDATE collateral_tx_out@.
--   * @address_unique_1_idx@ — used by the bulk @SELECT address@
--     that resolves existing addresses by their @md5(raw)@ hash.
--
-- All three use @IF NOT EXISTS@ so a resumed boot is a no-op, and
-- their names match what 'Phase.Preparing.Indexes' would emit later
-- — so the schema-driven full pass during
-- 'PreparingForVolatileTail' dedupes against them.
--
-- Tables are UNLOGGED at this point, so the index build is one-pass
-- (no WAL, no second validation scan) and runs to completion before
-- the consumer starts.
module DbSync.Phase.Ingest.IngestIndexes
  ( createIngestResolveIndexes
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Statement.Indexes (ingestResolveIndexStatements)
import DbSync.Trace.Timing (timedTraceIO_)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Component label for trace lines emitted by this module.
component :: Text
component = "IngestIndexes"

-- | Build the per-epoch resolver's working indexes on the open
-- consumer control connection.
--
-- Each statement is logged separately so an operator chasing a slow
-- boot sees which index is building. The build is one-shot on
-- still-UNLOGGED tables; subsequent boots see @IF NOT EXISTS@ skip
-- the work.
createIngestResolveIndexes :: AppTracer -> Conn.Connection -> IO ()
createIngestResolveIndexes tracer conn = do
  traceWith tracer $ LogMsg Info component
    "building per-epoch resolver indexes" Nothing
  for_ (zip [1 :: Int ..] ingestResolveIndexStatements) $ \(i, ddl) ->
    timedTraceIO_ tracer component
      ("ingest-resolve index " <> show i)
      (runDdl ddl)
  where
    runDdl ddl = do
      result <- Conn.use conn (Sess.script ddl)
      case result of
        Right () -> pure ()
        Left  e  -> panic $
          "Phase.Ingest.IngestIndexes: " <> show e <> " for " <> ddl
