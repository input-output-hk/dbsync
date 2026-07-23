{-# LANGUAGE OverloadedStrings #-}

-- | Build the per-epoch resolver's working indexes on the
-- still-UNLOGGED Ingest tables.
--
-- @lsm-tree@ and the COPY path want unindexed heaps for throughput,
-- but the per-epoch address-resolver worker
-- ('DbSync.Worker.TxOut.Worker') issues bulk @UPDATE tx_out@ /
-- @UPDATE collateral_tx_out@ / @SELECT address@ statements matching
-- by id (PK) or @raw_hash@. Without indexes those degrade to a hash
-- join against the full heap whose cost grows with chain history,
-- producing the CPU-idle stretches seen at epoch boundaries late in
-- 'IngestChainHistory'.
--
-- Builds @tx_out_pkey_idx@, @collateral_tx_out_pkey_idx@ and
-- @address_unique_1_idx@, all with @IF NOT EXISTS@ (a resumed boot is
-- a no-op). Their names match what 'Phase.Preparing.Indexes' emits,
-- so the schema-driven pass during 'PreparingForVolatileTail'
-- dedupes against them. UNLOGGED tables make the build one-pass (no
-- WAL, no second validation scan).
module DbSync.Phase.Ingest.Indexes
  ( createIngestResolveIndexes
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.Indexes (IndexStatement (..), ingestResolveIndexStatements)
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
    "building per-epoch resolver indexes"
  for_ ingestResolveIndexStatements $ \ix ->
    timedTraceIO_ tracer component
      ("ingest-resolve index " <> isName ix)
      (useConn "Phase.Ingest.IngestIndexes" conn (Sess.script (isSql ix)))
