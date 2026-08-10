{-# LANGUAGE OverloadedStrings #-}

-- | Build the per-epoch resolver's working indexes on the
-- still-UNLOGGED Ingest tables. The COPY path wants unindexed heaps
-- for throughput, but 'DbSync.Worker.TxOut.Worker' matches by PK or
-- @raw_hash@ and hash-joins the full heap without them.
--
-- The index names match what 'Phase.Preparing.Indexes' emits, so the
-- later schema-driven pass dedupes against them.
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

component :: Text
component = "IngestIndexes"

-- | Each statement logs separately, so an operator chasing a slow
-- boot sees which index builds. Every statement uses
-- @IF NOT EXISTS@, so a resumed boot skips the work.
createIngestResolveIndexes :: AppTracer -> Conn.Connection -> IO ()
createIngestResolveIndexes tracer conn = do
  traceWith tracer $ LogMsg Info component
    "building per-epoch resolver indexes"
  for_ ingestResolveIndexStatements $ \ix ->
    timedTraceIO_ tracer component
      ("ingest-resolve index " <> isName ix)
      (useConn "Phase.Ingest.IngestIndexes" conn (Sess.script (isSql ix)))
