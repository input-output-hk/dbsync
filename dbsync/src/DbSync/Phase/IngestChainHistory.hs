-- | Ingest Chain History phase.
--
-- Bulk-loads historical blocks using PostgreSQL COPY streams and UNLOGGED
-- tables. Runs from genesis (or last checkpoint) up to the configured
-- catch-up threshold before the chain tip.
module DbSync.Phase.IngestChainHistory
  ( run
  ) where

import Cardano.Prelude

import DbSync.AppM (IngestM)

-- | Run the IngestChainHistory phase.
--
-- TODO: orchestrate the receiver ('DbSync.Node.Connection.connectToNode')
-- and the consumer ('DbSync.Ingest.Consumer.runConsumer') in lock-step
-- under 'withAsync', then exit when the immutable tip is reached so the
-- caller can transition to 'DbSync.Phase.PreparingForVolatileTail'.
run :: IngestM ()
run = panic "TODO: not implemented"
