-- | Ingest Chain History phase.
--
-- Bulk-loads historical blocks using PostgreSQL COPY streams and UNLOGGED
-- tables. Runs from genesis (or last checkpoint) up to the configured
-- catch-up threshold before the chain tip.
module DbSync.Phase.IngestChainHistory
  ( run
  ) where

import Cardano.Prelude

-- | Run the IngestChainHistory phase.
run :: IO ()
run = panic "TODO: not implemented"
