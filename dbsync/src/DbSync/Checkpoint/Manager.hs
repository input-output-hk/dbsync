-- | Checkpoint orchestration.
--
-- Coordinates periodic checkpointing during IngestChainHistory:
-- serialises dedup maps and records the current slot so that
-- restart can resume without replaying from genesis.
module DbSync.Checkpoint.Manager
  ( -- TODO: checkpoint, restoreFromCheckpoint
  ) where

import Cardano.Prelude
