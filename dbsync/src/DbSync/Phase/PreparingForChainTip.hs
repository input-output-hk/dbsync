-- | Preparing for Chain Tip phase.
--
-- Transitions from bulk ingest to tip-following: converts UNLOGGED tables
-- to LOGGED, creates indexes and constraints, and serialises dedup maps
-- as the final checkpoint.
module DbSync.Phase.PreparingForChainTip
  ( run
  ) where

import Cardano.Prelude

-- | Run the PreparingForChainTip phase.
run :: IO ()
run = panic "TODO: not implemented"
