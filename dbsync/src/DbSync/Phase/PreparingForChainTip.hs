-- | Preparing for Chain Tip phase.
--
-- Transitions from bulk ingest to tip-following: converts UNLOGGED tables
-- to LOGGED, creates indexes and constraints, and serialises dedup maps
-- as the final checkpoint.
module DbSync.Phase.PreparingForChainTip
  ( run
  ) where

import Cardano.Prelude

import DbSync.AppM (CoreM)

-- | Run the PreparingForChainTip phase.
--
-- TODO: drive the DDL transition (LOGGED tables, index + constraint
-- creation, @ANALYZE@) using the connection string from the env-level
-- 'DbSync.Config.Types.SyncConfig', then return so the caller can
-- transition to 'DbSync.Phase.FollowingChainTip'.
run :: CoreM ()
run = panic "TODO: not implemented"
