-- | Following Chain Tip phase.
--
-- Processes blocks as they arrive at the chain tip using standard
-- INSERT/UPDATE statements. Handles rollbacks via deletion cascades.
module DbSync.Phase.FollowingChainTip
  ( run
  ) where

import Cardano.Prelude

import DbSync.AppM (FollowM)

-- | Run the FollowingChainTip phase.
--
-- TODO: drive the per-block INSERT pipeline against the env-level
-- 'DbSync.Env.FollowEnv'; this is the steady-state phase that runs
-- indefinitely once the immutable tip has been reached.
run :: FollowM ()
run = panic "TODO: not implemented"
