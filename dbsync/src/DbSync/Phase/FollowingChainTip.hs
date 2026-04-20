-- | Following Chain Tip phase.
--
-- Processes blocks as they arrive at the chain tip using standard
-- INSERT/UPDATE statements. Handles rollbacks via deletion cascades.
module DbSync.Phase.FollowingChainTip
  ( run
  ) where

import Cardano.Prelude

-- | Run the FollowingChainTip phase.
run :: IO ()
run = panic "TODO: not implemented"
