-- | Core projection.
--
-- Writes the fundamental tables: block, tx, tx_in, tx_out.
-- This projection is always enabled and cannot be disabled.
module DbSync.Projection.Core
  ( coreProjection
  ) where

import Cardano.Prelude

-- | The core projection definition (block, tx, tx_in, tx_out tables).
-- TODO: Return a ProjectionDef once DbSync.Projection is defined.
coreProjection :: ()
coreProjection = panic "TODO: not implemented"
