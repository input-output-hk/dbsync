-- | Current state projection.
--
-- Maintains denormalised \"current state\" tables (current UTxO set,
-- active delegations, etc.) for fast point-in-time queries.
module DbSync.Projection.CurrentState
  ( currentStateProjection
  ) where

import Cardano.Prelude

-- | The current state projection definition.
currentStateProjection :: ()
currentStateProjection = panic "TODO: not implemented"
