-- | Current state extractor.
--
-- Maintains denormalised \"current state\" tables (current UTxO set,
-- active delegations, etc.) for fast point-in-time queries.
module DbSync.Extractor.CurrentState
  ( currentStateExtractor
  ) where

import Cardano.Prelude

-- | The current state extractor definition.
currentStateExtractor :: ()
currentStateExtractor = panic "TODO: not implemented"
