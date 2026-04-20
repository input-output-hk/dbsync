-- | CBOR projection.
--
-- Optionally stores raw CBOR-encoded transaction bodies alongside
-- the parsed data, enabling downstream consumers to re-serialise.
module DbSync.Projection.Cbor
  ( cborProjection
  ) where

import Cardano.Prelude

-- | The CBOR projection definition.
cborProjection :: ()
cborProjection = panic "TODO: not implemented"
