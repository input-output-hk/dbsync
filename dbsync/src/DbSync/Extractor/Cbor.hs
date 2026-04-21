-- | CBOR extractor.
--
-- Optionally stores raw CBOR-encoded transaction bodies alongside
-- the parsed data, enabling downstream consumers to re-serialise.
module DbSync.Extractor.Cbor
  ( cborExtractor
  ) where

import Cardano.Prelude

-- | The CBOR extractor definition.
cborExtractor :: ()
cborExtractor = panic "TODO: not implemented"
