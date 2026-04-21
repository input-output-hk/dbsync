-- | Core extractor.
--
-- Writes the fundamental tables: block, tx, tx_in, tx_out.
-- This extractor is always enabled and cannot be disabled.
module DbSync.Extractor.Core
  ( coreExtractor
  ) where

import Cardano.Prelude

-- | The core extractor definition (block, tx, tx_in, tx_out tables).
-- TODO: Return a ExtractorDef once DbSync.Extractor is defined.
coreExtractor :: ()
coreExtractor = panic "TODO: not implemented"
