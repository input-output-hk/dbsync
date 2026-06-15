-- | Extractor that owns the @epoch_finalized@ table plus the
-- @epoch_current@ and @epoch@ views.
--
-- @pdProcess@ is a no-op: the table is populated by SQL from three
-- phase hooks (backfill at end of Ingest, append at each Follow
-- boundary, delete on Follow rollback) rather than from per-block
-- COPY. Registering the 'TableDef' here is enough to make
-- 'DbSync.Db.Schema.Init.initSchema' create the table and emit the
-- view DDL when the extractor is enabled.
module DbSync.Extractor.Epoch
  ( epochExtractor
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.EpochView (epochFinalizedTableDef)
import DbSync.Extractor (ExtractorDef (..))

-- | The @epoch@ extractor.
epochExtractor :: ExtractorDef
epochExtractor = ExtractorDef
  { pdName    = "epoch"
  , pdTables  = [epochFinalizedTableDef]
  , pdProcess = \_ -> pure ()
  }
