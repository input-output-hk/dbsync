-- | Owns the @epoch_finalized@ table plus the @epoch_current@ and
-- @epoch@ views. @pdProcess@ is a no-op: three phase hooks fill the
-- table with SQL instead of per-block COPY. They backfill at the end
-- of Ingest, append at each Follow boundary, and delete on a Follow
-- rollback.
module DbSync.Extractor.Epoch
  ( epochExtractor
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.EpochView (epochFinalizedTableDef)
import DbSync.Extractor (ExtractorDef (..))

epochExtractor :: ExtractorDef
epochExtractor = ExtractorDef
  { pdName    = "epoch"
  , pdTables  = [epochFinalizedTableDef]
  , pdProcess = \_ -> pure ()
  }
