-- | COPY writer for the table owned by the @epoch_sync_stats@ extractor.
module DbSync.Phase.Ingest.Writer.Epoch
  ( writeEpochSyncStatsCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.EpochSyncStats
  ( EpochSyncStats
  , encodeEpochSyncStatsCopy
  , epochSyncStatsTableDef
  )
import DbSync.Db.Schema.Ids (EpochSyncStatsId)
import DbSync.Db.Schema.Types (TableDef (..))

writeEpochSyncStatsCopy :: LoaderStream -> EpochSyncStatsId -> EpochSyncStats -> IO ()
writeEpochSyncStatsCopy ls essid ess =
  lsWriteRow ls (tdName epochSyncStatsTableDef) (encodeEpochSyncStatsCopy essid ess)
