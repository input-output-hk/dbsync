-- | hasql writer for the table owned by the @epoch_sync_stats@
-- extractor. Both flavours panic via 'todoWriteLeaf' until the insert
-- statements are wired.
module DbSync.Phase.Following.Writer.Epoch
  ( writeEpochSyncStatsConn
  , writeEpochSyncStatsBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats)
import DbSync.Db.Schema.Ids (EpochSyncStatsId)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (todoWriteLeaf)

writeEpochSyncStatsConn :: Conn.Connection -> EpochSyncStatsId -> EpochSyncStats -> IO ()
writeEpochSyncStatsConn _ _ = todoWriteLeaf "writeEpochSyncStats"

writeEpochSyncStatsBuf :: WriteBuffer -> EpochSyncStatsId -> EpochSyncStats -> IO ()
writeEpochSyncStatsBuf _ _ = todoWriteLeaf "writeEpochSyncStats"
