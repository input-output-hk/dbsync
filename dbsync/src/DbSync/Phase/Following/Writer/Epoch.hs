-- | hasql writer for the table owned by the @epoch_sync_stats@
-- extractor.
module DbSync.Phase.Following.Writer.Epoch
  ( writeEpochSyncStatsConn
  , writeEpochSyncStatsBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats)
import DbSync.Db.Schema.Ids (EpochSyncStatsId)
import DbSync.Db.Statement.EpochSyncStats (insertEpochSyncStatsRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeEpochSyncStatsConn
  :: Conn.Connection -> EpochSyncStatsId -> EpochSyncStats -> IO ()
writeEpochSyncStatsConn conn essid ess =
  runConn conn (essid, ess) insertEpochSyncStatsRowStmt

writeEpochSyncStatsBuf
  :: WriteBuffer -> EpochSyncStatsId -> EpochSyncStats -> IO ()
writeEpochSyncStatsBuf buf essid ess =
  queueBuf buf (essid, ess) insertEpochSyncStatsRowStmt
