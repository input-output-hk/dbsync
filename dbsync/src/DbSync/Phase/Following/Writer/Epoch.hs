-- | hasql writer for the table owned by the @epoch_sync_stats@ extractor.
--
-- Follow-phase insert plumbing not landed yet; both flavours fall
-- through to 'todoWrite' for now.
module DbSync.Phase.Following.Writer.Epoch
  ( writeEpochSyncStatsConn
  , writeEpochSyncStatsBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats)
import DbSync.Db.Schema.Ids (EpochSyncStatsId)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (todoWrite)

writeEpochSyncStatsConn :: Conn.Connection -> EpochSyncStatsId -> EpochSyncStats -> IO ()
writeEpochSyncStatsConn _ = todoWrite "writeEpochSyncStats"

writeEpochSyncStatsBuf :: WriteBuffer -> EpochSyncStatsId -> EpochSyncStats -> IO ()
writeEpochSyncStatsBuf _ = todoWrite "writeEpochSyncStats"
