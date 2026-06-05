-- | Follow 'IdResolver' fragment for the @epoch_sync_stats@ extractor.
module DbSync.Phase.Following.Resolver.Epoch
  ( assignEpochSyncStatsIdConn
  , assignEpochSyncStatsIdBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids (EpochSyncStatsId)
import DbSync.Db.Statement.EpochSyncStats (nextEpochSyncStatsIdStmt)
import DbSync.Phase.Following.Resolver.Internal (runStmt)

assignEpochSyncStatsIdConn :: Conn.Connection -> IO EpochSyncStatsId
assignEpochSyncStatsIdConn conn = runStmt conn () nextEpochSyncStatsIdStmt

-- | @epoch_sync_stats@ is counter-managed but has no pre-allocation
-- lane: the row fires at most once per epoch boundary, not per block,
-- so one synchronous @nextval@ round-trip is preferable to a counter
-- field on 'PreAllocatedIds' threaded through every block.
assignEpochSyncStatsIdBuf :: Conn.Connection -> IO EpochSyncStatsId
assignEpochSyncStatsIdBuf conn = runStmt conn () nextEpochSyncStatsIdStmt
