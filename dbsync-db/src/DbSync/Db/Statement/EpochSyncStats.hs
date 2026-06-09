-- | Hasql 'Statement' bindings for the @epoch_sync_stats@ extractor
-- table: @epoch_sync_stats@.
module DbSync.Db.Statement.EpochSyncStats
  ( insertEpochSyncStatsRowStmt
  , nextEpochSyncStatsIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.EpochSyncStats
  ( EpochSyncStats
  , epochSyncStatsEncoder
  , epochSyncStatsTableDef
  )
import DbSync.Db.Schema.Ids (EpochSyncStatsId (..), idEncoder)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt)

-- ---------------------------------------------------------------------------
-- * epoch_sync_stats
-- ---------------------------------------------------------------------------

insertEpochSyncStatsRowStmt :: Stmt.Statement (EpochSyncStatsId, EpochSyncStats) ()
insertEpochSyncStatsRowStmt =
  Stmt.preparable (insertRowSql epochSyncStatsTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getEpochSyncStatsId)
           <> (snd >$< epochSyncStatsEncoder)

nextEpochSyncStatsIdStmt :: Stmt.Statement () EpochSyncStatsId
nextEpochSyncStatsIdStmt = nextIdStmt epochSyncStatsTableDef EpochSyncStatsId
