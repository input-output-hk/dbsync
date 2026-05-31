-- | Ingest 'IdResolver' fragment for the @epoch_sync_stats@ extractor.
module DbSync.Phase.Ingest.Resolver.Epoch
  ( assignEpochSyncStatsIdIngest
  ) where

import Cardano.Prelude

import Data.IORef (IORef)

import DbSync.Db.Schema.Ids (EpochSyncStatsId (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..))
import DbSync.Phase.Ingest.Resolver.Internal (bump)

assignEpochSyncStatsIdIngest :: IORef ExtractState -> IO EpochSyncStatsId
assignEpochSyncStatsIdIngest stRef = bump stRef icEpochSyncStatsId (\cs c -> cs { icEpochSyncStatsId = c }) EpochSyncStatsId
