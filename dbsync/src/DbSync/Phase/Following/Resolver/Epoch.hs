-- | Follow 'IdResolver' fragment for the @epoch_sync_stats@ extractor.
--
-- Follow-phase plumbing not landed; both flavours use the same stub.
module DbSync.Phase.Following.Resolver.Epoch
  ( assignEpochSyncStatsIdStub
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Ids (EpochSyncStatsId)
import DbSync.Phase.Following.Resolver.Internal (todoResolve)

assignEpochSyncStatsIdStub :: IO EpochSyncStatsId
assignEpochSyncStatsIdStub = todoResolve "assignEpochSyncStatsId"
