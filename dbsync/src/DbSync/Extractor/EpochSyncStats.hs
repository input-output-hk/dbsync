{-# LANGUAGE OverloadedStrings #-}

-- | Owns the @epoch_sync_stats@ table, which records sync performance
-- at each epoch boundary. @pdProcess@ is a no-op: the Consumer writes
-- the stats through the Writer when it commits the boundary.
module DbSync.Extractor.EpochSyncStats
  ( epochSyncStatsExtractor
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.EpochSyncStats (epochSyncStatsTableDef)
import DbSync.Extractor (ExtractorDef (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

epochSyncStatsExtractor :: ExtractorDef
epochSyncStatsExtractor = ExtractorDef
  { pdName    = "epoch_sync_stats"
  , pdTables  = [epochSyncStatsTableDef]
  , pdProcess = \_ -> pure ()  -- No-op: Consumer writes directly
  }
