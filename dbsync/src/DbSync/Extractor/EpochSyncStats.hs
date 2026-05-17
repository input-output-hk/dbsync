{-# LANGUAGE OverloadedStrings #-}

-- | EpochSyncStats extractor.
--
-- Owns the @epoch_sync_stats@ table for tracking sync performance
-- metrics at each epoch boundary. This extractor has a no-op
-- 'pdProcess' — the Consumer writes epoch stats directly via
-- the Writer at epoch boundary commit time.
--
-- Defining it as an extractor ensures the table schema is
-- created alongside all other extractor tables.
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
  { pdName         = "epoch_sync_stats"
  , pdVersion      = 1
  , pdDependencies = []  -- Independent meta-data, no block data dependencies
  , pdTables       = [epochSyncStatsTableDef]
  , pdProcess      = \_ -> pure ()  -- No-op: Consumer writes directly
  }
