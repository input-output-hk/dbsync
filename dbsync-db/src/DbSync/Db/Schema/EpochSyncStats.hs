{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the EpochSyncStats extractor table: epoch_sync_stats.
--
-- Tracks sync performance metrics at each epoch boundary:
-- blocks processed, throughput, elapsed time, and sync phase.
-- Replaces the original @epoch_sync_time@ table with enhanced metrics.
module DbSync.Db.Schema.EpochSyncStats
  ( -- * Schema types
    EpochSyncStats (..)
  , SyncPhase (..)

    -- * Table definitions
  , epochSyncStatsTableDef

    -- * COPY encoding
  , encodeEpochSyncStatsCopy
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import DbSync.Db.Schema.Core (encodeInt64, encodeWord64)
import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Writer.Copy.Encoder (encodeToCopyRow)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key EpochSyncStats = EpochSyncStatsId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | Which sync phase was active when this epoch completed.
data SyncPhase
  = IngestChainHistory
  | FollowingChainTip
  deriving stock (Eq, Show)

-- | The @epoch_sync_stats@ table.
-- One row per epoch, recording sync performance metrics.
data EpochSyncStats = EpochSyncStats
  { epochSyncStatsEpochNo         :: !Word64
  , epochSyncStatsBlocksProcessed :: !Word64
  , epochSyncStatsBlocksPerSec    :: !Double
  , epochSyncStatsElapsedSec      :: !Double
  , epochSyncStatsSyncedAt        :: !UTCTime
  , epochSyncStatsPhase           :: !SyncPhase
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

epochSyncStatsTableDef :: TableDef
epochSyncStatsTableDef = TableDef
  { tdName    = "epoch_sync_stats"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt    False
      , ColumnDef "epoch_no"         PgBigInt    False
      , ColumnDef "blocks_processed" PgBigInt    False
      , ColumnDef "blocks_per_sec"   PgNumeric   False
      , ColumnDef "elapsed_sec"      PgNumeric   False
      , ColumnDef "synced_at"        PgTimestamp False
      , ColumnDef "phase"            PgText      False
      ]
  , tdMode = TableUnlogged
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeEpochSyncStatsCopy :: EpochSyncStatsId -> EpochSyncStats -> ByteString
encodeEpochSyncStatsCopy (EpochSyncStatsId essid) ess =
  encodeToCopyRow
    [ Just $ encodeInt64 essid
    , Just $ encodeWord64 (epochSyncStatsEpochNo ess)
    , Just $ encodeWord64 (epochSyncStatsBlocksProcessed ess)
    , Just $ encodeDouble (epochSyncStatsBlocksPerSec ess)
    , Just $ encodeDouble (epochSyncStatsElapsedSec ess)
    , Just $ encodeTimestamp (epochSyncStatsSyncedAt ess)
    , Just $ phaseToBytes (epochSyncStatsPhase ess)
    ]

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

encodeDouble :: Double -> ByteString
encodeDouble = BS8.pack . show

encodeTimestamp :: UTCTime -> ByteString
encodeTimestamp = BS8.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

phaseToBytes :: SyncPhase -> ByteString
phaseToBytes IngestChainHistory = "IngestChainHistory"
phaseToBytes FollowingChainTip  = "FollowingChainTip"
