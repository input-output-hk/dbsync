{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for the @epoch_sync_stats@ extractor.
--
-- The extractor owns two tables, both written by the consumer thread
-- at each epoch-boundary commit:
--
--   * @epoch_sync_stats@ — our metrics (blocks/sec, throughput, phase).
--   * @epoch_sync_time@ — the original-project parity table
--     (epoch number, elapsed seconds, sync state).
module DbSync.Db.Schema.EpochSyncStats
  ( -- * Schema types
    EpochSyncStats (..)
  , SyncPhase (..)
  , EpochSyncTime (..)

    -- * Table definitions
  , epochSyncStatsTableDef
  , epochSyncTimeTableDef

    -- * COPY encoding
  , encodeEpochSyncStatsCopy
  , encodeEpochSyncTimeCopy
  ) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock (UTCTime)

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types (SyncState, bSyncState)
import DbSync.Db.Writer.Copy.Encoder (buildCopyRow, bInt64, bUTCTime, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key EpochSyncStats = EpochSyncStatsId
type instance Key EpochSyncTime = EpochSyncTimeId

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

-- | The @epoch_sync_time@ table.
-- Original-project parity. Unique on @no@.
data EpochSyncTime = EpochSyncTime
  { epochSyncTimeNo      :: !Word64
  , epochSyncTimeSeconds :: !Word64
  , epochSyncTimeState   :: !SyncState
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
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Just ["id"]
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "epoch_no"]
  , tdGeneratedColumns = []
  }

epochSyncTimeTableDef :: TableDef
epochSyncTimeTableDef = TableDef
  { tdName    = "epoch_sync_time"
  , tdColumns =
      [ ColumnDef "id"      PgBigInt              False
      , ColumnDef "no"      PgBigInt              False
      , ColumnDef "seconds" PgBigInt              False
      , ColumnDef "state"   (PgEnum "syncstatetype") False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "no"]
  , tdGeneratedColumns = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeEpochSyncStatsCopy :: EpochSyncStatsId -> EpochSyncStats -> ByteString
encodeEpochSyncStatsCopy (EpochSyncStatsId essid) ess =
  buildCopyRow
    [ Just $ bInt64 essid
    , Just $ bWord64 (epochSyncStatsEpochNo ess)
    , Just $ bWord64 (epochSyncStatsBlocksProcessed ess)
    , Just $ bDouble (epochSyncStatsBlocksPerSec ess)
    , Just $ bDouble (epochSyncStatsElapsedSec ess)
    , Just $ bUTCTime (epochSyncStatsSyncedAt ess)
    , Just $ bPhase (epochSyncStatsPhase ess)
    ]

encodeEpochSyncTimeCopy :: EpochSyncTimeId -> EpochSyncTime -> ByteString
encodeEpochSyncTimeCopy (EpochSyncTimeId estid) est =
  buildCopyRow
    [ Just $ bInt64 estid
    , Just $ bWord64 (epochSyncTimeNo est)
    , Just $ bWord64 (epochSyncTimeSeconds est)
    , Just $ bSyncState (epochSyncTimeState est)
    ]

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

bDouble :: Double -> Builder
bDouble = byteString . BS8.pack . show

bPhase :: SyncPhase -> Builder
bPhase IngestChainHistory = byteString "IngestChainHistory"
bPhase FollowingChainTip  = byteString "FollowingChainTip"
