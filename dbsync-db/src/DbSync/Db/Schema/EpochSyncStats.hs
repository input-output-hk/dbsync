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
  , EpochSyncTime (..)

    -- * Table definitions
  , epochSyncStatsTableDef
  , epochSyncTimeTableDef

    -- * COPY encoding
  , encodeEpochSyncStatsCopy
  , encodeEpochSyncTimeCopy

    -- * Hasql encoders \/ decoders
  , epochSyncStatsEncoder
  , epochSyncStatsDecoder
  , entityEpochSyncStatsDecoder
  ) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc, utcToLocalTime)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types
  ( SyncState
  , bSyncState
  , doubleAsNumericDecoder
  , doubleAsNumericEncoder
  )
import DbSync.Db.Loader.Encoder (buildCopyRow, bInt64, bUTCTime, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key EpochSyncStats = EpochSyncStatsId
type instance Key EpochSyncTime = EpochSyncTimeId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @epoch_sync_stats@ table.
-- One row per epoch, recording sync performance metrics.
data EpochSyncStats = EpochSyncStats
  { epochSyncStatsEpochNo         :: !Word64
  , epochSyncStatsBlocksProcessed :: !Word64
  , epochSyncStatsBlocksPerSec    :: !Double
  , epochSyncStatsElapsedSec      :: !Double
  , epochSyncStatsSyncedAt        :: !UTCTime
  , epochSyncStatsPhase           :: !Text
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

-- | Counter-managed (not @IDENTITY@) because the @UNIQUE (epoch_no)@
-- constraint is enforced on an on-chain natural key. A mid-Ingest
-- crash can leave a row past the @sync_state@ snapshot; the resume
-- cleanup needs the per-table counter pass to delete it before the
-- next boundary's COPY re-emits the same @epoch_no@.
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
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "epoch_no"]
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdForeignKeys = []
  }

epochSyncTimeTableDef :: TableDef
epochSyncTimeTableDef = TableDef
  { tdName    = "epoch_sync_time"
  , tdColumns =
      [ ColumnDef "id"      PgBigInt False
      , ColumnDef "no"      PgBigInt False
      , ColumnDef "seconds" PgBigInt False
      , ColumnDef "state"   PgText   False
      ]
  , tdMode    = TableUnlogged
  , tdPrimaryKey     = Nothing
  , tdChecks         = []
  , tdColumnDefaults = []
  , tdUniqueConstraints = [pure "no"]
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
  , tdForeignKeys = []
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
    , Just $ byteString (TE.encodeUtf8 (epochSyncStatsPhase ess))
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
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

epochSyncStatsEncoder :: E.Params EpochSyncStats
epochSyncStatsEncoder = mconcat
  [ epochSyncStatsEpochNo         >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochSyncStatsBlocksProcessed >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , epochSyncStatsBlocksPerSec    >$< E.param (E.nonNullable doubleAsNumericEncoder)
  , epochSyncStatsElapsedSec      >$< E.param (E.nonNullable doubleAsNumericEncoder)
  , epochSyncStatsSyncedAt        >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)
  , epochSyncStatsPhase           >$< E.param (E.nonNullable E.text)
  ]

epochSyncStatsDecoder :: D.Row EpochSyncStats
epochSyncStatsDecoder = EpochSyncStats
  <$> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable doubleAsNumericDecoder)
  <*> D.column (D.nonNullable doubleAsNumericDecoder)
  <*> D.column (D.nonNullable utcTimeAsTimestampDecoder)
  <*> D.column (D.nonNullable D.text)

entityEpochSyncStatsDecoder :: D.Row (EpochSyncStatsId, EpochSyncStats)
entityEpochSyncStatsDecoder = (,)
  <$> idDecoder EpochSyncStatsId
  <*> epochSyncStatsDecoder

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

bDouble :: Double -> Builder
bDouble = byteString . BS8.pack . show

utcTimeAsTimestampEncoder :: E.Value UTCTime
utcTimeAsTimestampEncoder = utcToLocalTime utc >$< E.timestamp

utcTimeAsTimestampDecoder :: D.Value UTCTime
utcTimeAsTimestampDecoder = localTimeToUTC utc <$> D.timestamp
