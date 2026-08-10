{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema for the @epoch_finalized@ table plus the @epoch_current@ and
-- @epoch@ views.
--
-- @epoch_finalized@ holds every completed epoch, @epoch_current@
-- aggregates the active one live, and @epoch@ unions the two into the
-- public series. @epoch_finalized.id@ is @(epoch_no + 1)@, not an
-- auto-increment.
module DbSync.Db.Schema.EpochView
  ( -- * Schema type
    EpochFinalized (..)

    -- * Table definition
  , epochFinalizedTableDef
  , epochFinalizedTableName

    -- * Column records (compile-time-safe column references)
  , EpochFinalizedCols (..), epochFinalizedCols, epochFinalizedColsList

    -- * Per-module column-record registry
  , epochViewColumnRecords

    -- * COPY encoding (unused at runtime; kept for symmetry)
  , encodeEpochFinalizedCopy

    -- * View DDL
  , createEpochViewsSql
  , dropEpochViewsSql

    -- * View names (exported so callers can introspect)
  , epochCurrentViewName
  , epochViewName
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.WideWord (Word128)

import DbSync.Db.Schema.Core (BlockCols (..), blockCols)
import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids (EpochId, getEpochId)
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableColumn (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Types (DbLovelace (..), bWord128)
import DbSync.Db.Loader.Encoder
  ( buildCopyRow
  , bInt64
  , bUTCTime
  , bWord64
  )

-- ---------------------------------------------------------------------------
-- * Key type family instance
-- ---------------------------------------------------------------------------

type instance Key EpochFinalized = EpochId

-- ---------------------------------------------------------------------------
-- * Schema type
-- ---------------------------------------------------------------------------

-- | One completed epoch's aggregate. The active epoch comes from the
-- @epoch_current@ view instead.
data EpochFinalized = EpochFinalized
  { epochFinalizedOutSum    :: !Word128
  , epochFinalizedFees      :: !DbLovelace
  , epochFinalizedTxCount   :: !Word64
  , epochFinalizedBlkCount  :: !Word64
  , epochFinalizedNo        :: !Word64
  , epochFinalizedStartTime :: !UTCTime
  , epochFinalizedEndTime   :: !UTCTime
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definition
-- ---------------------------------------------------------------------------

epochFinalizedTableName :: Text
epochFinalizedTableName = "epoch_finalized"

-- | LOGGED from creation, because direct SQL populates this table and the
-- COPY pipeline never touches it. It therefore skips the UNLOGGED →
-- LOGGED flip in @PreparingForVolatileTail@. @UNIQUE (no)@ lets the
-- boundary append use @ON CONFLICT (no) DO UPDATE@.
epochFinalizedTableDef :: TableDef
epochFinalizedTableDef = TableDef
  { tdName    = epochFinalizedTableName
  , tdColumns =
      [ ColumnDef "id"         PgBigInt    False
      , ColumnDef "out_sum"    PgNumeric   False
      , ColumnDef "fees"       PgNumeric   False
      , ColumnDef "tx_count"   PgBigInt    False
      , ColumnDef "blk_count"  PgBigInt    False
      , ColumnDef "no"         PgBigInt    False
      , ColumnDef "start_time" PgTimestamp False
      , ColumnDef "end_time"   PgTimestamp False
      ]
  , tdMode              = TableLogged
  , tdPrimaryKey        = Just ["id"]
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = [pure "no"]
  , tdGeneratedColumns  = []
  , tdIdentityColumns = []
  , tdParentRefs       = []
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data EpochFinalizedCols = EpochFinalizedCols
  { efcId        :: !TableColumn
  , efcOutSum    :: !TableColumn
  , efcFees      :: !TableColumn
  , efcTxCount   :: !TableColumn
  , efcBlkCount  :: !TableColumn
  , efcNo        :: !TableColumn
  , efcStartTime :: !TableColumn
  , efcEndTime   :: !TableColumn
  }

epochFinalizedCols :: EpochFinalizedCols
epochFinalizedCols =
  let c = TableColumn epochFinalizedTableDef
  in EpochFinalizedCols
       { efcId        = c "id"
       , efcOutSum    = c "out_sum"
       , efcFees      = c "fees"
       , efcTxCount   = c "tx_count"
       , efcBlkCount  = c "blk_count"
       , efcNo        = c "no"
       , efcStartTime = c "start_time"
       , efcEndTime   = c "end_time"
       }

epochFinalizedColsList :: [TableColumn]
epochFinalizedColsList =
  [ epochFinalizedCols.efcId
  , epochFinalizedCols.efcOutSum
  , epochFinalizedCols.efcFees
  , epochFinalizedCols.efcTxCount
  , epochFinalizedCols.efcBlkCount
  , epochFinalizedCols.efcNo
  , epochFinalizedCols.efcStartTime
  , epochFinalizedCols.efcEndTime
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

epochViewColumnRecords :: [(TableDef, [TableColumn])]
epochViewColumnRecords =
  [ (epochFinalizedTableDef, epochFinalizedColsList)
  ]

-- ---------------------------------------------------------------------------
-- * COPY encoding (symmetry only)
-- ---------------------------------------------------------------------------

-- | Only the encoder round-trip tests call this. The runtime populates
-- the table by SQL.
encodeEpochFinalizedCopy :: EpochId -> EpochFinalized -> ByteString
encodeEpochFinalizedCopy eid ef =
  buildCopyRow
    [ Just $ bInt64 (getEpochId eid)
    , Just $ bWord128 (epochFinalizedOutSum ef)
    , Just $ bWord64 (unDbLovelace (epochFinalizedFees ef))
    , Just $ bWord64 (epochFinalizedTxCount ef)
    , Just $ bWord64 (epochFinalizedBlkCount ef)
    , Just $ bWord64 (epochFinalizedNo ef)
    , Just $ bUTCTime (epochFinalizedStartTime ef)
    , Just $ bUTCTime (epochFinalizedEndTime ef)
    ]

-- ---------------------------------------------------------------------------
-- * View names
-- ---------------------------------------------------------------------------

epochCurrentViewName :: Text
epochCurrentViewName = "epoch_current"

epochViewName :: Text
epochViewName = "epoch"

-- ---------------------------------------------------------------------------
-- * View DDL
-- ---------------------------------------------------------------------------

-- | @CREATE VIEW@ DDL for @epoch_current@, then @epoch@.
--
-- @epoch_current@ aggregates the un-finalised epoch live from @block@ and
-- @tx@. The @NOT EXISTS@ guard skips any epoch that @epoch_finalized@
-- already holds, because the @UNION ALL@ in @epoch@ would double-count it.
-- A per-row check, rather than @> MAX(no)@, also tolerates a
-- non-contiguous finalised set.
createEpochViewsSql :: Text
createEpochViewsSql = T.unlines
  [ "CREATE VIEW " <> epochCurrentViewName <> " AS"
  , "  SELECT"
  , "    (b." <> epochNo <> "::bigint + 1)                AS " <> idCol <> ","
  , "    COALESCE(SUM(tx.out_sum), 0)::numeric            AS " <> outSum <> ","
  , "    COALESCE(SUM(tx.fee), 0)                         AS " <> fees <> ","
  , "    COUNT(tx.id)::bigint                             AS " <> txCount <> ","
  , "    COUNT(DISTINCT b." <> blkId <> ")::bigint        AS " <> blkCount <> ","
  , "    b." <> epochNo <> "::bigint                      AS " <> noCol <> ","
  , "    MIN(b." <> bTime <> ")                           AS " <> startTime <> ","
  , "    MAX(b." <> bTime <> ")                           AS " <> endTime
  , "  FROM block b"
  , "  LEFT JOIN tx ON tx.block_id = b." <> blkId
  , "  WHERE b." <> epochNo <> " IS NOT NULL"
  , "    AND NOT EXISTS (SELECT 1 FROM \"" <> epochFinalizedTableName
       <> "\" ef WHERE ef." <> noCol <> " = b." <> epochNo <> ")"
  , "  GROUP BY b." <> epochNo <> ";"
  , ""
  , "CREATE VIEW " <> epochViewName <> " AS"
  , "  SELECT " <> projection
  , "    FROM " <> epochFinalizedTableName
  , "  UNION ALL"
  , "  SELECT " <> projection
  , "    FROM " <> epochCurrentViewName <> ";"
  ]
  where
    idCol     = epochFinalizedCols.efcId.tcName
    outSum    = epochFinalizedCols.efcOutSum.tcName
    fees      = epochFinalizedCols.efcFees.tcName
    txCount   = epochFinalizedCols.efcTxCount.tcName
    blkCount  = epochFinalizedCols.efcBlkCount.tcName
    noCol     = epochFinalizedCols.efcNo.tcName
    startTime = epochFinalizedCols.efcStartTime.tcName
    endTime   = epochFinalizedCols.efcEndTime.tcName
    epochNo   = blockCols.bcEpochNo.tcName
    blkId     = blockCols.bcId.tcName
    bTime     = blockCols.bcTime.tcName
    projection = T.intercalate ", "
      [ idCol, outSum, fees, txCount, blkCount, noCol, startTime, endTime ]

-- | Runs before the table's @DROP TABLE@. Order matters: @epoch@ depends
-- on @epoch_current@ and @epoch_finalized@, so @epoch@ goes first.
dropEpochViewsSql :: Text
dropEpochViewsSql = T.unlines
  [ "DROP VIEW IF EXISTS " <> epochViewName <> ";"
  , "DROP VIEW IF EXISTS " <> epochCurrentViewName <> ";"
  ]
