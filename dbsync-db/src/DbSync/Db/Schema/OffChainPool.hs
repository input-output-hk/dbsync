{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for @off_chain_pool_data@ and
-- @off_chain_pool_fetch_error@. Both are identity-leaves written by
-- the off-chain pool worker.
module DbSync.Db.Schema.OffChainPool
  ( -- * Schema types
    OffChainPoolData (..)
  , OffChainPoolFetchError (..)

    -- * Table definitions
  , offChainPoolDataTableDef
  , offChainPoolFetchErrorTableDef

    -- * Column records (compile-time-safe column references)
  , OffChainPoolDataCols (..), offChainPoolDataCols, offChainPoolDataColsList
  , OffChainPoolFetchErrorCols (..), offChainPoolFetchErrorCols, offChainPoolFetchErrorColsList

    -- * Per-module column-record registry
  , offChainPoolColumnRecords

    -- * COPY encoding
  , encodeOffChainPoolDataCopy
  , encodeOffChainPoolFetchErrorCopy

    -- * Hasql encoders \/ decoders
  , offChainPoolDataEncoder
  , offChainPoolDataDecoder
  , entityOffChainPoolDataDecoder
  , offChainPoolFetchErrorEncoder
  , offChainPoolFetchErrorDecoder
  , entityOffChainPoolFetchErrorDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc, utcToLocalTime)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Loader.Encoder (buildCopyRow, bHex, bInt64, bText, bUTCTime, bWord64)

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key OffChainPoolData = OffChainPoolDataId
type instance Key OffChainPoolFetchError = OffChainPoolFetchErrorId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | One row per successful pool-metadata fetch; unique on
-- @(pool_id, pmr_id)@.
data OffChainPoolData = OffChainPoolData
  { offChainPoolDataPoolId     :: !PoolHashId
  , offChainPoolDataTickerName :: !Text
  , offChainPoolDataHash       :: !ByteString
  , offChainPoolDataJson       :: !Text
  , offChainPoolDataBytes      :: !ByteString
  , offChainPoolDataPmrId      :: !PoolMetadataRefId
  }
  deriving stock (Eq, Show)

-- | One row per failed (or retried) fetch attempt; unique on
-- @(pool_id, fetch_time, retry_count)@.
data OffChainPoolFetchError = OffChainPoolFetchError
  { offChainPoolFetchErrorPoolId     :: !PoolHashId
  , offChainPoolFetchErrorFetchTime  :: !UTCTime
  , offChainPoolFetchErrorPmrId      :: !PoolMetadataRefId
  , offChainPoolFetchErrorFetchError :: !Text
  , offChainPoolFetchErrorRetryCount :: !Word64
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

offChainPoolDataTableDef :: TableDef
offChainPoolDataTableDef = TableDef
  { tdName    = "off_chain_pool_data"
  , tdColumns =
      [ ColumnDef "id"          PgBigInt False
      , ColumnDef "pool_id"     PgBigInt False
      , ColumnDef "ticker_name" PgText   False
      , ColumnDef "hash"        PgBytea  False
      , ColumnDef "json"        PgJsonb  False
      , ColumnDef "bytes"       PgBytea  False
      , ColumnDef "pmr_id"      PgBigInt False
      ]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["pool_id" :| ["pmr_id"]]
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = ["id"]
  , tdParentRefs       = []
  }

offChainPoolFetchErrorTableDef :: TableDef
offChainPoolFetchErrorTableDef = TableDef
  { tdName    = "off_chain_pool_fetch_error"
  , tdColumns =
      [ ColumnDef "id"          PgBigInt    False
      , ColumnDef "pool_id"     PgBigInt    False
      , ColumnDef "fetch_time"  PgTimestamp False
      , ColumnDef "pmr_id"      PgBigInt    False
      , ColumnDef "fetch_error" PgText      False
      , ColumnDef "retry_count" PgBigInt    False
      ]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["pool_id" :| ["fetch_time", "retry_count"]]
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = ["id"]
  , tdParentRefs       = []
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data OffChainPoolDataCols = OffChainPoolDataCols
  { ocpdcId         :: !TableColumn
  , ocpdcPoolId     :: !TableColumn
  , ocpdcTickerName :: !TableColumn
  , ocpdcHash       :: !TableColumn
  , ocpdcJson       :: !TableColumn
  , ocpdcBytes      :: !TableColumn
  , ocpdcPmrId      :: !TableColumn
  }

offChainPoolDataCols :: OffChainPoolDataCols
offChainPoolDataCols =
  let c = TableColumn offChainPoolDataTableDef
  in OffChainPoolDataCols
       { ocpdcId         = c "id"
       , ocpdcPoolId     = c "pool_id"
       , ocpdcTickerName = c "ticker_name"
       , ocpdcHash       = c "hash"
       , ocpdcJson       = c "json"
       , ocpdcBytes      = c "bytes"
       , ocpdcPmrId      = c "pmr_id"
       }

offChainPoolDataColsList :: [TableColumn]
offChainPoolDataColsList =
  [ offChainPoolDataCols.ocpdcId
  , offChainPoolDataCols.ocpdcPoolId
  , offChainPoolDataCols.ocpdcTickerName
  , offChainPoolDataCols.ocpdcHash
  , offChainPoolDataCols.ocpdcJson
  , offChainPoolDataCols.ocpdcBytes
  , offChainPoolDataCols.ocpdcPmrId
  ]

data OffChainPoolFetchErrorCols = OffChainPoolFetchErrorCols
  { ocpfecId         :: !TableColumn
  , ocpfecPoolId     :: !TableColumn
  , ocpfecFetchTime  :: !TableColumn
  , ocpfecPmrId      :: !TableColumn
  , ocpfecFetchError :: !TableColumn
  , ocpfecRetryCount :: !TableColumn
  }

offChainPoolFetchErrorCols :: OffChainPoolFetchErrorCols
offChainPoolFetchErrorCols =
  let c = TableColumn offChainPoolFetchErrorTableDef
  in OffChainPoolFetchErrorCols
       { ocpfecId         = c "id"
       , ocpfecPoolId     = c "pool_id"
       , ocpfecFetchTime  = c "fetch_time"
       , ocpfecPmrId      = c "pmr_id"
       , ocpfecFetchError = c "fetch_error"
       , ocpfecRetryCount = c "retry_count"
       }

offChainPoolFetchErrorColsList :: [TableColumn]
offChainPoolFetchErrorColsList =
  [ offChainPoolFetchErrorCols.ocpfecId
  , offChainPoolFetchErrorCols.ocpfecPoolId
  , offChainPoolFetchErrorCols.ocpfecFetchTime
  , offChainPoolFetchErrorCols.ocpfecPmrId
  , offChainPoolFetchErrorCols.ocpfecFetchError
  , offChainPoolFetchErrorCols.ocpfecRetryCount
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

offChainPoolColumnRecords :: [(TableDef, [TableColumn])]
offChainPoolColumnRecords =
  [ (offChainPoolDataTableDef,       offChainPoolDataColsList)
  , (offChainPoolFetchErrorTableDef, offChainPoolFetchErrorColsList)
  ]

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeOffChainPoolDataCopy :: OffChainPoolData -> ByteString
encodeOffChainPoolDataCopy d =
  buildCopyRow
    [ Just $ bInt64 (getPoolHashId (offChainPoolDataPoolId d))
    , Just $ bText (offChainPoolDataTickerName d)
    , Just $ bHex (offChainPoolDataHash d)
    , Just $ bText (offChainPoolDataJson d)
    , Just $ bHex (offChainPoolDataBytes d)
    , Just $ bInt64 (getPoolMetadataRefId (offChainPoolDataPmrId d))
    ]

encodeOffChainPoolFetchErrorCopy :: OffChainPoolFetchError -> ByteString
encodeOffChainPoolFetchErrorCopy e =
  buildCopyRow
    [ Just $ bInt64 (getPoolHashId (offChainPoolFetchErrorPoolId e))
    , Just $ bUTCTime (offChainPoolFetchErrorFetchTime e)
    , Just $ bInt64 (getPoolMetadataRefId (offChainPoolFetchErrorPmrId e))
    , Just $ bText (offChainPoolFetchErrorFetchError e)
    , Just $ bWord64 (offChainPoolFetchErrorRetryCount e)
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- | The @json@ column is @jsonb@; the Haskell-side representation stays
-- 'Text'. PostgreSQL accepts raw JSON bytes through 'E.jsonbBytes'.
jsonbTextEncoder :: E.Value Text
jsonbTextEncoder = TE.encodeUtf8 >$< E.jsonbBytes

jsonbTextDecoder :: D.Value Text
jsonbTextDecoder = D.jsonbBytes $ \bs ->
  case TE.decodeUtf8' bs of
    Right t -> Right t
    Left  e -> Left (show e)

utcTimeAsTimestampEncoder :: E.Value UTCTime
utcTimeAsTimestampEncoder = utcToLocalTime utc >$< E.timestamp

utcTimeAsTimestampDecoder :: D.Value UTCTime
utcTimeAsTimestampDecoder = localTimeToUTC utc <$> D.timestamp

offChainPoolDataEncoder :: E.Params OffChainPoolData
offChainPoolDataEncoder = mconcat
  [ offChainPoolDataPoolId     >$< idEncoder getPoolHashId
  , offChainPoolDataTickerName >$< E.param (E.nonNullable E.text)
  , offChainPoolDataHash       >$< E.param (E.nonNullable E.bytea)
  , offChainPoolDataJson       >$< E.param (E.nonNullable jsonbTextEncoder)
  , offChainPoolDataBytes      >$< E.param (E.nonNullable E.bytea)
  , offChainPoolDataPmrId      >$< idEncoder getPoolMetadataRefId
  ]

offChainPoolDataDecoder :: D.Row OffChainPoolData
offChainPoolDataDecoder = OffChainPoolData
  <$> idDecoder PoolHashId
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable jsonbTextDecoder)
  <*> D.column (D.nonNullable D.bytea)
  <*> idDecoder PoolMetadataRefId

entityOffChainPoolDataDecoder :: D.Row (OffChainPoolDataId, OffChainPoolData)
entityOffChainPoolDataDecoder = (,)
  <$> idDecoder OffChainPoolDataId
  <*> offChainPoolDataDecoder

offChainPoolFetchErrorEncoder :: E.Params OffChainPoolFetchError
offChainPoolFetchErrorEncoder = mconcat
  [ offChainPoolFetchErrorPoolId     >$< idEncoder getPoolHashId
  , offChainPoolFetchErrorFetchTime  >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)
  , offChainPoolFetchErrorPmrId      >$< idEncoder getPoolMetadataRefId
  , offChainPoolFetchErrorFetchError >$< E.param (E.nonNullable E.text)
  , offChainPoolFetchErrorRetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  ]

offChainPoolFetchErrorDecoder :: D.Row OffChainPoolFetchError
offChainPoolFetchErrorDecoder = OffChainPoolFetchError
  <$> idDecoder PoolHashId
  <*> D.column (D.nonNullable utcTimeAsTimestampDecoder)
  <*> idDecoder PoolMetadataRefId
  <*> D.column (D.nonNullable D.text)
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityOffChainPoolFetchErrorDecoder :: D.Row (OffChainPoolFetchErrorId, OffChainPoolFetchError)
entityOffChainPoolFetchErrorDecoder = (,)
  <$> idDecoder OffChainPoolFetchErrorId
  <*> offChainPoolFetchErrorDecoder
