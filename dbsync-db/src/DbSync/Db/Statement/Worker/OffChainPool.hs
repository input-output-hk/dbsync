{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @off_chain_pools@ extractor:
-- result tables, SMASH-maintained tables, and the work-queue lookups
-- the worker uses to find pending or due-for-retry refs.
module DbSync.Db.Statement.Worker.OffChainPool
  ( -- * Inserts
    insertOffChainPoolDataRowStmt
  , insertOffChainPoolFetchErrorRowStmt
  , insertDelistedPoolRowStmt
  , insertReservedPoolTickerRowStmt

    -- * Work-queue lookups
  , PendingPoolFetch (..)
  , queryNewPendingPoolFetchesStmt
  , queryRetryPendingPoolFetchesStmt

    -- * Retry-count lookup
  , selectMaxRetryCountStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids
  ( PoolHashId (..)
  , PoolMetadataRefId (..)
  , idDecoder
  , idEncoder
  )
import DbSync.Db.Schema.OffChainPool
  ( OffChainPoolData
  , OffChainPoolDataCols (..)
  , OffChainPoolFetchError
  , OffChainPoolFetchErrorCols (..)
  , offChainPoolDataCols
  , offChainPoolDataEncoder
  , offChainPoolDataTableDef
  , offChainPoolFetchErrorCols
  , offChainPoolFetchErrorEncoder
  , offChainPoolFetchErrorTableDef
  )
import DbSync.Db.Schema.Pool
  ( DelistedPool
  , PoolHashCols (..)
  , PoolMetadataRefCols (..)
  , ReservedPoolTicker
  , delistedPoolEncoder
  , delistedPoolTableDef
  , poolHashCols
  , poolHashTableDef
  , poolMetadataRefCols
  , poolMetadataRefTableDef
  , reservedPoolTickerEncoder
  , reservedPoolTickerTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)
import DbSync.Db.Statement.Common (insertRowSql)

-- ---------------------------------------------------------------------------
-- * Inserts
-- ---------------------------------------------------------------------------

insertOffChainPoolDataRowStmt :: Stmt.Statement OffChainPoolData ()
insertOffChainPoolDataRowStmt =
  Stmt.preparable (insertRowSql offChainPoolDataTableDef) offChainPoolDataEncoder D.noResult

insertOffChainPoolFetchErrorRowStmt :: Stmt.Statement OffChainPoolFetchError ()
insertOffChainPoolFetchErrorRowStmt =
  Stmt.preparable
    (insertRowSql offChainPoolFetchErrorTableDef)
    offChainPoolFetchErrorEncoder
    D.noResult

insertDelistedPoolRowStmt :: Stmt.Statement DelistedPool ()
insertDelistedPoolRowStmt =
  Stmt.preparable (insertRowSql delistedPoolTableDef) delistedPoolEncoder D.noResult

insertReservedPoolTickerRowStmt :: Stmt.Statement ReservedPoolTicker ()
insertReservedPoolTickerRowStmt =
  Stmt.preparable
    (insertRowSql reservedPoolTickerTableDef)
    reservedPoolTickerEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * Work-queue lookups
-- ---------------------------------------------------------------------------

-- | One ref in the off-chain pool fetch work queue.
-- 'ppfPrevFetchTime' is 'Nothing' for never-attempted refs and
-- @Just t@ (with matching retry count) for refs due to retry.
data PendingPoolFetch = PendingPoolFetch
  { ppfPoolId         :: !PoolHashId
  , ppfPmrId          :: !PoolMetadataRefId
  , ppfUrl            :: !Text
  , ppfHash           :: !ByteString
  , ppfPrevFetchTime  :: !(Maybe UTCTime)
  , ppfPrevRetryCount :: !Word64
  }
  deriving stock (Eq, Show)

-- | Latest @pool_metadata_ref@ per pool with neither a success nor a
-- recorded fetch error yet.
queryNewPendingPoolFetchesStmt :: Stmt.Statement Int32 [PendingPoolFetch]
queryNewPendingPoolFetchesStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = mconcat
      [ "WITH latest_refs AS ("
      , " SELECT MAX(", col poolMetadataRefCols.pmrcId, ") AS max_id"
      , " FROM ", table poolMetadataRefTableDef
      , " GROUP BY ", col poolMetadataRefCols.pmrcPoolId
      , ")"
      , "SELECT ", qcol "ph" poolHashCols.phcId
      , ", ", qcol "pmr" poolMetadataRefCols.pmrcId
      , ", ", qcol "pmr" poolMetadataRefCols.pmrcUrl
      , ", ", qcol "pmr" poolMetadataRefCols.pmrcHash
      , " FROM ", table poolHashTableDef, " ph"
      , " INNER JOIN ", table poolMetadataRefTableDef, " pmr"
      ,   " ON ", qcol "ph" poolHashCols.phcId
      ,   " = ", qcol "pmr" poolMetadataRefCols.pmrcPoolId
      , " WHERE ", qcol "pmr" poolMetadataRefCols.pmrcId, " IN (SELECT max_id FROM latest_refs)"
      , "   AND NOT EXISTS ("
      , "     SELECT 1 FROM ", table offChainPoolDataTableDef, " pod"
      , "     WHERE ", qcol "pod" offChainPoolDataCols.ocpdcPmrId
      ,     " = ", qcol "pmr" poolMetadataRefCols.pmrcId
      , "   )"
      , "   AND NOT EXISTS ("
      , "     SELECT 1 FROM ", table offChainPoolFetchErrorTableDef, " pofe"
      , "     WHERE ", qcol "pofe" offChainPoolFetchErrorCols.ocpfecPmrId
      ,     " = ", qcol "pmr" poolMetadataRefCols.pmrcId
      , "   )"
      , " LIMIT $1"
      ]

    encoder = E.param (E.nonNullable E.int4)

    decoder = D.rowList $
      (\phId pmrId url h -> PendingPoolFetch phId pmrId url h Nothing 0)
        <$> idDecoder PoolHashId
        <*> idDecoder PoolMetadataRefId
        <*> D.column (D.nonNullable D.text)
        <*> D.column (D.nonNullable D.bytea)

-- | Refs whose most recent attempt was a recorded failure with no
-- later success. Returns the prior fetch time and retry count so the
-- caller can apply exponential backoff.
queryRetryPendingPoolFetchesStmt :: Stmt.Statement Int32 [PendingPoolFetch]
queryRetryPendingPoolFetchesStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = mconcat
      [ "WITH latest_errors AS ("
      , " SELECT MAX(", col offChainPoolFetchErrorCols.ocpfecId, ") AS max_id"
      , " FROM ", table offChainPoolFetchErrorTableDef
      , " WHERE NOT EXISTS ("
      , "   SELECT 1 FROM ", table offChainPoolDataTableDef, " pod"
      , "   WHERE ", qcol "pod" offChainPoolDataCols.ocpdcPmrId
      ,   " = ", qcol (table offChainPoolFetchErrorTableDef) offChainPoolFetchErrorCols.ocpfecPmrId
      , " )"
      , " GROUP BY ", col offChainPoolFetchErrorCols.ocpfecPoolId
      , ")"
      , "SELECT ", qcol "ph" poolHashCols.phcId
      , ", ", qcol "pmr" poolMetadataRefCols.pmrcId
      , ", ", qcol "pmr" poolMetadataRefCols.pmrcUrl
      , ", ", qcol "pmr" poolMetadataRefCols.pmrcHash
      , ", ", qcol "pofe" offChainPoolFetchErrorCols.ocpfecFetchTime
      , ", ", qcol "pofe" offChainPoolFetchErrorCols.ocpfecRetryCount
      , " FROM ", table poolHashTableDef, " ph"
      , " INNER JOIN ", table poolMetadataRefTableDef, " pmr"
      ,   " ON ", qcol "ph" poolHashCols.phcId
      ,   " = ", qcol "pmr" poolMetadataRefCols.pmrcPoolId
      , " INNER JOIN ", table offChainPoolFetchErrorTableDef, " pofe"
      ,   " ON ", qcol "pofe" offChainPoolFetchErrorCols.ocpfecPmrId
      ,   " = ", qcol "pmr" poolMetadataRefCols.pmrcId
      , " WHERE ", qcol "pofe" offChainPoolFetchErrorCols.ocpfecId, " IN (SELECT max_id FROM latest_errors)"
      , " ORDER BY ", qcol "pofe" offChainPoolFetchErrorCols.ocpfecId, " ASC"
      , " LIMIT $1"
      ]

    encoder = E.param (E.nonNullable E.int4)

    decoder = D.rowList $
      (\phId pmrId url h t r -> PendingPoolFetch phId pmrId url h (Just t) r)
        <$> idDecoder PoolHashId
        <*> idDecoder PoolMetadataRefId
        <*> D.column (D.nonNullable D.text)
        <*> D.column (D.nonNullable D.bytea)
        <*> D.column (D.nonNullable (localTimeToUTC utc <$> D.timestamp))
        <*> D.column (D.nonNullable (fromIntegral <$> D.int8))

-- ---------------------------------------------------------------------------
-- * Retry-count lookup
-- ---------------------------------------------------------------------------

-- | Highest @retry_count@ recorded for a @(pool_id, pmr_id)@, or
-- 'Nothing' if no error row exists. The worker bumps the value by one
-- when inserting the next failure row.
selectMaxRetryCountStmt
  :: Stmt.Statement (PoolHashId, PoolMetadataRefId) (Maybe Word64)
selectMaxRetryCountStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = mconcat
      [ "SELECT MAX(", col offChainPoolFetchErrorCols.ocpfecRetryCount, ")"
      , " FROM ", table offChainPoolFetchErrorTableDef
      , " WHERE ", col offChainPoolFetchErrorCols.ocpfecPoolId, " = $1"
      , " AND ", col offChainPoolFetchErrorCols.ocpfecPmrId, " = $2"
      ]

    encoder =
      (fst >$< idEncoder getPoolHashId)
        <> (snd >$< idEncoder getPoolMetadataRefId)

    decoder = D.singleRow $
      D.column (D.nullable (fromIntegral <$> D.int8))
