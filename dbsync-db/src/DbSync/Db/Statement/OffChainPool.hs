{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @off_chain_pools@ extractor:
-- result tables, SMASH-maintained tables, and the work-queue lookups
-- the worker uses to find pending or due-for-retry refs.
module DbSync.Db.Statement.OffChainPool
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
import qualified Data.Text as T
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
  , OffChainPoolFetchError
  , offChainPoolDataEncoder
  , offChainPoolDataTableDef
  , offChainPoolFetchErrorEncoder
  , offChainPoolFetchErrorTableDef
  )
import DbSync.Db.Schema.Pool
  ( DelistedPool
  , ReservedPoolTicker
  , delistedPoolEncoder
  , delistedPoolTableDef
  , reservedPoolTickerEncoder
  , reservedPoolTickerTableDef
  )
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
--
-- 'ppfPrevFetchTime' is 'Nothing' for refs never attempted and
-- 'Just t' (with matching retry count) for refs whose last attempt
-- failed and is due to retry.
data PendingPoolFetch = PendingPoolFetch
  { ppfPoolId         :: !PoolHashId
  , ppfPmrId          :: !PoolMetadataRefId
  , ppfUrl            :: !Text
  , ppfHash           :: !ByteString
  , ppfPrevFetchTime  :: !(Maybe UTCTime)
  , ppfPrevRetryCount :: !Word64
  }
  deriving stock (Eq, Show)

-- | Latest @pool_metadata_ref@ per pool that has neither a success
-- nor a recorded fetch error yet.
queryNewPendingPoolFetchesStmt :: Stmt.Statement Int32 [PendingPoolFetch]
queryNewPendingPoolFetchesStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = T.concat
      [ "WITH latest_refs AS ("
      , " SELECT MAX(id) AS max_id"
      , " FROM pool_metadata_ref"
      , " GROUP BY pool_id"
      , ")"
      , "SELECT ph.id, pmr.id, pmr.url, pmr.hash"
      , " FROM pool_hash ph"
      , " INNER JOIN pool_metadata_ref pmr ON ph.id = pmr.pool_id"
      , " WHERE pmr.id IN (SELECT max_id FROM latest_refs)"
      , "   AND NOT EXISTS ("
      , "     SELECT 1 FROM off_chain_pool_data pod"
      , "     WHERE pod.pmr_id = pmr.id"
      , "   )"
      , "   AND NOT EXISTS ("
      , "     SELECT 1 FROM off_chain_pool_fetch_error pofe"
      , "     WHERE pofe.pmr_id = pmr.id"
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
-- later success. Returns the prior fetch time and retry count so
-- the caller can apply the exponential-backoff schedule.
queryRetryPendingPoolFetchesStmt :: Stmt.Statement Int32 [PendingPoolFetch]
queryRetryPendingPoolFetchesStmt =
  Stmt.preparable sql encoder decoder
  where
    sql = T.concat
      [ "WITH latest_errors AS ("
      , " SELECT MAX(id) AS max_id"
      , " FROM off_chain_pool_fetch_error"
      , " WHERE NOT EXISTS ("
      , "   SELECT 1 FROM off_chain_pool_data pod"
      , "   WHERE pod.pmr_id = off_chain_pool_fetch_error.pmr_id"
      , " )"
      , " GROUP BY pool_id"
      , ")"
      , "SELECT ph.id, pmr.id, pmr.url, pmr.hash,"
      , "       pofe.fetch_time, pofe.retry_count"
      , " FROM pool_hash ph"
      , " INNER JOIN pool_metadata_ref pmr ON ph.id = pmr.pool_id"
      , " INNER JOIN off_chain_pool_fetch_error pofe ON pofe.pmr_id = pmr.id"
      , " WHERE pofe.id IN (SELECT max_id FROM latest_errors)"
      , " ORDER BY pofe.id ASC"
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

-- | The highest @retry_count@ already recorded for a
-- @(pool_id, pmr_id)@ pair, or 'Nothing' if no error row exists.
-- The worker bumps the returned value by one when inserting the
-- next failure row.
selectMaxRetryCountStmt
  :: Stmt.Statement (PoolHashId, PoolMetadataRefId) (Maybe Word64)
selectMaxRetryCountStmt =
  Stmt.preparable sql encoder decoder
  where
    sql =
      "SELECT MAX(retry_count) FROM off_chain_pool_fetch_error \
      \WHERE pool_id = $1 AND pmr_id = $2"

    encoder =
      (fst >$< idEncoder getPoolHashId)
        <> (snd >$< idEncoder getPoolMetadataRefId)

    decoder = D.singleRow $
      D.column (D.nullable (fromIntegral <$> D.int8))
