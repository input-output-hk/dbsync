{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @address@ dedup table.
--
-- 'FollowingChainTip' resolves an address by @raw@ via
-- 'queryAddressIdStmt'. On a miss it allocates a new id from
-- 'nextAddressIdStmt' and the writer inserts via
-- 'insertAddressRowStmt'.
--
-- 'IngestChainHistory' uses the bulk variants ('bulkSelectAddressIdsStmt',
-- 'bulkInsertAddressesStmt') from the background AddressResolver worker
-- to fold a whole epoch's address-resolution work into a constant
-- number of round-trips.
module DbSync.Db.Statement.Address
  ( -- * Inserts
    insertAddressRowStmt

    -- * ID allocation
  , nextAddressIdStmt

    -- * Lookups
  , queryAddressIdStmt

    -- * Bulk operations (used by IngestChainHistory)
  , BulkAddressInsert (..)
  , bulkSelectAddressIdsStmt
  , bulkInsertAddressesStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Address
  ( Address
  , addressEncoder
  , addressTableDef
  )
import DbSync.Db.Schema.Ids (AddressId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Common (LookupColumn (..), arrayParam, nextIdStmt, nullArrayParam, queryIdByColumnStmt)

table :: Text
table = tdName addressTableDef

insertAddressRowStmt :: Stmt.Statement (AddressId, Address) ()
insertAddressRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getAddressId)
           <> (snd >$< addressEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, address, raw, has_script, payment_cred, stake_address_id)"
      , " VALUES ($1, $2, $3, $4, $5, $6)"
      ]

nextAddressIdStmt :: Stmt.Statement () AddressId
nextAddressIdStmt = nextIdStmt addressTableDef AddressId

-- | Look up an existing 'AddressId' by raw address bytes.
queryAddressIdStmt :: Stmt.Statement ByteString (Maybe AddressId)
queryAddressIdStmt = queryIdByColumnStmt addressTableDef ByRaw AddressId

-- ---------------------------------------------------------------------------
-- * Bulk operations
-- ---------------------------------------------------------------------------

-- | Parallel-arrays payload for 'bulkInsertAddressesStmt'. Each list
-- holds one column's values and must be the same length as the others;
-- @baiIds[i]@, @baiAddresses[i]@, etc. together describe one row.
data BulkAddressInsert = BulkAddressInsert
  { baiIds            :: ![Int64]
  , baiAddresses      :: ![Text]
  , baiRaws           :: ![ByteString]
  , baiHasScript      :: ![Bool]
  , baiPaymentCreds   :: ![Maybe ByteString]
  , baiStakeAddressId :: ![Maybe Int64]
  }

-- | Look up many addresses at once. Returns @(raw, id)@ pairs for every
-- input raw that already exists in the @address@ table; missing raws
-- are simply absent from the result.
--
-- One round-trip regardless of input size.
bulkSelectAddressIdsStmt :: Stmt.Statement [ByteString] [(ByteString, AddressId)]
bulkSelectAddressIdsStmt =
  Stmt.preparable sql (arrayParam E.bytea) decoder
  where
    decoder = D.rowList $ (,)
      <$> D.column (D.nonNullable D.bytea)
      <*> idDecoder AddressId
    sql = "SELECT raw, id FROM " <> table <> " WHERE raw = ANY($1)"

-- | Bulk-insert addresses. Six parallel arrays, one per column.
--
-- The caller is responsible for pre-checking via
-- 'bulkSelectAddressIdsStmt' to avoid violating the @raw@ UNIQUE
-- constraint; this statement performs no @ON CONFLICT@ handling.
--
-- One round-trip regardless of input size.
bulkInsertAddressesStmt :: Stmt.Statement BulkAddressInsert ()
bulkInsertAddressesStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder =
         (baiIds            >$< arrayParam     E.int8)
      <> (baiAddresses      >$< arrayParam     E.text)
      <> (baiRaws           >$< arrayParam     E.bytea)
      <> (baiHasScript      >$< arrayParam     E.bool)
      <> (baiPaymentCreds   >$< nullArrayParam E.bytea)
      <> (baiStakeAddressId >$< nullArrayParam E.int8)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, address, raw, has_script, payment_cred, stake_address_id)"
      , " SELECT * FROM unnest($1, $2, $3, $4, $5, $6)"
      ]
