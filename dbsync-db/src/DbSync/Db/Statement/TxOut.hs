{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @tx_out@ table.
--
-- Used during 'FollowingChainTip' (per-output INSERT and id allocation).
-- The 'IngestChainHistory' phase writes via COPY instead.
module DbSync.Db.Statement.TxOut
  ( -- * Inserts
    insertTxOutRowStmt

    -- * Updates
  , updateTxOutAddressIdStmt
  , bulkUpdateTxOutAddressIdsStmt

    -- * ID allocation
  , nextTxOutIdStmt

    -- * Lookups
  , queryOutputValueStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (AddressId (..), TxOutId (..), idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (TxOut, txOutEncoder, txOutTableDef)
import DbSync.Db.Statement.Common (arrayParam, nextIdStmt)
import DbSync.Db.Types (DbLovelace, dbLovelaceValueDecoder)

table :: Text
table = tdName txOutTableDef

-- | Insert a 'TxOut' with a caller-chosen id. Used by
-- 'FollowingChainTip' after the resolver allocates the id via
-- 'nextTxOutIdStmt'.
insertTxOutRowStmt :: Stmt.Statement (TxOutId, TxOut) ()
insertTxOutRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getTxOutId)
           <> (snd >$< txOutEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( id, tx_id, index, address_id, stake_address_id"
      , " , value, data_hash, inline_datum_id"
      , " , reference_script_id, consumed_by_tx_id)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"
      ]

-- | Fill in @tx_out.address_id@ for an existing row.
updateTxOutAddressIdStmt :: Stmt.Statement (AddressId, TxOutId) ()
updateTxOutAddressIdStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getAddressId)
           <> (snd >$< idEncoder getTxOutId)
    sql = "UPDATE " <> table <> " SET address_id = $1 WHERE id = $2"

-- | Bulk-update @tx_out.address_id@. Two parallel arrays: tx_out ids
-- and the address id to assign to each. One round-trip regardless of
-- input size; used by the IngestChainHistory address-resolver worker
-- to fold an epoch's worth of FK fills into one statement.
bulkUpdateTxOutAddressIdsStmt :: Stmt.Statement ([Int64], [Int64]) ()
bulkUpdateTxOutAddressIdsStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< arrayParam E.int8)    -- tx_out ids
           <> (snd >$< arrayParam E.int8)    -- address ids
    sql = T.concat
      [ "UPDATE ", table, " SET address_id = u.aid"
      , " FROM unnest($1, $2) AS u(tx_out_id, aid)"
      , " WHERE ", table, ".id = u.tx_out_id"
      ]

-- | Allocate a new id from the @tx_out_id_seq@ sequence.
nextTxOutIdStmt :: Stmt.Statement () TxOutId
nextTxOutIdStmt = nextIdStmt txOutTableDef TxOutId

-- | Look up a 'tx_out.value' by the producing tx's hash and the
-- output index. 'Nothing' when no such output exists in the DB.
queryOutputValueStmt :: Stmt.Statement (ByteString, Word16) (Maybe DbLovelace)
queryOutputValueStmt =
  Stmt.preparable sql encoder (D.rowMaybe valueDecoder)
  where
    encoder = (fst >$< E.param (E.nonNullable E.bytea))
           <> (snd >$< E.param (E.nonNullable (fromIntegral >$< E.int8)))
    valueDecoder = D.column (D.nonNullable dbLovelaceValueDecoder)
    sql = T.concat
      [ "SELECT tx_out.value FROM tx_out"
      , " JOIN tx ON tx.id = tx_out.tx_id"
      , " WHERE tx.hash = $1 AND tx_out.index = $2"
      ]
