{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @tx@ table.
module DbSync.Db.Statement.Tx
  ( -- * Inserts
    insertTxStmt
  , insertTxRowStmt

    -- * ID allocation
  , nextTxIdStmt

    -- * Lookups
  , queryTxIdByHashStmt
  , queryTxCountStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Core (Tx, txEncoder, txTableDef)
import DbSync.Db.Schema.Ids (TxId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName txTableDef

-- | Insert a 'Tx', let the DB pick an id, return it.
insertTxStmt :: Stmt.Statement Tx TxId
insertTxStmt =
  Stmt.preparable sql txEncoder (D.singleRow $ idDecoder TxId)
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( hash, block_id, block_index, out_sum, fee, deposit, size"
      , " , invalid_before, invalid_hereafter, valid_contract, script_size"
      , " , treasury_donation)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)"
      , " RETURNING id"
      ]

-- | Insert a 'Tx' with a caller-chosen id.
insertTxRowStmt :: Stmt.Statement (TxId, Tx) ()
insertTxRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getTxId)
           <> (snd >$< txEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( id, hash, block_id, block_index, out_sum, fee, deposit, size"
      , " , invalid_before, invalid_hereafter, valid_contract, script_size"
      , " , treasury_donation)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)"
      ]

-- | Allocate a new id from the @tx_id_seq@ sequence.
nextTxIdStmt :: Stmt.Statement () TxId
nextTxIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder TxId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"

-- | Look up a tx id by its hash.
queryTxIdByHashStmt :: Stmt.Statement ByteString (Maybe TxId)
queryTxIdByHashStmt =
  Stmt.preparable sql
    (E.param (E.nonNullable E.bytea))
    (D.rowMaybe (idDecoder TxId))
  where
    sql = "SELECT id FROM " <> table <> " WHERE hash = $1"

-- | Count rows in @tx@.
queryTxCountStmt :: Stmt.Statement () Int64
queryTxCountStmt =
  Stmt.preparable sql E.noParams
    (D.singleRow (D.column (D.nonNullable D.int8)))
  where
    sql = "SELECT COUNT(*) FROM " <> table
