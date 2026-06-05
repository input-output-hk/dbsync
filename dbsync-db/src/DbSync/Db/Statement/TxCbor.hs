{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @tx_cbor@ table.
module DbSync.Db.Statement.TxCbor
  ( -- * Inserts
    insertTxCborRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.CBOR (TxCbor, txCborEncoder, txCborTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName txCborTableDef

insertTxCborRowStmt :: Stmt.Statement TxCbor ()
insertTxCborRowStmt =
  Stmt.preparable sql txCborEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (tx_id, bytes) VALUES ($1, $2)"
      ]

