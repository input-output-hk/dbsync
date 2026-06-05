{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @tx_metadata@ table.
module DbSync.Db.Statement.TxMetadata
  ( -- * Inserts
    insertTxMetadataRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Metadata (TxMetadata, txMetadataEncoder, txMetadataTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName txMetadataTableDef

-- | Insert a 'TxMetadata' with a caller-chosen id.
insertTxMetadataRowStmt :: Stmt.Statement TxMetadata ()
insertTxMetadataRowStmt =
  Stmt.preparable sql txMetadataEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (key, json, bytes, tx_id) VALUES ($1, $2, $3, $4)"
      ]
