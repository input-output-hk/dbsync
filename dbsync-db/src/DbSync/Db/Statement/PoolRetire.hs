{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @pool_retire@ table.
module DbSync.Db.Statement.PoolRetire
  ( -- * Inserts
    insertPoolRetireRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Pool (PoolRetire, poolRetireEncoder, poolRetireTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName poolRetireTableDef

insertPoolRetireRowStmt :: Stmt.Statement PoolRetire ()
insertPoolRetireRowStmt =
  Stmt.preparable sql poolRetireEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (hash_id, cert_index, announced_tx_id, retiring_epoch)"
      , " VALUES ($1, $2, $3, $4)"
      ]

