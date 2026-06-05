{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @pool_owner@ table.
module DbSync.Db.Statement.PoolOwner
  ( -- * Inserts
    insertPoolOwnerRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Pool (PoolOwner, poolOwnerEncoder, poolOwnerTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName poolOwnerTableDef

insertPoolOwnerRowStmt :: Stmt.Statement PoolOwner ()
insertPoolOwnerRowStmt =
  Stmt.preparable sql poolOwnerEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (addr_id, pool_update_id) VALUES ($1, $2)"
      ]

