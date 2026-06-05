{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @pool_relay@ table.
module DbSync.Db.Statement.PoolRelay
  ( -- * Inserts
    insertPoolRelayRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Pool (PoolRelay, poolRelayEncoder, poolRelayTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName poolRelayTableDef

insertPoolRelayRowStmt :: Stmt.Statement PoolRelay ()
insertPoolRelayRowStmt =
  Stmt.preparable sql poolRelayEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (update_id, ipv4, ipv6, dns_name, dns_srv_name, port)"
      , " VALUES ($1, $2, $3, $4, $5, $6)"
      ]

