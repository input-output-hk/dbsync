{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @delegation@ table.
module DbSync.Db.Statement.Delegation
  ( -- * Inserts
    insertDelegationRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.StakeDelegation
  ( Delegation
  , delegationEncoder
  , delegationTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName delegationTableDef

insertDelegationRowStmt :: Stmt.Statement Delegation ()
insertDelegationRowStmt =
  Stmt.preparable sql delegationEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (addr_id, cert_index, pool_hash_id, active_epoch_no, tx_id"
      , " , slot_no, redeemer_id)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7)"
      ]

