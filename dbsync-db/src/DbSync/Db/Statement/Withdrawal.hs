{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @withdrawal@ table.
module DbSync.Db.Statement.Withdrawal
  ( -- * Inserts
    insertWithdrawalRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.StakeDelegation
  ( Withdrawal
  , withdrawalEncoder
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName withdrawalTableDef

insertWithdrawalRowStmt :: Stmt.Statement Withdrawal ()
insertWithdrawalRowStmt =
  Stmt.preparable sql withdrawalEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (addr_id, tx_id, amount, redeemer_id)"
      , " VALUES ($1, $2, $3, $4)"
      ]

