{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @stake_deregistration@ table.
module DbSync.Db.Statement.StakeDeregistration
  ( -- * Inserts
    insertStakeDeregistrationRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.StakeDelegation
  ( StakeDeregistration
  , stakeDeregistrationEncoder
  , stakeDeregistrationTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName stakeDeregistrationTableDef

insertStakeDeregistrationRowStmt :: Stmt.Statement StakeDeregistration ()
insertStakeDeregistrationRowStmt =
  Stmt.preparable sql stakeDeregistrationEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (addr_id, cert_index, epoch_no, tx_id, redeemer_id)"
      , " VALUES ($1, $2, $3, $4, $5)"
      ]

