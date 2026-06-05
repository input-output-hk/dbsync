{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @stake_registration@ table.
module DbSync.Db.Statement.StakeRegistration
  ( -- * Inserts
    insertStakeRegistrationRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.StakeDelegation
  ( StakeRegistration
  , stakeRegistrationEncoder
  , stakeRegistrationTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName stakeRegistrationTableDef

insertStakeRegistrationRowStmt :: Stmt.Statement StakeRegistration ()
insertStakeRegistrationRowStmt =
  Stmt.preparable sql stakeRegistrationEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (addr_id, cert_index, epoch_no, tx_id, deposit)"
      , " VALUES ($1, $2, $3, $4, $5)"
      ]

