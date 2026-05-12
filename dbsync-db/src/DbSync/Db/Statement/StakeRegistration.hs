{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @stake_registration@ table.
module DbSync.Db.Statement.StakeRegistration
  ( -- * Inserts
    insertStakeRegistrationRowStmt

    -- * ID allocation
  , nextStakeRegistrationIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (StakeRegistrationId (..), idEncoder)
import DbSync.Db.Schema.StakeDelegation
  ( StakeRegistration
  , stakeRegistrationEncoder
  , stakeRegistrationTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Common (nextIdStmt)

table :: Text
table = tdName stakeRegistrationTableDef

insertStakeRegistrationRowStmt
  :: Stmt.Statement (StakeRegistrationId, StakeRegistration) ()
insertStakeRegistrationRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getStakeRegistrationId)
           <> (snd >$< stakeRegistrationEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, addr_id, cert_index, epoch_no, tx_id, deposit)"
      , " VALUES ($1, $2, $3, $4, $5, $6)"
      ]

nextStakeRegistrationIdStmt :: Stmt.Statement () StakeRegistrationId
nextStakeRegistrationIdStmt = nextIdStmt stakeRegistrationTableDef StakeRegistrationId
