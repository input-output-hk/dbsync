{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @stake_deregistration@ table.
module DbSync.Db.Statement.StakeDeregistration
  ( -- * Inserts
    insertStakeDeregistrationRowStmt

    -- * ID allocation
  , nextStakeDeregistrationIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (StakeDeregistrationId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.StakeDelegation
  ( StakeDeregistration
  , stakeDeregistrationEncoder
  , stakeDeregistrationTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName stakeDeregistrationTableDef

insertStakeDeregistrationRowStmt
  :: Stmt.Statement (StakeDeregistrationId, StakeDeregistration) ()
insertStakeDeregistrationRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getStakeDeregistrationId)
           <> (snd >$< stakeDeregistrationEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, addr_id, cert_index, epoch_no, tx_id, redeemer_id)"
      , " VALUES ($1, $2, $3, $4, $5, $6)"
      ]

nextStakeDeregistrationIdStmt :: Stmt.Statement () StakeDeregistrationId
nextStakeDeregistrationIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder StakeDeregistrationId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
