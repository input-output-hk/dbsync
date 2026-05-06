{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @delegation@ table.
module DbSync.Db.Statement.Delegation
  ( -- * Inserts
    insertDelegationRowStmt

    -- * ID allocation
  , nextDelegationIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (DelegationId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.StakeDelegation
  ( Delegation
  , delegationEncoder
  , delegationTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName delegationTableDef

insertDelegationRowStmt :: Stmt.Statement (DelegationId, Delegation) ()
insertDelegationRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getDelegationId)
           <> (snd >$< delegationEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, addr_id, cert_index, pool_hash_id, active_epoch_no, tx_id"
      , " , slot_no, redeemer_id)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
      ]

nextDelegationIdStmt :: Stmt.Statement () DelegationId
nextDelegationIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder DelegationId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
