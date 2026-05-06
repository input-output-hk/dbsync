{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @pool_update@ table.
module DbSync.Db.Statement.PoolUpdate
  ( -- * Inserts
    insertPoolUpdateRowStmt

    -- * ID allocation
  , nextPoolUpdateIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (PoolUpdateId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Pool (PoolUpdate, poolUpdateEncoder, poolUpdateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName poolUpdateTableDef

insertPoolUpdateRowStmt :: Stmt.Statement (PoolUpdateId, PoolUpdate) ()
insertPoolUpdateRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getPoolUpdateId)
           <> (snd >$< poolUpdateEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " ( id, hash_id, cert_index, vrf_key_hash, pledge, active_epoch_no"
      , " , meta_id, margin, fixed_cost, registered_tx_id, reward_addr_id"
      , " , deposit)"
      , " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)"
      ]

nextPoolUpdateIdStmt :: Stmt.Statement () PoolUpdateId
nextPoolUpdateIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder PoolUpdateId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
