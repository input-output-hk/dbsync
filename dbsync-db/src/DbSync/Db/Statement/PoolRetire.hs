{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @pool_retire@ table.
module DbSync.Db.Statement.PoolRetire
  ( -- * Inserts
    insertPoolRetireRowStmt

    -- * ID allocation
  , nextPoolRetireIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (PoolRetireId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Pool (PoolRetire, poolRetireEncoder, poolRetireTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName poolRetireTableDef

insertPoolRetireRowStmt :: Stmt.Statement (PoolRetireId, PoolRetire) ()
insertPoolRetireRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getPoolRetireId)
           <> (snd >$< poolRetireEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, hash_id, cert_index, announced_tx_id, retiring_epoch)"
      , " VALUES ($1, $2, $3, $4, $5)"
      ]

nextPoolRetireIdStmt :: Stmt.Statement () PoolRetireId
nextPoolRetireIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder PoolRetireId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
