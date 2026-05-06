{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @pool_hash@ dedup table.
module DbSync.Db.Statement.PoolHash
  ( -- * Inserts
    insertPoolHashRowStmt

    -- * ID allocation
  , nextPoolHashIdStmt

    -- * Lookups
  , queryPoolHashIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (PoolHashId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Pool (PoolHash, poolHashEncoder, poolHashTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName poolHashTableDef

insertPoolHashRowStmt :: Stmt.Statement (PoolHashId, PoolHash) ()
insertPoolHashRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getPoolHashId)
           <> (snd >$< poolHashEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, hash_raw, view) VALUES ($1, $2, $3)"
      ]

nextPoolHashIdStmt :: Stmt.Statement () PoolHashId
nextPoolHashIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder PoolHashId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"

-- | Look up an existing 'PoolHashId' by 28-byte pool key hash.
queryPoolHashIdStmt :: Stmt.Statement ByteString (Maybe PoolHashId)
queryPoolHashIdStmt =
  Stmt.preparable sql
    (E.param (E.nonNullable E.bytea))
    (D.rowMaybe (idDecoder PoolHashId))
  where
    sql = "SELECT id FROM " <> table <> " WHERE hash_raw = $1"
