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
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (PoolHashId (..), idEncoder)
import DbSync.Db.Schema.Pool (PoolHash, poolHashEncoder, poolHashTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Common (LookupColumn (..), nextIdStmt, queryIdByColumnStmt)

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
nextPoolHashIdStmt = nextIdStmt poolHashTableDef PoolHashId

-- | Look up an existing 'PoolHashId' by 28-byte pool key hash.
queryPoolHashIdStmt :: Stmt.Statement ByteString (Maybe PoolHashId)
queryPoolHashIdStmt = queryIdByColumnStmt poolHashTableDef ByHashRaw PoolHashId
