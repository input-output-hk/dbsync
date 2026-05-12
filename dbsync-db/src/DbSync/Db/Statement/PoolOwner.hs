{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @pool_owner@ table.
module DbSync.Db.Statement.PoolOwner
  ( -- * Inserts
    insertPoolOwnerRowStmt

    -- * ID allocation
  , nextPoolOwnerIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (PoolOwnerId (..), idEncoder)
import DbSync.Db.Schema.Pool (PoolOwner, poolOwnerEncoder, poolOwnerTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Common (nextIdStmt)

table :: Text
table = tdName poolOwnerTableDef

insertPoolOwnerRowStmt :: Stmt.Statement (PoolOwnerId, PoolOwner) ()
insertPoolOwnerRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getPoolOwnerId)
           <> (snd >$< poolOwnerEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, addr_id, pool_update_id) VALUES ($1, $2, $3)"
      ]

nextPoolOwnerIdStmt :: Stmt.Statement () PoolOwnerId
nextPoolOwnerIdStmt = nextIdStmt poolOwnerTableDef PoolOwnerId
