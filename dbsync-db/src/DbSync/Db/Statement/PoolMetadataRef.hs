{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @pool_metadata_ref@ table.
module DbSync.Db.Statement.PoolMetadataRef
  ( -- * Inserts
    insertPoolMetadataRefRowStmt

    -- * ID allocation
  , nextPoolMetadataRefIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (PoolMetadataRefId (..), idEncoder)
import DbSync.Db.Schema.Pool
  ( PoolMetadataRef
  , poolMetadataRefEncoder
  , poolMetadataRefTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Common (nextIdStmt)

table :: Text
table = tdName poolMetadataRefTableDef

insertPoolMetadataRefRowStmt :: Stmt.Statement (PoolMetadataRefId, PoolMetadataRef) ()
insertPoolMetadataRefRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getPoolMetadataRefId)
           <> (snd >$< poolMetadataRefEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, pool_id, url, hash, registered_tx_id) VALUES ($1, $2, $3, $4, $5)"
      ]

nextPoolMetadataRefIdStmt :: Stmt.Statement () PoolMetadataRefId
nextPoolMetadataRefIdStmt = nextIdStmt poolMetadataRefTableDef PoolMetadataRefId
