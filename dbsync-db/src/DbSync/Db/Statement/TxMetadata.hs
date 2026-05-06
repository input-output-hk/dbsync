{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @tx_metadata@ table.
module DbSync.Db.Statement.TxMetadata
  ( -- * Inserts
    insertTxMetadataRowStmt

    -- * ID allocation
  , nextTxMetadataIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (TxMetadataId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.Metadata (TxMetadata, txMetadataEncoder, txMetadataTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName txMetadataTableDef

-- | Insert a 'TxMetadata' with a caller-chosen id.
insertTxMetadataRowStmt :: Stmt.Statement (TxMetadataId, TxMetadata) ()
insertTxMetadataRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getTxMetadataId)
           <> (snd >$< txMetadataEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, key, json, bytes, tx_id) VALUES ($1, $2, $3, $4, $5)"
      ]

-- | Allocate a new id from the @tx_metadata_id_seq@ sequence.
nextTxMetadataIdStmt :: Stmt.Statement () TxMetadataId
nextTxMetadataIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder TxMetadataId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
