{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @tx_cbor@ table.
module DbSync.Db.Statement.TxCbor
  ( -- * Inserts
    insertTxCborRowStmt

    -- * ID allocation
  , nextTxCborIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.CBOR (TxCbor, txCborEncoder, txCborTableDef)
import DbSync.Db.Schema.Ids (TxCborId (..), idEncoder)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Common (nextIdStmt)

table :: Text
table = tdName txCborTableDef

insertTxCborRowStmt :: Stmt.Statement (TxCborId, TxCbor) ()
insertTxCborRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getTxCborId)
           <> (snd >$< txCborEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, tx_id, bytes) VALUES ($1, $2, $3)"
      ]

nextTxCborIdStmt :: Stmt.Statement () TxCborId
nextTxCborIdStmt = nextIdStmt txCborTableDef TxCborId
