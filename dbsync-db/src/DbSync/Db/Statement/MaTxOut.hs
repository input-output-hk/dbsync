{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @ma_tx_out@ table (multi-asset
-- amounts attached to a transaction output).
module DbSync.Db.Statement.MaTxOut
  ( -- * Inserts
    insertMaTxOutRowStmt

    -- * ID allocation
  , nextMaTxOutIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids (MaTxOutId (..), idDecoder, idEncoder)
import DbSync.Db.Schema.MultiAsset (MaTxOut, maTxOutEncoder, maTxOutTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName maTxOutTableDef

-- | Insert an 'MaTxOut' with a caller-chosen id.
insertMaTxOutRowStmt :: Stmt.Statement (MaTxOutId, MaTxOut) ()
insertMaTxOutRowStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fst >$< idEncoder getMaTxOutId)
           <> (snd >$< maTxOutEncoder)
    sql = T.concat
      [ "INSERT INTO ", table
      , " (id, quantity, tx_out_id, ident) VALUES ($1, $2, $3, $4)"
      ]

-- | Allocate a new id from the @ma_tx_out_id_seq@ sequence.
nextMaTxOutIdStmt :: Stmt.Statement () MaTxOutId
nextMaTxOutIdStmt =
  Stmt.preparable sql E.noParams (D.singleRow $ idDecoder MaTxOutId)
  where
    sql = "SELECT nextval('" <> table <> "_id_seq')"
