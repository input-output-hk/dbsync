{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @ma_tx_out@ table (multi-asset
-- amounts attached to a transaction output).
module DbSync.Db.Statement.MaTxOut
  ( -- * Inserts
    insertMaTxOutRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.MultiAsset (MaTxOut, maTxOutEncoder, maTxOutTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName maTxOutTableDef

-- | Insert an 'MaTxOut' with a caller-chosen id.
insertMaTxOutRowStmt :: Stmt.Statement MaTxOut ()
insertMaTxOutRowStmt =
  Stmt.preparable sql maTxOutEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (quantity, tx_out_id, ident) VALUES ($1, $2, $3)"
      ]
