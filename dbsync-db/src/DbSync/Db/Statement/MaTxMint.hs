{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @ma_tx_mint@ table (per-tx
-- minting / burning events).
module DbSync.Db.Statement.MaTxMint
  ( -- * Inserts
    insertMaTxMintRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.MultiAsset (MaTxMint, maTxMintEncoder, maTxMintTableDef)
import DbSync.Db.Schema.Types (TableDef (..))

table :: Text
table = tdName maTxMintTableDef

-- | Insert an 'MaTxMint' with a caller-chosen id.
insertMaTxMintRowStmt :: Stmt.Statement MaTxMint ()
insertMaTxMintRowStmt =
  Stmt.preparable sql maTxMintEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (quantity, tx_id, ident) VALUES ($1, $2, $3)"
      ]
