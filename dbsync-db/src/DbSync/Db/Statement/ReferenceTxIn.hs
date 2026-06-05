{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @reference_tx_in@ table.
--
-- Reference inputs (Babbage+) point to outputs read but not consumed by
-- the spending transaction (e.g. for inline scripts and reference
-- scripts). Schema-wise identical to @collateral_tx_in@.
module DbSync.Db.Statement.ReferenceTxIn
  ( -- * Inserts
    insertReferenceTxInRowStmt

  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( ReferenceTxIn
  , referenceTxInEncoder
  , referenceTxInTableDef
  )

table :: Text
table = tdName referenceTxInTableDef

-- | Insert a 'ReferenceTxIn' with a caller-chosen id.
insertReferenceTxInRowStmt :: Stmt.Statement ReferenceTxIn ()
insertReferenceTxInRowStmt =
  Stmt.preparable sql referenceTxInEncoder D.noResult
  where
    sql = T.concat
      [ "INSERT INTO ", table
      , " (tx_in_id, tx_out_id, tx_out_index, tx_out_hash)"
      , " VALUES ($1, $2, $3, $4)"
      ]
