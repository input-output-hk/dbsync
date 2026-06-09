{-# LANGUAGE OverloadedStrings #-}

-- | Post-load FK resolution for the three input tables.
--
-- During Ingest the 'UtxoStore' resolves most inputs at COPY time;
-- cache-misses land with @tx_out_id = NULL@. This module rebuilds
-- those tables via @CREATE … LIKE@ + @INSERT … SELECT LEFT JOIN@ +
-- @DROP@ + @RENAME@ to fill the NULLs in one sequential pass.
--
-- CTAS is preferred over UPDATE because:
--
--   * UPDATE rewrites the heap MVCC-style and churns every pre-built
--     B-tree index; CTAS writes a fresh heap and the schema-wide
--     index pass builds them once at the end.
--   * Sequential writes hit a multiple of the random-write rate.
--   * @LEFT JOIN@ preserves orphan inputs (no matching @tx.hash@) as
--     @tx_out_id = NULL@.
--
-- @tx_out.consumed_by_tx_id@ stays on an UPDATE: it touches a much
-- smaller residual (only what 'ConsumedByWorker' didn't write live)
-- and @tx_out@ has no indexes worth churning during Prep.
module DbSync.Db.Statement.Worker.Resolve
  ( -- * SQL scripts
    resolveTxInScript
  , resolveCollateralTxInScript
  , resolveReferenceTxInScript

    -- * Single-statement UPDATEs
  , resolveConsumedByTxIdStmt
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Schema.UTxO
  ( CollateralTxInCols (..)
  , ReferenceTxInCols (..)
  , TxInCols (..)
  , TxOutCols (..)
  , collateralTxInCols
  , collateralTxInTableDef
  , referenceTxInCols
  , referenceTxInTableDef
  , txInCols
  , txInTableDef
  , txOutCols
  , txOutTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)

resolveTxInScript :: Text
resolveTxInScript =
  ctasScript (tdName txInTableDef)
    [ txInCols.ticId.tcName
    , txInCols.ticTxInId.tcName
    , txInCols.ticTxOutIndex.tcName
    , txInCols.ticTxOutHash.tcName
    , txInCols.ticRedeemerId.tcName
    ]

-- | Same shape as 'resolveTxInScript' minus @redeemer_id@.
resolveCollateralTxInScript :: Text
resolveCollateralTxInScript =
  ctasScript (tdName collateralTxInTableDef)
    [ collateralTxInCols.cticId.tcName
    , collateralTxInCols.cticTxInId.tcName
    , collateralTxInCols.cticTxOutIndex.tcName
    , collateralTxInCols.cticTxOutHash.tcName
    ]

-- | Same shape as 'resolveCollateralTxInScript'.
resolveReferenceTxInScript :: Text
resolveReferenceTxInScript =
  ctasScript (tdName referenceTxInTableDef)
    [ referenceTxInCols.rticId.tcName
    , referenceTxInCols.rticTxInId.tcName
    , referenceTxInCols.rticTxOutIndex.tcName
    , referenceTxInCols.rticTxOutHash.tcName
    ]

-- | Walk all resolved inputs and stamp the producing
-- @tx_out.consumed_by_tx_id@ with the consuming tx id. Run after the
-- three CTAS rebuilds so @tx_in.tx_out_id@ is fully populated.
-- Per-epoch ConsumedByWorker covers the bulk during Ingest; this
-- statement fills the cache-miss residual.
resolveConsumedByTxIdStmt :: Stmt.Statement () Int64
resolveConsumedByTxIdStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.unwords
      [ "UPDATE", table txOutTableDef
      , "SET", col txOutCols.tocConsumedByTxId
      , "=", qcol (table txInTableDef) txInCols.ticTxInId
      , "FROM", table txInTableDef
      , "WHERE", qcol (table txInTableDef) txInCols.ticTxOutId
      , "=", qcol (table txOutTableDef) txOutCols.tocTxId
      , "AND", qcol (table txInTableDef) txInCols.ticTxOutIndex
      , "=", qcol (table txOutTableDef) txOutCols.tocIndex
      , "AND", qcol (table txOutTableDef) txOutCols.tocConsumedByTxId
      , "IS NULL"
      ]

-- ---------------------------------------------------------------------------
-- * Internals
-- ---------------------------------------------------------------------------

-- | @passthrough@ is every column except @tx_out_id@. @tx_out_id@ is
-- resolved via @COALESCE(orig.tx_out_id, tx.id)@: cache hits keep
-- their pre-populated value, misses get the join result, orphans
-- stay NULL.
ctasScript :: Text -> [Text] -> Text
ctasScript tbl passthrough = T.unlines
  [ "CREATE UNLOGGED TABLE " <> newName <> " (LIKE " <> tbl
      <> " INCLUDING DEFAULTS INCLUDING IDENTITY);"
  , "INSERT INTO " <> newName <> " (" <> T.intercalate ", " allCols <> ")"
  , "SELECT " <> T.intercalate ", " selExprs
  , "  FROM " <> tbl <> " src"
  , "  LEFT JOIN tx ON tx.hash = src.tx_out_hash;"
  , "DROP TABLE " <> tbl <> ";"
  , "ALTER TABLE " <> newName <> " RENAME TO " <> tbl <> ";"
  ]
  where
    newName = tbl <> "_new"
    insertIdx = 2  -- "id", "tx_in_id", THEN tx_out_id
    (before, after) = splitAt insertIdx passthrough
    allCols = before ++ ["tx_out_id"] ++ after
    selExprs =
      map (\c -> "src." <> c) before
        ++ ["COALESCE(src.tx_out_id, tx.id) AS tx_out_id"]
        ++ map (\c -> "src." <> c) after
