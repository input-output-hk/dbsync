{-# LANGUAGE OverloadedStrings #-}

-- | Post-load FK resolution for the three input tables.
--
-- During Ingest, the 'UtxoStore' resolves the bulk of inputs at COPY
-- time and the rows go in with @tx_out_id@ already populated. Inputs
-- that missed the cache land with @tx_out_id = NULL@; this module
-- rebuilds the input tables via @CREATE … LIKE@ + @INSERT … SELECT
-- LEFT JOIN@ + @DROP@ + @RENAME@ to fill those NULLs in one
-- sequential pass.
--
-- CTAS is preferred to a column UPDATE because:
--
--   * UPDATE rewrites the heap MVCC-style and churns every pre-built
--     B-tree index on the table; CTAS writes a fresh heap with no
--     indexes attached, then the schema-wide index pass builds them
--     once at the end.
--   * Sequential writes hit a multiple of the random-write rate.
--   * Orphan inputs (no matching @tx.hash@) are preserved as
--     @tx_out_id = NULL@ via the @LEFT JOIN@.
--
-- The @consumed_by_tx_id@ column on @tx_out@ stays on an UPDATE:
-- it touches a much smaller residual (only rows the
-- 'ConsumedByWorker' didn't write live) and the @tx_out@ table has
-- no indexes worth churning during Prep.
module DbSync.Db.Statement.Resolve
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

-- | CTAS script for @tx_in@. The five-column projection matches the
-- @tx_in@ schema (id, tx_in_id, tx_out_id, tx_out_index, tx_out_hash,
-- redeemer_id).
resolveTxInScript :: Text
resolveTxInScript =
  ctasScript "tx_in"
    [ "id"
    , "tx_in_id"
    , "tx_out_index"
    , "tx_out_hash"
    , "redeemer_id"
    ]

-- | CTAS for @collateral_tx_in@. Same shape without @redeemer_id@.
resolveCollateralTxInScript :: Text
resolveCollateralTxInScript =
  ctasScript "collateral_tx_in"
    [ "id"
    , "tx_in_id"
    , "tx_out_index"
    , "tx_out_hash"
    ]

-- | CTAS for @reference_tx_in@. Same shape as collateral.
resolveReferenceTxInScript :: Text
resolveReferenceTxInScript =
  ctasScript "reference_tx_in"
    [ "id"
    , "tx_in_id"
    , "tx_out_index"
    , "tx_out_hash"
    ]

-- | Walk all resolved inputs and stamp each producing
-- @tx_out.consumed_by_tx_id@ with the consuming tx id. Runs after
-- the three CTAS rebuilds so @tx_in.tx_out_id@ is fully populated.
--
-- The per-epoch background worker covers the bulk during Ingest
-- (cache-hit inputs); this statement fills the residual on
-- cache-miss inputs.
resolveConsumedByTxIdStmt :: Stmt.Statement () Int64
resolveConsumedByTxIdStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.unwords
      [ "UPDATE tx_out"
      , "SET consumed_by_tx_id = tx_in.tx_in_id"
      , "FROM tx_in"
      , "WHERE tx_in.tx_out_id = tx_out.tx_id"
      , "AND tx_in.tx_out_index = tx_out.index"
      , "AND tx_out.consumed_by_tx_id IS NULL"
      ]

-- ---------------------------------------------------------------------------
-- * Internals
-- ---------------------------------------------------------------------------

-- | Build a CTAS script for one of the input tables.
--
-- @passthrough@ is every column except @tx_out_id@. The @tx_out_id@
-- column is resolved via @COALESCE(orig.tx_out_id, tx.id)@: cache
-- hits keep their pre-populated value, misses get the join result,
-- orphan inputs stay NULL.
ctasScript :: Text -> [Text] -> Text
ctasScript table passthrough = T.unlines
  [ "CREATE UNLOGGED TABLE " <> newName <> " (LIKE " <> table <> " INCLUDING DEFAULTS);"
  , "INSERT INTO " <> newName <> " (" <> T.intercalate ", " allCols <> ")"
  , "SELECT " <> T.intercalate ", " selExprs
  , "  FROM " <> table <> " src"
  , "  LEFT JOIN tx ON tx.hash = src.tx_out_hash;"
  , "DROP TABLE " <> table <> ";"
  , "ALTER TABLE " <> newName <> " RENAME TO " <> table <> ";"
  ]
  where
    newName = table <> "_new"
    insertIdx = 2  -- "id", "tx_in_id", THEN tx_out_id
    (before, after) = splitAt insertIdx passthrough
    allCols = before ++ ["tx_out_id"] ++ after
    selExprs =
      map (\c -> "src." <> c) before
        ++ ["COALESCE(src.tx_out_id, tx.id) AS tx_out_id"]
        ++ map (\c -> "src." <> c) after
