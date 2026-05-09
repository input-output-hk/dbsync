{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the post-load FK resolution pass.
--
-- The ingest path COPYs into UNLOGGED tables with the @tx_out_id@
-- columns left NULL on the four input tables, plus
-- @tx_out.consumed_by_tx_id@ left NULL on every output. Each row
-- carries the spent transaction's hash and output index instead;
-- these statements join back through @tx@ to populate the FK
-- columns once all data is on disk.
--
-- Each statement is a single bulk @UPDATE … FROM …@ that returns the
-- number of rows it touched. PG decides the join strategy; on a
-- mainnet-sized DB it will be a hash join against @tx.hash@.
module DbSync.Db.Statement.Resolve
  ( resolveTxInStmt
  , resolveCollateralTxInStmt
  , resolveReferenceTxInStmt
  , resolveConsumedByTxIdStmt
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Sql (quoteIdent)

-- | Match @tx_in.tx_out_hash@ against @tx.hash@ to populate
-- @tx_in.tx_out_id@. Skips rows already resolved.
resolveTxInStmt :: Stmt.Statement () Int64
resolveTxInStmt = resolveInputTableStmt "tx_in"

-- | Same shape as 'resolveTxInStmt' for the collateral input table.
resolveCollateralTxInStmt :: Stmt.Statement () Int64
resolveCollateralTxInStmt = resolveInputTableStmt "collateral_tx_in"

-- | Same shape as 'resolveTxInStmt' for the reference input table.
resolveReferenceTxInStmt :: Stmt.Statement () Int64
resolveReferenceTxInStmt = resolveInputTableStmt "reference_tx_in"

-- | Walk the resolved inputs and stamp each producing
-- @tx_out.consumed_by_tx_id@ with the consuming tx. Run after the
-- three input-side updates so @tx_in.tx_out_id@ is populated.
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

-- | Shared shape for the three input tables: walk the table, JOIN
-- @tx@ on hash equality, copy @tx.id@ into @tx_out_id@.
resolveInputTableStmt :: Text -> Stmt.Statement () Int64
resolveInputTableStmt table =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    qt = quoteIdent table
    sql = T.unwords
      [ "UPDATE", qt
      , "SET tx_out_id = tx.id"
      , "FROM tx"
      , "WHERE tx.hash =", qt <> ".tx_out_hash"
      , "AND", qt <> ".tx_out_id IS NULL"
      ]
