{-# LANGUAGE OverloadedStrings #-}

-- | Post-load FK resolution for the three input tables.
--
-- During Ingest the 'UtxoStore' resolves inputs at COPY time;
-- cache-misses land with @tx_out_id = NULL@. This module rebuilds
-- those tables via CTAS to fill the NULLs in one pass.
--
-- CTAS rather than UPDATE because UPDATE rewrites the heap MVCC-style
-- and PostgreSQL refuses to parallelise a plan that modifies rows; a
-- CTAS @SELECT@ is parallel-eligible and writes one sequential heap.
-- The price is that CTAS carries no constraints — 'rebuildTableScript'
-- re-attaches them from the 'TableDef'.
--
-- The @tx.hash@ lookup sits in the second arm of a @COALESCE@ rather
-- than a @LEFT JOIN@, so it is evaluated only for the rows the
-- 'UtxoStore' missed instead of once per input row. An input with no
-- matching @tx.hash@ at all stays NULL either way.
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

import DbSync.Db.Schema.Core (TxCols (..), txCols, txTableDef)
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
import DbSync.Db.Statement.Common (rebuildTableScript)

resolveTxInScript :: Text
resolveTxInScript =
  resolveScript txInTableDef txInCols.ticTxOutId txInCols.ticTxOutHash

resolveCollateralTxInScript :: Text
resolveCollateralTxInScript =
  resolveScript
    collateralTxInTableDef
    collateralTxInCols.cticTxOutId
    collateralTxInCols.cticTxOutHash

resolveReferenceTxInScript :: Text
resolveReferenceTxInScript =
  resolveScript
    referenceTxInTableDef
    referenceTxInCols.rticTxOutId
    referenceTxInCols.rticTxOutHash

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

resolveScript :: TableDef -> TableColumn -> TableColumn -> Text
resolveScript td txOutIdCol txOutHashCol =
  rebuildTableScript td [(tcName txOutIdCol, resolved)] (table td <> " src")
  where
    resolved = T.unwords
      [ "COALESCE(", qcol "src" txOutIdCol, ","
      , "(SELECT", qcol (table txTableDef) txCols.tcId
      , "FROM", table txTableDef
      , "WHERE", qcol (table txTableDef) txCols.tcHash
      ,      "=", qcol "src" txOutHashCol, "))"
      ]
