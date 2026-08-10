{-# LANGUAGE OverloadedStrings #-}

-- | Post-load FK resolution for the three input tables.
--
-- The 'UtxoStore' resolves inputs at COPY time; a cache miss lands with
-- @tx_out_id = NULL@. A CTAS rebuild fills those NULLs in one pass. CTAS
-- beats UPDATE here because PostgreSQL refuses to parallelise a plan that
-- modifies rows, and the @SELECT@ writes one sequential heap. CTAS drops
-- the constraints, so 'rebuildTableScript' re-attaches them from the
-- 'TableDef'.
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

-- | Stamp each producing @tx_out.consumed_by_tx_id@ with the consuming tx
-- id. Runs after the three CTAS rebuilds, which populate
-- @tx_in.tx_out_id@. The per-epoch ConsumedByWorker writes the bulk during
-- Ingest, so this statement only fills the cache-miss residual. That
-- residual is small enough to stay an UPDATE.
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

-- | The @tx.hash@ lookup sits in the second arm of the @COALESCE@, not in
-- a @LEFT JOIN@, so it runs only for the rows the 'UtxoStore' missed. An
-- input with no matching @tx.hash@ stays NULL either way.
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
