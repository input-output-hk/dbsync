{-# LANGUAGE OverloadedStrings #-}

-- | DELETEs behind @utxo.strategy: "prune"@: drop spent outputs so
-- the database keeps the UTxO set rather than every output ever made.
--
-- Both DELETEs take a @tx_out.consumed_by_tx_id@ watermark from
-- 'queryPruneWatermarkStmt'. Children go first: the
-- @ma_tx_out.tx_out_id@ FK has no @ON DELETE CASCADE@.
module DbSync.Db.Statement.Worker.Prune
  ( queryPruneWatermarkStmt
  , deleteConsumedMaTxOutStmt
  , deleteConsumedTxOutStmt
  , truncateCollateralTxOutStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Core (BlockCols (..), TxCols (..), blockCols, blockTableDef, txCols, txTableDef)
import DbSync.Db.Schema.Ids (TxId (..), idEncoder)
import DbSync.Db.Schema.MultiAsset (MaTxOutCols (..), maTxOutCols, maTxOutTableDef)
import DbSync.Db.Schema.UTxO
  ( TxOutCols (..)
  , collateralTxOutTableDef
  , txOutCols
  , txOutTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)

-- | Highest @tx.id@ in a block at least @$1@ blocks behind the tip.
-- Everything consumed at or below it is outside the rollback window
-- and safe to delete.
--
-- 'Nothing' when the chain is shorter than the offset, which leaves
-- the caller nothing to prune.
queryPruneWatermarkStmt :: Stmt.Statement Word64 (Maybe TxId)
queryPruneWatermarkStmt =
  Stmt.preparable sql encoder (D.singleRow (D.column (D.nullable (TxId <$> D.int8))))
  where
    encoder = E.param (E.nonNullable (fromIntegral >$< E.int8))
    sql = mconcat
      [ "SELECT MAX(", qcol (table txTableDef) txCols.tcId, ")"
      , " FROM ", table txTableDef
      , " INNER JOIN ", table blockTableDef
      , " ON ", qcol (table txTableDef) txCols.tcBlockId
      , " = ", qcol (table blockTableDef) blockCols.bcId
      , " WHERE ", qcol (table blockTableDef) blockCols.bcBlockNo
      , " <= (SELECT MAX(", col blockCols.bcBlockNo, ") FROM "
      , table blockTableDef, ") - $1"
      ]

-- | Assets on outputs about to be pruned. Runs before
-- 'deleteConsumedTxOutStmt'.
deleteConsumedMaTxOutStmt :: Stmt.Statement TxId Int64
deleteConsumedMaTxOutStmt =
  Stmt.preparable sql (idEncoder getTxId) D.rowsAffected
  where
    sql = mconcat
      [ "DELETE FROM ", table maTxOutTableDef
      , " WHERE ", col maTxOutCols.mtocTxOutId, " IN ("
      , "SELECT ", col txOutCols.tocId, " FROM ", table txOutTableDef
      , " WHERE ", col txOutCols.tocConsumedByTxId, " <= $1)"
      ]

deleteConsumedTxOutStmt :: Stmt.Statement TxId Int64
deleteConsumedTxOutStmt =
  Stmt.preparable sql (idEncoder getTxId) D.rowsAffected
  where
    sql = mconcat
      [ "DELETE FROM ", table txOutTableDef
      , " WHERE ", col txOutCols.tocConsumedByTxId, " <= $1"
      ]

-- | @collateral_tx_out@ holds the collateral return of /valid/ txs —
-- outputs the chain never created, since collateral is only taken
-- when phase-2 fails (the parser folds those into @tx_out@). They can
-- never be spent or appear in the UTxO set, so prune drops the lot.
truncateCollateralTxOutStmt :: Stmt.Statement () ()
truncateCollateralTxOutStmt =
  Stmt.preparable
    ("TRUNCATE TABLE " <> table collateralTxOutTableDef)
    E.noParams
    D.noResult
