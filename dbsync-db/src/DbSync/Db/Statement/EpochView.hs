{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @epoch@ extractor: the
-- @epoch_finalized@ table and the @epoch@ / @epoch_current@ views.
--
-- The table is populated by direct SQL — no COPY pipeline:
--
--   * 'backfillEpochFinalizedStmt' — one-shot fill at end of Ingest.
--   * 'appendEpochFinalizedStmt' — Follow boundary upsert.
--
-- Rollback cleanup goes through the shared epoch-keyed delete in
-- 'DbSync.Phase.Following.Rollback'.
module DbSync.Db.Statement.EpochView
  ( appendEpochFinalizedStmt
  , backfillEpochFinalizedStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Core
  ( BlockCols (..)
  , TxCols (..)
  , blockCols
  , blockTableDef
  , txCols
  , txTableDef
  )
import DbSync.Db.Schema.EpochView
  ( EpochFinalizedCols (..)
  , epochFinalizedCols
  , epochFinalizedTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)

-- ---------------------------------------------------------------------------
-- * epoch_finalized
-- ---------------------------------------------------------------------------

-- | Single-epoch upsert. @ON CONFLICT (no) DO UPDATE@ makes a
-- re-issued boundary harmless.
appendEpochFinalizedStmt :: Stmt.Statement Word64 ()
appendEpochFinalizedStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fromIntegral :: Word64 -> Int64) >$< E.param (E.nonNullable E.int8)
    sql = mconcat
      [ "INSERT INTO ", table epochFinalizedTableDef
      , " (", colList, ")"
      , " ", selectClause
      , " WHERE ", qcol "b" blockCols.bcEpochNo, " = $1"
      , " GROUP BY ", qcol "b" blockCols.bcEpochNo
      , " ON CONFLICT (", col epochFinalizedCols.efcNo, ") DO UPDATE SET "
      ,    col epochFinalizedCols.efcId,        " = EXCLUDED.", col epochFinalizedCols.efcId
      , ", ", col epochFinalizedCols.efcOutSum, " = EXCLUDED.", col epochFinalizedCols.efcOutSum
      , ", ", col epochFinalizedCols.efcFees,   " = EXCLUDED.", col epochFinalizedCols.efcFees
      , ", ", col epochFinalizedCols.efcTxCount,   " = EXCLUDED.", col epochFinalizedCols.efcTxCount
      , ", ", col epochFinalizedCols.efcBlkCount,  " = EXCLUDED.", col epochFinalizedCols.efcBlkCount
      , ", ", col epochFinalizedCols.efcStartTime, " = EXCLUDED.", col epochFinalizedCols.efcStartTime
      , ", ", col epochFinalizedCols.efcEndTime,   " = EXCLUDED.", col epochFinalizedCols.efcEndTime
      ]

-- | Bulk-insert every closed epoch (strictly below the maximum
-- @epoch_no@ in @block@). The current epoch is excluded so
-- @epoch_current@ remains its sole owner.
backfillEpochFinalizedStmt :: Stmt.Statement () ()
backfillEpochFinalizedStmt =
  Stmt.preparable sql E.noParams D.noResult
  where
    sql = mconcat
      [ "INSERT INTO ", table epochFinalizedTableDef
      , " (", colList, ")"
      , " ", selectClause
      , " WHERE ", qcol "b" blockCols.bcEpochNo, " IS NOT NULL"
      , " AND ", qcol "b" blockCols.bcEpochNo
      ,   " > COALESCE((SELECT MAX(", col epochFinalizedCols.efcNo, ") FROM "
      ,   table epochFinalizedTableDef, "), -1)"
      , " AND ", qcol "b" blockCols.bcEpochNo
      ,   " < (SELECT MAX(", col blockCols.bcEpochNo, ") FROM "
      ,   table blockTableDef
      ,   " WHERE ", col blockCols.bcEpochNo, " IS NOT NULL)"
      , " GROUP BY ", qcol "b" blockCols.bcEpochNo
      , " ON CONFLICT (", col epochFinalizedCols.efcNo, ") DO NOTHING"
      ]

-- ---------------------------------------------------------------------------
-- * Shared SQL fragments
-- ---------------------------------------------------------------------------

colList :: Text
colList = mconcat
  [        col epochFinalizedCols.efcId
  , ", ", col epochFinalizedCols.efcOutSum
  , ", ", col epochFinalizedCols.efcFees
  , ", ", col epochFinalizedCols.efcTxCount
  , ", ", col epochFinalizedCols.efcBlkCount
  , ", ", col epochFinalizedCols.efcNo
  , ", ", col epochFinalizedCols.efcStartTime
  , ", ", col epochFinalizedCols.efcEndTime
  ]

-- | Aggregates one epoch's row from @block@ + @tx@.
selectClause :: Text
selectClause = mconcat
  [ "SELECT (", qcol "b" blockCols.bcEpochNo, "::bigint + 1)"
  , ", COALESCE(SUM(", qcol "tx" txCols.tcOutSum, "), 0)::numeric"
  , ", COALESCE(SUM(", qcol "tx" txCols.tcFee, "), 0)"
  , ", COUNT(", qcol "tx" txCols.tcId, ")::bigint"
  , ", COUNT(DISTINCT ", qcol "b" blockCols.bcId, ")::bigint"
  , ", ", qcol "b" blockCols.bcEpochNo, "::bigint"
  , ", MIN(", qcol "b" blockCols.bcTime, ")"
  , ", MAX(", qcol "b" blockCols.bcTime, ")"
  , " FROM ", table blockTableDef, " b"
  , " LEFT JOIN ", table txTableDef, " tx ON "
  ,    qcol "tx" txCols.tcBlockId, " = ", qcol "b" blockCols.bcId
  ]
