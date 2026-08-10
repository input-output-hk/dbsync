{-# LANGUAGE OverloadedStrings #-}

-- | Post-load backfill of the @tx.fee@ and @tx.deposit@ columns that the
-- ingest parser cannot fill from the transaction body alone.
--
-- The @fee@ UPDATEs touch few rows, so they use per-row subqueries. The
-- valid-contract @deposit@ UPDATE aggregates and then joins, because in
-- ledger-disabled mode every valid tx needs the computation.
module DbSync.Db.Statement.Worker.Backfill
  ( -- * Prepared 'Stmt.Statement' values
    backfillPhaseTwoFeeStmt
  , backfillPhaseTwoDepositStmt
  , backfillValidContractDepositStmt
  , backfillByronFeeStmt
    -- * Deposit-affecting source tables
  , depositSourceTxIds
    -- * Raw SQL strings
    --
    -- Exported so tests can run @EXPLAIN@ and assert on the plan shape.
    -- A bad plan does not fail on small fixtures; it stalls on a real
    -- chain.
  , backfillPhaseTwoFeeSql
  , backfillPhaseTwoDepositSql
  , backfillValidContractDepositSql
  , backfillByronFeeSql
  ) where

import Cardano.Prelude

import qualified Data.Text as T
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
import DbSync.Db.Schema.Governance
  ( DrepRegistrationCols (..)
  , GovActionProposalCols (..)
  , drepRegistrationCols
  , govActionProposalCols
  )
import DbSync.Db.Schema.Pool
  ( PoolRetireCols (..)
  , PoolUpdateCols (..)
  , poolRetireCols
  , poolUpdateCols
  )
import DbSync.Db.Schema.StakeDelegation
  ( StakeDeregistrationCols (..)
  , StakeRegistrationCols (..)
  , WithdrawalCols (..)
  , stakeDeregistrationCols
  , stakeRegistrationCols
  , withdrawalCols
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types (TableColumn (..))
import DbSync.Db.Schema.UTxO
  ( TxInCols (..)
  , TxOutCols (..)
  , txInCols
  , txInTableDef
  , txOutCols
  , txOutTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)

-- | Patch the @0@ fee sentinel on phase-2 failed Alonzo+ txs that
-- carried no @total_collateral@ field. For a failed tx the parser
-- folds the collateral inputs into @tx_in@ and the collateral return
-- into @tx_out@, so the real fee is @SUM(input.value) - out_sum@.
-- Requires 'resolveTxInStmt' to have populated @tx_in.tx_out_id@. The
-- @EXISTS@ guard skips rows for which no input row was written (data
-- anomalies only); txs that set @total_collateral@ already have a
-- non-zero fee and are excluded by the @fee = 0@ guard.
backfillPhaseTwoFeeStmt :: Stmt.Statement () Int64
backfillPhaseTwoFeeStmt =
  Stmt.preparable backfillPhaseTwoFeeSql E.noParams D.rowsAffected

backfillPhaseTwoFeeSql :: Text
backfillPhaseTwoFeeSql = T.unwords
  [ "UPDATE", table txTableDef
  , "SET",    col txCols.tcFee, "= COALESCE("
  , "  (SELECT SUM(p.", col txOutCols.tocValue, ")"
  , "   FROM", table txInTableDef, "ti"
  , "   JOIN", table txOutTableDef, "p"
  , "     ON p.", col txOutCols.tocTxId
  ,        "=",    qcol "ti" txInCols.ticTxOutId
  , "    AND p.", col txOutCols.tocIndex
  ,        "=",    qcol "ti" txInCols.ticTxOutIndex
  , "   WHERE",   qcol "ti" txInCols.ticTxInId
  ,        "= tx.", col txCols.tcId, "),"
  , "  0)"
  , "- tx.", col txCols.tcOutSum
  , "WHERE", col txCols.tcValidContract, "= FALSE"
  , "  AND", col txCols.tcFee, "= 0"
  , "  AND EXISTS ("
  , "    SELECT 1 FROM", table txInTableDef, "ti"
  , "    WHERE", qcol "ti" txInCols.ticTxInId
  ,         "= tx.", col txCols.tcId, ")"
  ]

-- | Stamp @0@ on phase-2 failed txs whose @deposit@ is still NULL.
-- Independent of FK resolution.
backfillPhaseTwoDepositStmt :: Stmt.Statement () Int64
backfillPhaseTwoDepositStmt =
  Stmt.preparable backfillPhaseTwoDepositSql E.noParams D.rowsAffected

backfillPhaseTwoDepositSql :: Text
backfillPhaseTwoDepositSql = T.unwords
  [ "UPDATE", table txTableDef
  , "SET",    col txCols.tcDeposit, "= 0"
  , "WHERE",  col txCols.tcValidContract, "= FALSE"
  , "  AND",  col txCols.tcDeposit, "IS NULL"
  ]

-- | Every table whose presence on a tx implies a deposit-affecting
-- certificate or proposal, paired with its @tx_id@ column. Callers
-- filter this to the tables the enabled extractors actually created
-- before handing it to 'backfillValidContractDepositStmt'.
depositSourceTxIds :: [TableColumn]
depositSourceTxIds =
  [ stakeRegistrationCols.srcTxId
  , stakeDeregistrationCols.sdcTxId
  , poolUpdateCols.pucRegisteredTxId
  , poolRetireCols.prcAnnouncedTxId
  , govActionProposalCols.gapcTxId
  , drepRegistrationCols.drcTxId
  ]

-- | Compute @deposit = inputs + withdrawals - outputs - fee -
-- treasury_donation@ for valid-contract activity txs whose @deposit@
-- is still NULL. Requires 'resolveTxInStmt' to have populated
-- @tx_in.tx_out_id@.
backfillValidContractDepositStmt :: [TableColumn] -> Stmt.Statement () Int64
backfillValidContractDepositStmt sources =
  Stmt.preparable (backfillValidContractDepositSql sources) E.noParams D.rowsAffected

-- | @affected_txs@ unions the given @tx_id@ columns — normally
-- 'depositSourceTxIds' minus the tables absent from the schema.
-- An empty list renders invalid SQL, so callers skip the statement
-- instead of passing one.
backfillValidContractDepositSql :: [TableColumn] -> Text
backfillValidContractDepositSql sources = T.unwords
  [ "WITH affected_txs AS ("
  , T.intercalate " UNION ALL " (map sourceSelect sources)
  , "), targets AS ("
  , "  SELECT DISTINCT a.tx_id"
  , "  FROM affected_txs a"
  , "  JOIN", table txTableDef, "ON tx.", col txCols.tcId, "= a.tx_id"
  , "  WHERE tx.", col txCols.tcValidContract, "= TRUE"
  , "    AND tx.", col txCols.tcDeposit, "IS NULL"
  , "), in_sum AS ("
  , "  SELECT", qcol "ti" txInCols.ticTxInId, "AS tx_id,"
  , "         SUM(producing.", col txOutCols.tocValue, ") AS total"
  , "  FROM",  table txInTableDef, "ti"
  , "  JOIN",  table txOutTableDef, "producing"
  , "    ON producing.", col txOutCols.tocTxId
  ,        "=", qcol "ti" txInCols.ticTxOutId
  , "   AND producing.", col txOutCols.tocIndex
  ,        "=", qcol "ti" txInCols.ticTxOutIndex
  , "  WHERE", qcol "ti" txInCols.ticTxInId
  ,        "IN (SELECT tx_id FROM targets)"
  , "  GROUP BY", qcol "ti" txInCols.ticTxInId
  , "), withdraw_sum AS ("
  , "  SELECT", col withdrawalCols.wcTxId, ","
  , "         SUM(", col withdrawalCols.wcAmount, ") AS total"
  , "  FROM",  table withdrawalTableDef
  , "  WHERE", col withdrawalCols.wcTxId
  ,        "IN (SELECT tx_id FROM targets)"
  , "  GROUP BY", col withdrawalCols.wcTxId
  , ")"
  , "UPDATE", table txTableDef
  , "SET", col txCols.tcDeposit, "="
  , "  COALESCE(i.total, 0) + COALESCE(w.total, 0)"
  , "  - tx.", col txCols.tcOutSum
  , "  - tx.", col txCols.tcFee
  , "  - tx.", col txCols.tcTreasuryDonation
  , "FROM in_sum i"
  , "LEFT JOIN withdraw_sum w ON w.tx_id = i.tx_id"
  , "WHERE tx.", col txCols.tcId, "= i.tx_id"
  ]
  where
    -- Alias every branch so the union's output name holds whichever
    -- source table comes first.
    sourceSelect c = T.unwords
      ["SELECT", col c, "AS tx_id FROM", table c.tcTable]

-- | @fee = inputs - outputs@ for Byron-era txs whose @fee@ is still
-- the @0@ sentinel. Byron blocks are identified via @block.vrf_key IS
-- NULL@ (a Shelley+ header field): the final Byron epoch already
-- advertises @proto_major = 2@, so a @proto_major@ guard would strand
-- those last Byron txs at the @0@ sentinel.
-- Genesis txs aren't extracted; orphan-input rows are excluded by
-- the @EXISTS@ guard. Requires 'resolveTxInStmt' to have populated
-- @tx_in.tx_out_id@.
backfillByronFeeStmt :: Stmt.Statement () Int64
backfillByronFeeStmt =
  Stmt.preparable backfillByronFeeSql E.noParams D.rowsAffected

backfillByronFeeSql :: Text
backfillByronFeeSql = T.unwords
  [ "UPDATE", table txTableDef
  , "SET",    col txCols.tcFee, "= COALESCE("
  , "  (SELECT SUM(p.", col txOutCols.tocValue, ")"
  , "   FROM", table txInTableDef, "ti"
  , "   JOIN", table txOutTableDef, "p"
  , "     ON p.", col txOutCols.tocTxId
  ,        "=",    qcol "ti" txInCols.ticTxOutId
  , "    AND p.", col txOutCols.tocIndex
  ,        "=",    qcol "ti" txInCols.ticTxOutIndex
  , "   WHERE",   qcol "ti" txInCols.ticTxInId
  ,        "= tx.", col txCols.tcId, "),"
  , "  0)"
  , "- tx.", col txCols.tcOutSum
  , "FROM", table blockTableDef, "b"
  , "WHERE tx.", col txCols.tcBlockId
  ,         "= b.", col blockCols.bcId
  , "  AND b.", col blockCols.bcVrfKey, "IS NULL"
  , "  AND tx.", col txCols.tcFee, "= 0"
  , "  AND EXISTS ("
  , "    SELECT 1 FROM", table txInTableDef, "ti"
  , "    WHERE", qcol "ti" txInCols.ticTxInId
  ,         "= tx.", col txCols.tcId, ")"
  ]
