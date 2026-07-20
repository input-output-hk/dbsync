{-# LANGUAGE OverloadedStrings #-}

-- | Post-load backfill of four @tx@ columns the ingest parser cannot
-- fill from the body alone:
--
--   * @fee@ on phase-2 failed Alonzo+ txs — body fee is meaningless;
--     real fee is @collateral_inputs - collateral_return@. Parser
--     writes @0@ sentinel.
--   * @fee@ on Byron txs — no explicit fee field; real fee is
--     @inputs - outputs@. Parser writes @0@ sentinel.
--   * @deposit@ on phase-2 failed txs — always @0@. Parser leaves NULL.
--   * @deposit@ on valid-contract txs in ledger-disabled mode —
--     @inputs + withdrawals - outputs - fee - treasury_donation@.
--     Parser leaves NULL until 'DbSync.Db.Statement.Worker.Resolve'
--     populates @tx_in.tx_out_id@.
--
-- The @fee@ UPDATEs drive off the small set of rows that need
-- patching and use per-row subqueries. The valid-contract @deposit@
-- UPDATE uses an aggregate-then-join because every valid tx needs
-- the computation in ledger-disabled mode — bulk-scan locality wins
-- at that scale.
--
-- All identifiers go through 'DbSync.Db.Sql.Refs' so a 'TableDef'
-- rename surfaces at module-load time, not as silently-wrong SQL.
module DbSync.Db.Statement.Worker.Backfill
  ( -- * Prepared 'Stmt.Statement' values
    backfillPhaseTwoFeeStmt
  , backfillPhaseTwoDepositStmt
  , backfillValidContractDepositStmt
  , backfillByronFeeStmt
    -- * Raw SQL strings
    --
    -- Exported so tests can feed them to @EXPLAIN@ and assert on the
    -- plan shape. A bad plan (e.g. a Nested Loop around a large
    -- aggregate) doesn't surface as a functional failure on small
    -- fixtures — it surfaces as a stall on a real chain. Plan-shape
    -- assertions catch that regardless of fixture size.
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
import DbSync.Db.Schema.Pool
  ( PoolRetireCols (..)
  , PoolUpdateCols (..)
  , poolRetireCols
  , poolRetireTableDef
  , poolUpdateCols
  , poolUpdateTableDef
  )
import DbSync.Db.Schema.StakeDelegation
  ( StakeDeregistrationCols (..)
  , StakeRegistrationCols (..)
  , WithdrawalCols (..)
  , stakeDeregistrationCols
  , stakeDeregistrationTableDef
  , stakeRegistrationCols
  , stakeRegistrationTableDef
  , withdrawalCols
  , withdrawalTableDef
  )
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

-- | Compute @deposit = inputs + withdrawals - outputs - fee -
-- treasury_donation@ for valid-contract activity txs whose @deposit@
-- is still NULL. Requires 'resolveTxInStmt' to have populated
-- @tx_in.tx_out_id@.
backfillValidContractDepositStmt :: Stmt.Statement () Int64
backfillValidContractDepositStmt =
  Stmt.preparable backfillValidContractDepositSql E.noParams D.rowsAffected

-- | @affected_txs@ sources tx ids from every table whose presence
-- implies a deposit-affecting certificate. Extend the UNION when a
-- new such table lands (drep / committee registrations).
backfillValidContractDepositSql :: Text
backfillValidContractDepositSql = T.unwords
  [ "WITH affected_txs AS ("
  , "  SELECT", col stakeRegistrationCols.srcTxId
  , "  FROM",   table stakeRegistrationTableDef
  , "  UNION ALL SELECT", col stakeDeregistrationCols.sdcTxId
  , "  FROM",   table stakeDeregistrationTableDef
  , "  UNION ALL SELECT", col poolUpdateCols.pucRegisteredTxId
  , "  FROM",   table poolUpdateTableDef
  , "  UNION ALL SELECT", col poolRetireCols.prcAnnouncedTxId
  , "  FROM",   table poolRetireTableDef
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
