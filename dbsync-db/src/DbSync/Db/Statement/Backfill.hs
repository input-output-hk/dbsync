{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the post-load tx-column backfill
-- pass.
--
-- The ingest parser cannot fill four @tx@ columns from the body
-- alone:
--
--   * @fee@ on a phase-2 failed Alonzo+ tx — the body's declared fee
--     is meaningless because the protocol charges collateral inputs
--     minus the optional collateral return instead. The parser
--     writes @0@ as a sentinel.
--   * @fee@ on a Byron tx — Byron has no explicit fee field; the
--     real fee is @inputs - outputs@. The Byron parser cannot
--     resolve input values, so it writes @0@ as a sentinel.
--   * @deposit@ on a phase-2 failed tx — always @0@ because the
--     deposit is forfeited along with all body effects. The parser
--     leaves it NULL; this module sets it to @0@.
--   * @deposit@ on a valid-contract tx in ledger-disabled mode —
--     computed from inputs, withdrawals, outputs, fee and treasury
--     donation. The parser leaves it NULL because it cannot resolve
--     input values until the FK pass has run; this module fills it
--     once 'DbSync.Db.Statement.Resolve' has populated
--     @tx_in.tx_out_id@.
--
-- The @fee@ UPDATEs drive off the (small) set of rows that need
-- patching and look up their inputs / collateral via per-row
-- subqueries. The @valid-contract deposit@ UPDATE retains the
-- aggregate-then-join shape because every valid tx needs the
-- computation in ledger-disabled mode — bulk-scan locality wins
-- over per-row random access at that scale.
--
-- All identifiers go through 'DbSync.Db.Sql.Refs' so a rename in
-- the corresponding 'TableDef' surfaces at module-load time instead
-- of silently producing wrong SQL.
module DbSync.Db.Statement.Backfill
  ( -- * Prepared 'Stmt.Statement' values
    backfillPhaseTwoFeeStmt
  , backfillPhaseTwoDepositStmt
  , backfillValidContractDepositStmt
  , backfillByronFeeStmt
    -- * Raw SQL strings
    --
    -- These are exposed so tests can feed them to @EXPLAIN@ and
    -- assert on the plan shape. Bad plans (Nested Loop with a
    -- one-row outer estimate around a 3M-row aggregate) don't
    -- surface as functional failures on small fixtures — they
    -- surface as a multi-hour hang on a real chain. The plan-shape
    -- assertions catch that class of regression regardless of
    -- fixture size.
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

import DbSync.Db.Schema.Core (blockTableDef, txTableDef)
import DbSync.Db.Schema.StakeDelegation (withdrawalTableDef)
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)

-- | Replace the @0@ fee sentinel on phase-2 failed Alonzo+ txs
-- with @SUM(collateral_in.value) - SUM(collateral_out.value)@.
-- Requires 'resolveCollateralTxInStmt' to have populated
-- @collateral_tx_in.tx_out_id@.
--
-- Drives off @tx WHERE valid_contract = FALSE AND fee = 0@ (hundreds
-- of rows on a typical chain), then looks up each phase-2 fail's
-- collateral inputs and return value via the pre-resolve indexes on
-- @collateral_tx_in (tx_in_id)@ and @collateral_tx_out (tx_id)@. The
-- @EXISTS@ guard mirrors the original CTE form's behaviour of not
-- touching rows for which no collateral input row was written
-- (data anomalies only — phase-2 fails always carry collateral on a
-- well-formed chain).
backfillPhaseTwoFeeStmt :: Stmt.Statement () Int64
backfillPhaseTwoFeeStmt =
  Stmt.preparable backfillPhaseTwoFeeSql E.noParams D.rowsAffected

-- | SQL string behind 'backfillPhaseTwoFeeStmt'.
backfillPhaseTwoFeeSql :: Text
backfillPhaseTwoFeeSql = T.unwords
  [ "UPDATE", table txTableDef
  , "SET",    col txTableDef "fee", "= COALESCE("
  , "  (SELECT SUM(p.", col txOutTableDef "value", ")"
  , "   FROM", table collateralTxInTableDef, "cti"
  , "   JOIN", table txOutTableDef, "p"
  , "     ON p.", col txOutTableDef "tx_id"
  ,        "=",    qcol "cti" collateralTxInTableDef "tx_out_id"
  , "    AND p.", col txOutTableDef "index"
  ,        "=",    qcol "cti" collateralTxInTableDef "tx_out_index"
  , "   WHERE",   qcol "cti" collateralTxInTableDef "tx_in_id"
  ,        "= tx.", col txTableDef "id", "),"
  , "  0)"
  , "- COALESCE("
  , "  (SELECT SUM(", col collateralTxOutTableDef "value", ")"
  , "   FROM",  table collateralTxOutTableDef
  , "   WHERE", col collateralTxOutTableDef "tx_id"
  ,        "= tx.", col txTableDef "id", "),"
  , "  0)"
  , "WHERE", col txTableDef "valid_contract", "= FALSE"
  , "  AND", col txTableDef "fee", "= 0"
  , "  AND EXISTS ("
  , "    SELECT 1 FROM", table collateralTxInTableDef, "cti"
  , "    WHERE", qcol "cti" collateralTxInTableDef "tx_in_id"
  ,         "= tx.", col txTableDef "id", ")"
  ]

-- | Stamp @0@ on phase-2 failed txs whose @deposit@ is still NULL.
-- Independent of any FK resolution; safe to run at any time after
-- ingest.
backfillPhaseTwoDepositStmt :: Stmt.Statement () Int64
backfillPhaseTwoDepositStmt =
  Stmt.preparable backfillPhaseTwoDepositSql E.noParams D.rowsAffected

-- | SQL string behind 'backfillPhaseTwoDepositStmt'.
backfillPhaseTwoDepositSql :: Text
backfillPhaseTwoDepositSql = T.unwords
  [ "UPDATE", table txTableDef
  , "SET",    col txTableDef "deposit", "= 0"
  , "WHERE",  col txTableDef "valid_contract", "= FALSE"
  , "  AND",  col txTableDef "deposit", "IS NULL"
  ]

-- | Compute @deposit = inputs + withdrawals - outputs - fee -
-- treasury_donation@ for valid-contract txs whose @deposit@ is
-- still NULL. This is the ledger-disabled fallback; ledger-enabled
-- runs receive deposits from the worker before this UPDATE sees
-- the row.
--
-- Requires 'resolveTxInStmt' to have populated @tx_in.tx_out_id@
-- so input values can be looked up via @tx_out@.
--
-- Aggregate-then-join shape is retained on purpose: in
-- ledger-disabled mode every valid tx needs the calc, so the
-- target set is the whole table. Bulk-scan locality beats per-row
-- random I/O at that scale. The planner needs accurate statistics
-- for @tx@ / @tx_in@ / @tx_out@ / @withdrawal@ to choose Hash Join
-- here; 'Phase.Preparing.Run.run' runs an explicit ANALYZE
-- between resolve and backfill so the stats reflect the post-resolve
-- cardinalities rather than what autovacuum last saw mid-ingest.
backfillValidContractDepositStmt :: Stmt.Statement () Int64
backfillValidContractDepositStmt =
  Stmt.preparable backfillValidContractDepositSql E.noParams D.rowsAffected

-- | SQL string behind 'backfillValidContractDepositStmt'.
backfillValidContractDepositSql :: Text
backfillValidContractDepositSql = T.unwords
  [ "WITH in_sum AS ("
  , "  SELECT", qcol "ti" txInTableDef "tx_in_id", "AS tx_id,"
  , "         SUM(producing.", col txOutTableDef "value", ") AS total"
  , "  FROM",  table txInTableDef, "ti"
  , "  JOIN",  table txOutTableDef, "producing"
  , "    ON producing.", col txOutTableDef "tx_id"
  ,        "=", qcol "ti" txInTableDef "tx_out_id"
  , "   AND producing.", col txOutTableDef "index"
  ,        "=", qcol "ti" txInTableDef "tx_out_index"
  , "  GROUP BY", qcol "ti" txInTableDef "tx_in_id"
  , "), withdraw_sum AS ("
  , "  SELECT", col withdrawalTableDef "tx_id", ","
  , "         SUM(", col withdrawalTableDef "amount", ") AS total"
  , "  FROM",  table withdrawalTableDef
  , "  GROUP BY", col withdrawalTableDef "tx_id"
  , ")"
  , "UPDATE", table txTableDef
  , "SET", col txTableDef "deposit", "="
  , "  COALESCE(i.total, 0) + COALESCE(w.total, 0)"
  , "  - tx.", col txTableDef "out_sum"
  , "  - tx.", col txTableDef "fee"
  , "  - tx.", col txTableDef "treasury_donation"
  , "FROM in_sum i"
  , "LEFT JOIN withdraw_sum w ON w.tx_id = i.tx_id"
  , "WHERE tx.", col txTableDef "id", "= i.tx_id"
  , "  AND tx.", col txTableDef "valid_contract", "= TRUE"
  , "  AND tx.", col txTableDef "deposit", "IS NULL"
  ]

-- | Compute @fee = inputs - outputs@ for Byron-era txs whose @fee@
-- is still the parser's @0@ sentinel. Byron is identified via
-- @block.proto_major < 2@.
--
-- Genesis-era txs (the initial UTxO setup) are not extracted by the
-- ingest pipeline at all, so they never appear here. A regular
-- Byron tx whose inputs cannot be resolved (orphan, edge case) is
-- excluded by the @EXISTS@ guard on @tx_in@.
--
-- Drives off the small @tx WHERE block.proto_major < 2 AND fee = 0@
-- set and looks up each tx's input sum via the pre-resolve
-- @tx_in (tx_in_id)@ index. Requires 'resolveTxInStmt' to have
-- populated @tx_in.tx_out_id@.
backfillByronFeeStmt :: Stmt.Statement () Int64
backfillByronFeeStmt =
  Stmt.preparable backfillByronFeeSql E.noParams D.rowsAffected

-- | SQL string behind 'backfillByronFeeStmt'.
backfillByronFeeSql :: Text
backfillByronFeeSql = T.unwords
  [ "UPDATE", table txTableDef
  , "SET",    col txTableDef "fee", "= COALESCE("
  , "  (SELECT SUM(p.", col txOutTableDef "value", ")"
  , "   FROM", table txInTableDef, "ti"
  , "   JOIN", table txOutTableDef, "p"
  , "     ON p.", col txOutTableDef "tx_id"
  ,        "=",    qcol "ti" txInTableDef "tx_out_id"
  , "    AND p.", col txOutTableDef "index"
  ,        "=",    qcol "ti" txInTableDef "tx_out_index"
  , "   WHERE",   qcol "ti" txInTableDef "tx_in_id"
  ,        "= tx.", col txTableDef "id", "),"
  , "  0)"
  , "- tx.", col txTableDef "out_sum"
  , "FROM", table blockTableDef, "b"
  , "WHERE tx.", col txTableDef "block_id"
  ,         "= b.", col blockTableDef "id"
  , "  AND b.", col blockTableDef "proto_major", "< 2"
  , "  AND tx.", col txTableDef "fee", "= 0"
  , "  AND EXISTS ("
  , "    SELECT 1 FROM", table txInTableDef, "ti"
  , "    WHERE", qcol "ti" txInTableDef "tx_in_id"
  ,         "= tx.", col txTableDef "id", ")"
  ]
