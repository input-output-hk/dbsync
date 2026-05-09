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
-- Statements that depend on resolved inputs use a @WITH@ CTE to
-- aggregate before the @UPDATE@, so the join column on @tx_out@ is
-- only scanned once.
module DbSync.Db.Statement.Backfill
  ( backfillPhaseTwoFeeStmt
  , backfillPhaseTwoDepositStmt
  , backfillValidContractDepositStmt
  , backfillByronFeeStmt
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

-- | Replace the @0@ fee sentinel on phase-2 failed Alonzo+ txs
-- with @SUM(collateral_in.value) - SUM(collateral_out.value)@.
-- Requires 'resolveCollateralTxInStmt' to have populated
-- @collateral_tx_in.tx_out_id@.
backfillPhaseTwoFeeStmt :: Stmt.Statement () Int64
backfillPhaseTwoFeeStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.unwords
      [ "WITH coll_in_sum AS ("
      , "  SELECT cti.tx_in_id AS tx_id, SUM(producing.value) AS total"
      , "  FROM collateral_tx_in cti"
      , "  JOIN tx_out producing"
      , "    ON producing.tx_id = cti.tx_out_id"
      , "   AND producing.index = cti.tx_out_index"
      , "  GROUP BY cti.tx_in_id"
      , "), coll_out_sum AS ("
      , "  SELECT tx_id, SUM(value) AS total"
      , "  FROM collateral_tx_out"
      , "  GROUP BY tx_id"
      , ")"
      , "UPDATE tx"
      , "SET fee = COALESCE(i.total, 0) - COALESCE(o.total, 0)"
      , "FROM coll_in_sum i"
      , "LEFT JOIN coll_out_sum o ON o.tx_id = i.tx_id"
      , "WHERE tx.id = i.tx_id"
      , "  AND tx.valid_contract = FALSE"
      , "  AND tx.fee = 0"
      ]

-- | Stamp @0@ on phase-2 failed txs whose @deposit@ is still NULL.
-- Independent of any FK resolution; safe to run at any time after
-- ingest.
backfillPhaseTwoDepositStmt :: Stmt.Statement () Int64
backfillPhaseTwoDepositStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.unwords
      [ "UPDATE tx"
      , "SET deposit = 0"
      , "WHERE valid_contract = FALSE"
      , "  AND deposit IS NULL"
      ]

-- | Compute @deposit = inputs + withdrawals - outputs - fee -
-- treasury_donation@ for valid-contract txs whose @deposit@ is
-- still NULL. This is the ledger-disabled fallback; ledger-enabled
-- runs receive deposits from the worker before this UPDATE sees
-- the row.
--
-- Requires 'resolveTxInStmt' to have populated @tx_in.tx_out_id@
-- so input values can be looked up via @tx_out@.
backfillValidContractDepositStmt :: Stmt.Statement () Int64
backfillValidContractDepositStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.unwords
      [ "WITH in_sum AS ("
      , "  SELECT ti.tx_in_id AS tx_id, SUM(producing.value) AS total"
      , "  FROM tx_in ti"
      , "  JOIN tx_out producing"
      , "    ON producing.tx_id = ti.tx_out_id"
      , "   AND producing.index = ti.tx_out_index"
      , "  GROUP BY ti.tx_in_id"
      , "), withdraw_sum AS ("
      , "  SELECT tx_id, SUM(amount) AS total"
      , "  FROM withdrawal"
      , "  GROUP BY tx_id"
      , ")"
      , "UPDATE tx"
      , "SET deposit ="
      , "  COALESCE(i.total, 0) + COALESCE(w.total, 0)"
      , "  - tx.out_sum - tx.fee - tx.treasury_donation"
      , "FROM in_sum i"
      , "LEFT JOIN withdraw_sum w ON w.tx_id = i.tx_id"
      , "WHERE tx.id = i.tx_id"
      , "  AND tx.valid_contract = TRUE"
      , "  AND tx.deposit IS NULL"
      ]

-- | Compute @fee = inputs - outputs@ for Byron-era txs whose @fee@
-- is still the parser's @0@ sentinel. Byron is identified via
-- @block.proto_major < 2@.
--
-- Genesis-era txs (the initial UTxO setup) are not extracted by the
-- ingest pipeline at all, so they never appear here. A regular
-- Byron tx whose inputs cannot be resolved (orphan, edge case) is
-- naturally excluded by the inner @JOIN@ on the @in_sum@ CTE.
--
-- Requires 'resolveTxInStmt' to have populated @tx_in.tx_out_id@.
backfillByronFeeStmt :: Stmt.Statement () Int64
backfillByronFeeStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.unwords
      [ "WITH in_sum AS ("
      , "  SELECT ti.tx_in_id AS tx_id, SUM(producing.value) AS total"
      , "  FROM tx_in ti"
      , "  JOIN tx_out producing"
      , "    ON producing.tx_id = ti.tx_out_id"
      , "   AND producing.index = ti.tx_out_index"
      , "  GROUP BY ti.tx_in_id"
      , ")"
      , "UPDATE tx"
      , "SET fee = COALESCE(i.total, 0) - tx.out_sum"
      , "FROM in_sum i, block b"
      , "WHERE tx.id = i.tx_id"
      , "  AND b.id = tx.block_id"
      , "  AND b.proto_major < 2"
      , "  AND tx.fee = 0"
      ]
