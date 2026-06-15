{-# LANGUAGE OverloadedStrings #-}

-- | Recompute-invariant checks against the live test database. Each asserts
-- that a stored, derived value equals a fresh recomputation from its source
-- tables, catching the data drift a schema fingerprint cannot (the
-- cardano-db-sync \#2118 class). Every action returns the number of violating
-- rows, so @0@ means the invariant holds.
module DbSync.Test.RecomputeInvariants
  ( epochFinalizedDriftCount
  , blockTxCountDriftCount
  , txOutSumDriftCount
  ) where

import Cardano.Prelude

import qualified Data.Text as T

import DbSync.Test.Database (queryTestDb)

-- ---------------------------------------------------------------------------
-- * Invariant checks
-- ---------------------------------------------------------------------------

-- | @epoch_finalized@ rows whose stored aggregates diverge from a fresh
-- @block@+@tx@ recomputation (the body of the @epoch_current@ view), or whose
-- synthesised @id@ is not @no + 1@.
epochFinalizedDriftCount :: IO Int
epochFinalizedDriftCount =
  countViolations $
    T.unwords
      [ "SELECT ef.no FROM epoch_finalized ef"
      , "JOIN ("
      , "  SELECT b.epoch_no AS no,"
      , "         COALESCE(SUM(tx.out_sum), 0)::numeric AS out_sum,"
      , "         COALESCE(SUM(tx.fee), 0) AS fees,"
      , "         COUNT(tx.id)::bigint AS tx_count,"
      , "         COUNT(DISTINCT b.id)::bigint AS blk_count,"
      , "         MIN(b.time) AS start_time,"
      , "         MAX(b.time) AS end_time"
      , "  FROM block b LEFT JOIN tx ON tx.block_id = b.id"
      , "  WHERE b.epoch_no IS NOT NULL"
      , "  GROUP BY b.epoch_no"
      , ") r ON r.no = ef.no"
      , "WHERE ef.out_sum <> r.out_sum"
      , "   OR ef.fees <> r.fees"
      , "   OR ef.tx_count <> r.tx_count"
      , "   OR ef.blk_count <> r.blk_count"
      , "   OR ef.start_time <> r.start_time"
      , "   OR ef.end_time <> r.end_time"
      , "   OR ef.id <> ef.no + 1"
      ]

-- | @block@ rows whose stored @tx_count@ differs from the actual tx count.
blockTxCountDriftCount :: IO Int
blockTxCountDriftCount =
  countViolations $
    T.unwords
      [ "SELECT b.id FROM block b"
      , "LEFT JOIN (SELECT block_id, COUNT(*) AS c FROM tx GROUP BY block_id) t"
      , "  ON t.block_id = b.id"
      , "WHERE b.tx_count <> COALESCE(t.c, 0)"
      ]

-- | Valid @tx@ rows whose stored @out_sum@ differs from the sum of their
-- outputs. Scoped to @valid_contract@ because phase-2-failed txs record
-- @collateral_tx_out@ rather than @tx_out@.
txOutSumDriftCount :: IO Int
txOutSumDriftCount =
  countViolations $
    T.unwords
      [ "SELECT t.id FROM tx t"
      , "LEFT JOIN (SELECT tx_id, SUM(value) AS s FROM tx_out GROUP BY tx_id) o"
      , "  ON o.tx_id = t.id"
      , "WHERE t.valid_contract = true"
      , "  AND t.out_sum <> COALESCE(o.s, 0)"
      ]

-- Count the rows a mismatch query returns; -1 if the count fails to parse.
countViolations :: Text -> IO Int
countViolations q = do
  t <- T.strip <$> queryTestDb ("SELECT count(*) FROM (" <> q <> ") AS v;")
  pure (fromMaybe (-1) (readMaybe (T.unpack t)))
