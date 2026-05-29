{-# LANGUAGE OverloadedStrings #-}

-- | hasql statements that populate \/ trim the @epoch_finalized@
-- table by direct SQL — no COPY pipeline involvement.
--
-- Three callers:
--
--   * 'backfillEpochFinalizedStmt' — end-of-Ingest one-shot fill.
--     Inserts every closed epoch in the @block@ table.
--   * 'appendEpochFinalizedStmt' — Follow boundary hook. Upserts a
--     single epoch using @ON CONFLICT (no) DO UPDATE@ so a
--     re-issued boundary is harmless.
--   * 'deleteEpochFinalizedFromEpochStmt' — Follow rollback. Drops
--     every row at-or-after the rollback target's epoch_no.
module DbSync.Db.Statement.EpochFinalized
  ( appendEpochFinalizedStmt
  , backfillEpochFinalizedStmt
  , deleteEpochFinalizedFromEpochStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

-- ---------------------------------------------------------------------------
-- * Statements
-- ---------------------------------------------------------------------------

-- | Insert a single epoch's aggregate. Caller passes the epoch_no;
-- @ON CONFLICT (no) DO UPDATE@ makes the call idempotent across
-- re-issued boundaries.
appendEpochFinalizedStmt :: Stmt.Statement Word64 ()
appendEpochFinalizedStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder = (fromIntegral :: Word64 -> Int64) >$< E.param (E.nonNullable E.int8)
    sql = T.unlines
      [ "INSERT INTO epoch_finalized"
      , "  (id, out_sum, fees, tx_count, blk_count, no, start_time, end_time)"
      , "SELECT"
      , "  (b.epoch_no::bigint + 1),"
      , "  COALESCE(SUM(tx.out_sum), 0)::numeric,"
      , "  COALESCE(SUM(tx.fee), 0),"
      , "  COUNT(tx.id)::bigint,"
      , "  COUNT(DISTINCT b.id)::bigint,"
      , "  b.epoch_no::bigint,"
      , "  MIN(b.time),"
      , "  MAX(b.time)"
      , "FROM block b"
      , "LEFT JOIN tx ON tx.block_id = b.id"
      , "WHERE b.epoch_no = $1"
      , "GROUP BY b.epoch_no"
      , "ON CONFLICT (no) DO UPDATE SET"
      , "  id         = EXCLUDED.id,"
      , "  out_sum    = EXCLUDED.out_sum,"
      , "  fees       = EXCLUDED.fees,"
      , "  tx_count   = EXCLUDED.tx_count,"
      , "  blk_count  = EXCLUDED.blk_count,"
      , "  start_time = EXCLUDED.start_time,"
      , "  end_time   = EXCLUDED.end_time"
      ]

-- | Bulk-insert every closed epoch — i.e. every epoch_no strictly
-- below the maximum in @block@. The current epoch is excluded so
-- @epoch_current@ remains its sole owner.
backfillEpochFinalizedStmt :: Stmt.Statement () ()
backfillEpochFinalizedStmt =
  Stmt.preparable sql E.noParams D.noResult
  where
    sql = T.unlines
      [ "INSERT INTO epoch_finalized"
      , "  (id, out_sum, fees, tx_count, blk_count, no, start_time, end_time)"
      , "SELECT"
      , "  (b.epoch_no::bigint + 1),"
      , "  COALESCE(SUM(tx.out_sum), 0)::numeric,"
      , "  COALESCE(SUM(tx.fee), 0),"
      , "  COUNT(tx.id)::bigint,"
      , "  COUNT(DISTINCT b.id)::bigint,"
      , "  b.epoch_no::bigint,"
      , "  MIN(b.time),"
      , "  MAX(b.time)"
      , "FROM block b"
      , "LEFT JOIN tx ON tx.block_id = b.id"
      , "WHERE b.epoch_no IS NOT NULL"
      , "  AND b.epoch_no > COALESCE((SELECT MAX(no) FROM epoch_finalized), -1)"
      , "  AND b.epoch_no < (SELECT MAX(epoch_no) FROM block WHERE epoch_no IS NOT NULL)"
      , "GROUP BY b.epoch_no"
      , "ON CONFLICT (no) DO NOTHING"
      ]

-- | @DELETE@ every finalised epoch at-or-after the rollback target.
-- @>=@ rather than @>@ — the rollback-target's epoch is re-derived
-- live from the post-rollback @block@ + @tx@ data via the
-- @epoch_current@ view, not kept as a stale finalised row.
deleteEpochFinalizedFromEpochStmt :: Stmt.Statement Word64 Int64
deleteEpochFinalizedFromEpochStmt =
  Stmt.preparable sql encoder D.rowsAffected
  where
    encoder = (fromIntegral :: Word64 -> Int64) >$< E.param (E.nonNullable E.int8)
    sql = "DELETE FROM epoch_finalized WHERE no >= $1"
