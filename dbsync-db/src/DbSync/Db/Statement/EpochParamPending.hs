{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @epoch_param_pending@ table.
--
-- Three roles:
--
--   * 'insertEpochParamPendingStmt' — bulk INSERT used by the
--     consumer at each epoch boundary. @ON CONFLICT (epoch_no)
--     DO NOTHING@ makes a re-flush after partial crash a no-op.
--   * 'applyPoolUpdateDepositStmt' / 'applyStakeRegistrationDepositStmt'
--     — UPDATE pairs run in 'PreparingForVolatileTail' to fill the
--     ledger-derived deposit columns.
--   * 'truncateEpochParamPendingStmt' — clears the table at the end
--     of 'PreparingForVolatileTail' once the backfills have run. We
--     truncate rather than DROP so a future Follow → Ingest re-entry
--     finds the table intact.
module DbSync.Db.Statement.EpochParamPending
  ( -- * Bulk insert (called by the consumer)
    insertEpochParamPendingStmt

    -- * Backfill UPDATEs (called by PreparingForVolatileTail)
  , applyPoolUpdateDepositStmt
  , applyStakeRegistrationDepositStmt

    -- * Cleanup
  , truncateEpochParamPendingStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.EpochParamPending (epochParamPendingTableName)
import DbSync.Db.Statement.Common (arrayParam)
import DbSync.Db.Types (DbLovelace, dbLovelaceValueEncoder)

-- ---------------------------------------------------------------------------
-- * Bulk insert
-- ---------------------------------------------------------------------------

-- | Bulk-insert epoch-param rows. Three parallel arrays, one per
-- column. One round-trip regardless of input size.
--
-- @ON CONFLICT (epoch_no) DO NOTHING@: a partial crash may flush
-- the same epoch twice on resume. The conflict clause makes the
-- second flush a no-op rather than an error.
insertEpochParamPendingStmt
  :: Stmt.Statement ([Word64], [DbLovelace], [DbLovelace]) ()
insertEpochParamPendingStmt =
  Stmt.preparable sql encoder D.noResult
  where
    encoder =
         ((\(es, _, _) -> map (fromIntegral :: Word64 -> Int64) es)
            >$< arrayParam E.int8)
      <> ((\(_, ss, _) -> ss) >$< arrayParam dbLovelaceValueEncoder)
      <> ((\(_, _, ps) -> ps) >$< arrayParam dbLovelaceValueEncoder)
    sql = T.concat
      [ "INSERT INTO ", epochParamPendingTableName
      , " (epoch_no, stake_key_deposit, pool_deposit)"
      , " SELECT * FROM unnest($1, $2, $3)"
      , " ON CONFLICT (epoch_no) DO NOTHING"
      ]

-- ---------------------------------------------------------------------------
-- * Backfill UPDATEs
-- ---------------------------------------------------------------------------

-- | Fill @pool_update.deposit@ for first-registration rows from the
-- per-epoch protocol-param table. A pool's first registration is
-- the row with the smallest @id@ for a given @hash_id@; subsequent
-- re-registrations keep @deposit IS NULL@ to match the original
-- schema's contract.
--
-- The join goes @pool_update -> tx -> block -> epoch_param_pending@
-- so the protocol-param value is read at the epoch the registration
-- was applied. @AND pu.deposit IS NULL@ guarantees idempotency.
applyPoolUpdateDepositStmt :: Stmt.Statement () Int64
applyPoolUpdateDepositStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.unwords
      [ "WITH first_regs AS ("
      , "  SELECT hash_id, MIN(id) AS first_id"
      , "  FROM pool_update"
      , "  GROUP BY hash_id"
      , ")"
      , "UPDATE pool_update pu"
      , "SET deposit = epp.pool_deposit"
      , "FROM first_regs fr, tx t, block b, epoch_param_pending epp"
      , "WHERE pu.id = fr.first_id"
      , "  AND pu.registered_tx_id = t.id"
      , "  AND t.block_id = b.id"
      , "  AND b.epoch_no = epp.epoch_no"
      , "  AND pu.deposit IS NULL"
      ]

-- | Fill @stake_registration.deposit@ for rows whose cert carries
-- no inline value (Shelley-Babbage). Conway+ rows have the deposit
-- inline and are skipped by @AND sr.deposit IS NULL@.
applyStakeRegistrationDepositStmt :: Stmt.Statement () Int64
applyStakeRegistrationDepositStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.unwords
      [ "UPDATE stake_registration sr"
      , "SET deposit = epp.stake_key_deposit"
      , "FROM tx t, block b, epoch_param_pending epp"
      , "WHERE sr.tx_id = t.id"
      , "  AND t.block_id = b.id"
      , "  AND b.epoch_no = epp.epoch_no"
      , "  AND sr.deposit IS NULL"
      ]

-- ---------------------------------------------------------------------------
-- * Cleanup
-- ---------------------------------------------------------------------------

-- | @TRUNCATE epoch_param_pending@. Run at the end of
-- 'PreparingForVolatileTail' after the two apply UPDATEs. The table
-- stays in the schema so a future Follow → Ingest re-entry can
-- reuse it without re-running 'initSchema'.
truncateEpochParamPendingStmt :: Stmt.Statement () ()
truncateEpochParamPendingStmt =
  Stmt.preparable
    ("TRUNCATE TABLE " <> epochParamPendingTableName)
    E.noParams
    D.noResult
