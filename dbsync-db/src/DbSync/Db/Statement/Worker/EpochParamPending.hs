{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @epoch_param_pending@
-- workspace table. Three roles:
--
--   * 'insertEpochParamPendingStmt' — bulk insert by the consumer at
--     each epoch boundary. @ON CONFLICT (epoch_no) DO NOTHING@
--     makes a re-flush after a partial crash a no-op.
--   * 'applyPoolUpdateDepositStmt' / 'applyStakeRegistrationDepositStmt'
--     — Prep-phase UPDATEs that fill the ledger-derived deposit
--     columns on @pool_update@ and @stake_registration@.
--   * 'truncateEpochParamPendingStmt' — clears the table at the end
--     of Prep. Truncate (not DROP) so a future Follow → Ingest
--     re-entry finds the table intact.
module DbSync.Db.Statement.Worker.EpochParamPending
  ( -- * Bulk insert
    insertEpochParamPendingStmt

    -- * Backfill UPDATEs
  , applyPoolUpdateDepositStmt
  , applyStakeRegistrationDepositStmt

    -- * Cleanup
  , truncateEpochParamPendingStmt
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
import DbSync.Db.Schema.EpochParamPending
  ( EpochParamPendingCols (..)
  , epochParamPendingCols
  , epochParamPendingTableDef
  )
import DbSync.Db.Schema.Pool (PoolUpdateCols (..), poolUpdateCols, poolUpdateTableDef)
import DbSync.Db.Schema.StakeDelegation
  ( StakeRegistrationCols (..)
  , stakeRegistrationCols
  , stakeRegistrationTableDef
  )
import DbSync.Db.Sql.Refs (col, qcol, table)
import DbSync.Db.Statement.Common (arrayParam)
import DbSync.Db.Types (DbLovelace, dbLovelaceValueEncoder)

-- ---------------------------------------------------------------------------
-- * Bulk insert
-- ---------------------------------------------------------------------------

-- | Three parallel arrays, one per column. One round-trip regardless
-- of input size. @ON CONFLICT (epoch_no) DO NOTHING@ handles the
-- partial-crash re-flush case.
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
    sql = mconcat
      [ "INSERT INTO ", table epochParamPendingTableDef
      , " (", col epochParamPendingCols.eppcEpochNo
      , ", ", col epochParamPendingCols.eppcStakeKeyDeposit
      , ", ", col epochParamPendingCols.eppcPoolDeposit, ")"
      , " SELECT * FROM unnest($1, $2, $3)"
      , " ON CONFLICT (", col epochParamPendingCols.eppcEpochNo, ") DO NOTHING"
      ]

-- ---------------------------------------------------------------------------
-- * Backfill UPDATEs
-- ---------------------------------------------------------------------------

-- | Fill @pool_update.deposit@ for first-registration rows (smallest
-- @id@ per @hash_id@); only the first registration carries a deposit,
-- so subsequent re-registrations keep @deposit IS NULL@. Joins
-- @pool_update -> tx -> block -> epoch_param_pending@ so the
-- protocol-param value is read at the registration's epoch.
-- @AND pu.deposit IS NULL@ keeps it idempotent.
applyPoolUpdateDepositStmt :: Stmt.Statement () Int64
applyPoolUpdateDepositStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = mconcat
      [ "WITH first_regs AS ("
      , " SELECT ", col poolUpdateCols.pucHashId
      , ", MIN(", col poolUpdateCols.pucId, ") AS first_id"
      , " FROM ", table poolUpdateTableDef
      , " GROUP BY ", col poolUpdateCols.pucHashId
      , ")"
      , " UPDATE ", table poolUpdateTableDef, " pu"
      , " SET ", col poolUpdateCols.pucDeposit
      ,   " = ", qcol "epp" epochParamPendingCols.eppcPoolDeposit
      , " FROM first_regs fr, ", table txTableDef, " t, "
      ,   table blockTableDef, " b, "
      ,   table epochParamPendingTableDef, " epp"
      , " WHERE ", qcol "pu" poolUpdateCols.pucId, " = fr.first_id"
      , " AND ", qcol "pu" poolUpdateCols.pucRegisteredTxId
      ,   " = ", qcol "t" txCols.tcId
      , " AND ", qcol "t" txCols.tcBlockId
      ,   " = ", qcol "b" blockCols.bcId
      , " AND ", qcol "b" blockCols.bcEpochNo
      ,   " = ", qcol "epp" epochParamPendingCols.eppcEpochNo
      , " AND ", qcol "pu" poolUpdateCols.pucDeposit, " IS NULL"
      ]

-- | Fill @stake_registration.deposit@ for Shelley-Babbage rows whose
-- cert carries no inline value. Conway+ rows have it inline and are
-- skipped by @AND sr.deposit IS NULL@.
applyStakeRegistrationDepositStmt :: Stmt.Statement () Int64
applyStakeRegistrationDepositStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = mconcat
      [ "UPDATE ", table stakeRegistrationTableDef, " sr"
      , " SET ", col stakeRegistrationCols.srcDeposit
      ,   " = ", qcol "epp" epochParamPendingCols.eppcStakeKeyDeposit
      , " FROM ", table txTableDef, " t, "
      ,   table blockTableDef, " b, "
      ,   table epochParamPendingTableDef, " epp"
      , " WHERE ", qcol "sr" stakeRegistrationCols.srcTxId
      ,   " = ", qcol "t" txCols.tcId
      , " AND ", qcol "t" txCols.tcBlockId
      ,   " = ", qcol "b" blockCols.bcId
      , " AND ", qcol "b" blockCols.bcEpochNo
      ,   " = ", qcol "epp" epochParamPendingCols.eppcEpochNo
      , " AND ", qcol "sr" stakeRegistrationCols.srcDeposit, " IS NULL"
      ]

-- ---------------------------------------------------------------------------
-- * Cleanup
-- ---------------------------------------------------------------------------

truncateEpochParamPendingStmt :: Stmt.Statement () ()
truncateEpochParamPendingStmt =
  Stmt.preparable
    ("TRUNCATE TABLE " <> table epochParamPendingTableDef)
    E.noParams
    D.noResult
