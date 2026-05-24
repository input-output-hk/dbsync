{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @dbsync_sync_state@ singleton row.
--
-- The schema type 'SyncStateRow' and its encoder\/decoder live in
-- 'DbSync.Db.Schema.SyncState'. This module pairs them with the
-- hand-written SQL templates and exposes them as
-- 'Stmt.Statement' values.
module DbSync.Db.Statement.SyncState
  ( seedSyncStateStmt
  , readSyncStateStmt
  , writeSyncStateStmt
  , writeSyncStateSlotStmt
  , markSnapshotCompleteStmt
  , markSyncCompleteStmt
  , readPendingRollbackSlotStmt
  , writePendingRollbackSlotStmt
  , clearPendingRollbackSlotStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.SyncState
  ( SyncStateRow
  , syncStateRowDecoder
  , syncStateRowEncoder
  )

-- | Idempotent seed @INSERT@. Only @schema_version_applied@ and
-- @ledger_enabled@ come from the caller; all other columns pick up
-- their @DEFAULT@ values.
seedSyncStateStmt :: Stmt.Statement (Int32, Bool) ()
seedSyncStateStmt =
  Stmt.preparable sql encoder D.noResult
  where
    sql =
      "INSERT INTO dbsync_sync_state (schema_version_applied, ledger_enabled) \
      \VALUES ($1, $2) ON CONFLICT (id) DO NOTHING"
    encoder =
         (fst >$< E.param (E.nonNullable E.int4))
      <> (snd >$< E.param (E.nonNullable E.bool))

-- | Read the singleton row, or 'Nothing' if it has never been seeded.
-- 'syncStateRowDecoder' consumes every column in table order, so a
-- plain @SELECT *@ suffices.
readSyncStateStmt :: Stmt.Statement () (Maybe SyncStateRow)
readSyncStateStmt =
  Stmt.preparable
    "SELECT * FROM dbsync_sync_state WHERE id = 1"
    E.noParams
    (D.rowMaybe syncStateRowDecoder)

-- | Write the consumer-owned columns of the singleton row. Returns
-- the affected row count so callers can verify the row exists.
--
-- Placeholder order matches 'syncStateRowEncoder'. Does __not__
-- touch @last_snapshot_slot@ or @sync_complete@ — those columns are
-- owned by 'markSnapshotCompleteStmt' and 'markSyncCompleteStmt'.
writeSyncStateStmt :: Stmt.Statement SyncStateRow Int64
writeSyncStateStmt =
  Stmt.preparable sql syncStateRowEncoder D.rowsAffected
  where
    sql = T.concat
      [ "UPDATE dbsync_sync_state SET"
      , "  last_committed_slot             = $1"
      , ", last_committed_block_no         = $2"
      , ", last_committed_block_hash       = $3"
      , ", block_id_counter                = $4"
      , ", tx_id_counter                   = $5"
      , ", tx_out_id_counter               = $6"
      , ", tx_in_id_counter                = $7"
      , ", collateral_tx_in_id_counter     = $8"
      , ", reference_tx_in_id_counter      = $9"
      , ", tx_metadata_id_counter          = $10"
      , ", ma_tx_mint_id_counter           = $11"
      , ", ma_tx_out_id_counter            = $12"
      , ", slot_leader_id_counter          = $13"
      , ", address_id_counter              = $14"
      , ", stake_address_id_counter        = $15"
      , ", pool_hash_id_counter            = $16"
      , ", multi_asset_id_counter          = $17"
      , ", script_id_counter               = $18"
      , ", stake_registration_id_counter   = $19"
      , ", stake_deregistration_id_counter = $20"
      , ", delegation_id_counter           = $21"
      , ", withdrawal_id_counter           = $22"
      , ", pool_update_id_counter          = $23"
      , ", pool_metadata_ref_id_counter    = $24"
      , ", pool_owner_id_counter           = $25"
      , ", pool_retire_id_counter          = $26"
      , ", pool_relay_id_counter           = $27"
      , ", tx_cbor_id_counter              = $28"
      , ", epoch_sync_stats_id_counter     = $29"
      , ", ada_pots_id_counter             = $30"
      , ", collateral_tx_out_id_counter    = $31"
      , ", schema_version_applied          = $32"
      , ", ledger_enabled                  = $33"
      , ", updated_at                      = now()"
      , " WHERE id = 1"
      ]

-- | Advance only @last_committed_slot@, @last_committed_block_no@,
-- and @last_committed_block_hash@. Used by 'FollowingChainTip' inside
-- each per-block transaction — the counter columns aren't touched
-- because Follow allocates IDs through PG sequences via @nextval@
-- rather than IORef counters.
writeSyncStateSlotStmt :: Stmt.Statement (Word64, Word64, ByteString) Int64
writeSyncStateSlotStmt =
  Stmt.preparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "UPDATE dbsync_sync_state SET"
      , "  last_committed_slot       = $1"
      , ", last_committed_block_no   = $2"
      , ", last_committed_block_hash = $3"
      , ", updated_at                = now()"
      , " WHERE id = 1"
      ]
    encoder =
         ((\(s, _, _) -> fromIntegral s) >$< E.param (E.nonNullable E.int8))
      <> ((\(_, b, _) -> fromIntegral b) >$< E.param (E.nonNullable E.int8))
      <> ((\(_, _, h) -> h)              >$< E.param (E.nonNullable E.bytea))

-- | Record a successful ledger-snapshot write at the given slot.
-- Owned exclusively by the snapshot-writer thread.
markSnapshotCompleteStmt :: Stmt.Statement Word64 Int64
markSnapshotCompleteStmt =
  Stmt.preparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "UPDATE dbsync_sync_state SET"
      , "  last_snapshot_slot = $1"
      , ", updated_at         = now()"
      , " WHERE id = 1"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | Flip @sync_complete@ to true. Called once at the
-- 'IngestChainHistory' → 'FollowingChainTip' transition; subsequent
-- boots take the Follow-restart path.
markSyncCompleteStmt :: Stmt.Statement () Int64
markSyncCompleteStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.concat
      [ "UPDATE dbsync_sync_state SET"
      , "  sync_complete = true"
      , ", updated_at    = now()"
      , " WHERE id = 1"
      ]

-- | Read @pending_rollback_slot@. 'Nothing' means no rollback is
-- pending; @Just@ means the boot path should run a rollback to that
-- slot before normal resume.
readPendingRollbackSlotStmt :: Stmt.Statement () (Maybe Word64)
readPendingRollbackSlotStmt =
  Stmt.preparable
    "SELECT pending_rollback_slot FROM dbsync_sync_state WHERE id = 1"
    E.noParams
    (D.singleRow (fmap fromIntegral <$> D.column (D.nullable D.int8)))

-- | Persist a pending rollback target. Written by the ledger worker
-- on a deep rollback so the recovery survives the process restart.
-- Also written by the CLI @--rollback-to-slot@ path before the
-- cascade runs so a mid-rollback crash resumes cleanly.
writePendingRollbackSlotStmt :: Stmt.Statement Word64 Int64
writePendingRollbackSlotStmt =
  Stmt.preparable sql encoder D.rowsAffected
  where
    sql = T.concat
      [ "UPDATE dbsync_sync_state SET"
      , "  pending_rollback_slot = $1"
      , ", updated_at            = now()"
      , " WHERE id = 1"
      ]
    encoder = fromIntegral >$< E.param (E.nonNullable E.int8)

-- | Clear the pending-rollback marker. Called by the boot path after
-- the recovery rollback has completed and committed.
clearPendingRollbackSlotStmt :: Stmt.Statement () Int64
clearPendingRollbackSlotStmt =
  Stmt.preparable sql E.noParams D.rowsAffected
  where
    sql = T.concat
      [ "UPDATE dbsync_sync_state SET"
      , "  pending_rollback_slot = NULL"
      , ", updated_at            = now()"
      , " WHERE id = 1"
      ]
