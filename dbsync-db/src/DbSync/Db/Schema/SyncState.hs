{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Db.Schema.SyncState
Description : Schema definition for the @dbsync_sync_state@ singleton metadata table.

The @dbsync_sync_state@ table is the one piece of state that lives in
PostgreSQL regardless of ledger mode. It records:

  * The last committed chain position — @last_committed_slot@,
    @last_committed_block_no@, @last_committed_block_hash@ (all nullable
    on a fresh DB).
  * The ID counter values — one column per 'DbSync.Id.Counter.IdCounter'
    field currently in use.
  * Metadata — @schema_version_applied@, @ledger_enabled@, @updated_at@.

Unlike the extractor tables (@block@, @tx@, …) this table is:

  * __LOGGED from day one__ — the whole point is crash durability;
    there is no second pass to promote it.
  * __Never a COPY target__ — updates go through 'UPSERT' on a dedicated
    control connection at each epoch boundary.
  * __Constrained__ — @CHECK (id = 1)@ enforces the single-row invariant,
    a @PRIMARY KEY (id)@ with a @DEFAULT 1@ on the @id@ column gives us
    a trivially-atomic 'UPSERT' target.
-}
module DbSync.Db.Schema.SyncState
  ( -- * Table metadata
    syncStateTableName
  , syncStateTableDef

    -- * Column name helpers (exported so DML stays consistent with DDL)
  , syncStateColumns
  , syncStateCounterColumns
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )

-- | Name of the sync-state table. A single-element constant used by
-- both DDL generation and runtime DML queries.
syncStateTableName :: Text
syncStateTableName = "dbsync_sync_state"

-- | 'TableDef' for the @dbsync_sync_state@ table.
--
-- Every counter column is @BIGINT NOT NULL DEFAULT 1@ so that a fresh
-- seed row auto-populates correctly from the 'DEFAULT' clauses. The
-- @last_committed_*@ columns are nullable and default to NULL — they
-- are populated on the first epoch 'commitEpoch'.
--
-- Column order here defines the golden ordering used by tests and by
-- the hand-written SELECT/UPDATE statements in 'DbSync.Ledger.SyncState';
-- changes must stay in sync across all three.
syncStateTableDef :: TableDef
syncStateTableDef = TableDef
  { tdName    = syncStateTableName
  , tdColumns =
      [ ColumnDef "id"                              PgSmallInt    False
      , ColumnDef "last_committed_slot"             PgBigInt      True
      , ColumnDef "last_committed_block_no"         PgBigInt      True
      , ColumnDef "last_committed_block_hash"       PgBytea       True
      , ColumnDef "block_id_counter"                PgBigInt      False
      , ColumnDef "tx_id_counter"                   PgBigInt      False
      , ColumnDef "tx_out_id_counter"               PgBigInt      False
      , ColumnDef "tx_in_id_counter"                PgBigInt      False
      , ColumnDef "collateral_tx_in_id_counter"     PgBigInt      False
      , ColumnDef "reference_tx_in_id_counter"      PgBigInt      False
      , ColumnDef "tx_metadata_id_counter"          PgBigInt      False
      , ColumnDef "ma_tx_mint_id_counter"           PgBigInt      False
      , ColumnDef "ma_tx_out_id_counter"            PgBigInt      False
      , ColumnDef "slot_leader_id_counter"          PgBigInt      False
      , ColumnDef "stake_address_id_counter"        PgBigInt      False
      , ColumnDef "pool_hash_id_counter"            PgBigInt      False
      , ColumnDef "multi_asset_id_counter"          PgBigInt      False
      , ColumnDef "script_id_counter"               PgBigInt      False
      , ColumnDef "stake_registration_id_counter"   PgBigInt      False
      , ColumnDef "stake_deregistration_id_counter" PgBigInt      False
      , ColumnDef "delegation_id_counter"           PgBigInt      False
      , ColumnDef "withdrawal_id_counter"           PgBigInt      False
      , ColumnDef "pool_update_id_counter"          PgBigInt      False
      , ColumnDef "pool_metadata_ref_id_counter"    PgBigInt      False
      , ColumnDef "pool_owner_id_counter"           PgBigInt      False
      , ColumnDef "pool_retire_id_counter"          PgBigInt      False
      , ColumnDef "pool_relay_id_counter"           PgBigInt      False
      , ColumnDef "tx_cbor_id_counter"              PgBigInt      False
      , ColumnDef "epoch_sync_stats_id_counter"     PgBigInt      False
      , ColumnDef "schema_version_applied"          PgInteger     False
      , ColumnDef "ledger_enabled"                  PgBoolean     False
      , ColumnDef "updated_at"                      PgTimestampTz False
      ]
  , tdMode          = TableLogged
  , tdPrimaryKey    = Just ["id"]
  , tdChecks        = [ "\"id\" = 1" ]
  , tdColumnDefaults =
      -- id + every counter starts at 1; updated_at tracks the last write.
      ("id", "1") : ("updated_at", "now()") : map (\c -> (c, "1")) syncStateCounterColumns
  }

-- | All columns of the sync-state table, in golden order.
--
-- Kept in sync with 'syncStateTableDef'. Used by 'DbSync.Ledger.SyncState'
-- to build SELECT and UPDATE statements without string-duplicating the
-- column list.
syncStateColumns :: [Text]
syncStateColumns = map cdName (tdColumns syncStateTableDef)

-- | The @*_id_counter@ subset of 'syncStateColumns', in golden order.
--
-- One entry per current 'DbSync.Id.Counter.IdCounters' field.
syncStateCounterColumns :: [Text]
syncStateCounterColumns =
  [ "block_id_counter"
  , "tx_id_counter"
  , "tx_out_id_counter"
  , "tx_in_id_counter"
  , "collateral_tx_in_id_counter"
  , "reference_tx_in_id_counter"
  , "tx_metadata_id_counter"
  , "ma_tx_mint_id_counter"
  , "ma_tx_out_id_counter"
  , "slot_leader_id_counter"
  , "stake_address_id_counter"
  , "pool_hash_id_counter"
  , "multi_asset_id_counter"
  , "script_id_counter"
  , "stake_registration_id_counter"
  , "stake_deregistration_id_counter"
  , "delegation_id_counter"
  , "withdrawal_id_counter"
  , "pool_update_id_counter"
  , "pool_metadata_ref_id_counter"
  , "pool_owner_id_counter"
  , "pool_retire_id_counter"
  , "pool_relay_id_counter"
  , "tx_cbor_id_counter"
  , "epoch_sync_stats_id_counter"
  ]
