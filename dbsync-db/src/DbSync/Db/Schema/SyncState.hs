{-# LANGUAGE OverloadedStrings #-}

-- | Schema, types, and hasql encoders\/decoders for the
-- @dbsync_sync_state@ singleton metadata table.
--
-- This table is the one piece of state that survives a restart
-- regardless of ledger mode. It is LOGGED, never a COPY target, and
-- carries a single row pinned by @CHECK (id = 1)@.
--
-- Three writers, each touching a disjoint set of columns:
--
--   * Consumer ('commitEpoch') writes @last_committed_*@ and the
--     @*_id_counter@ columns.
--   * Snapshot writer writes @last_snapshot_slot@.
--   * Phase-transition flip writes @sync_complete@.
module DbSync.Db.Schema.SyncState
  ( -- * Row type
    SyncStateRow (..)

    -- * Table metadata
  , syncStateTableName
  , syncStateTableDef

    -- * Column-name helpers
  , syncStateColumns
  , syncStateCounterColumns

    -- * Hasql encoders \/ decoders
  , syncStateRowEncoder
  , syncStateRowDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )

-- ---------------------------------------------------------------------------
-- * Row type
-- ---------------------------------------------------------------------------

-- | A single row from the @dbsync_sync_state@ table.
--
-- Field order matches 'tdColumns' (skipping @id@ and @updated_at@,
-- which are managed by the table itself). The encoder, decoder, and
-- generated SQL below all rely on this ordering.
data SyncStateRow = SyncStateRow
  { ssrLastCommittedSlot             :: !(Maybe Word64)
  , ssrLastCommittedBlockNo          :: !(Maybe Word64)
  , ssrLastCommittedBlockHash        :: !(Maybe ByteString)
  , ssrLastSnapshotSlot              :: !(Maybe Word64)
  , ssrBlockIdCounter                :: !Int64
  , ssrTxIdCounter                   :: !Int64
  , ssrTxOutIdCounter                :: !Int64
  , ssrTxInIdCounter                 :: !Int64
  , ssrCollateralTxInIdCounter       :: !Int64
  , ssrReferenceTxInIdCounter        :: !Int64
  , ssrTxMetadataIdCounter           :: !Int64
  , ssrMaTxMintIdCounter             :: !Int64
  , ssrMaTxOutIdCounter              :: !Int64
  , ssrSlotLeaderIdCounter           :: !Int64
  , ssrAddressIdCounter              :: !Int64
  , ssrStakeAddressIdCounter         :: !Int64
  , ssrPoolHashIdCounter             :: !Int64
  , ssrMultiAssetIdCounter           :: !Int64
  , ssrScriptIdCounter               :: !Int64
  , ssrStakeRegistrationIdCounter    :: !Int64
  , ssrStakeDeregistrationIdCounter  :: !Int64
  , ssrDelegationIdCounter           :: !Int64
  , ssrWithdrawalIdCounter           :: !Int64
  , ssrPoolUpdateIdCounter           :: !Int64
  , ssrPoolMetadataRefIdCounter      :: !Int64
  , ssrPoolOwnerIdCounter            :: !Int64
  , ssrPoolRetireIdCounter           :: !Int64
  , ssrPoolRelayIdCounter            :: !Int64
  , ssrTxCborIdCounter               :: !Int64
  , ssrEpochSyncStatsIdCounter       :: !Int64
  , ssrAdaPotsIdCounter              :: !Int64
  , ssrCollateralTxOutIdCounter      :: !Int64
  , ssrSchemaVersionApplied          :: !Int
  , ssrLedgerEnabled                 :: !Bool
  , ssrSyncComplete                  :: !Bool
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table metadata
-- ---------------------------------------------------------------------------

syncStateTableName :: Text
syncStateTableName = "dbsync_sync_state"

-- | DDL definition. Counters default to 1 so a freshly-seeded row is
-- usable without further writes; @sync_complete@ defaults to false;
-- @updated_at@ tracks the last write via @now()@.
syncStateTableDef :: TableDef
syncStateTableDef = TableDef
  { tdName    = syncStateTableName
  , tdColumns =
      [ ColumnDef "id"                              PgSmallInt    False
      , ColumnDef "last_committed_slot"             PgBigInt      True
      , ColumnDef "last_committed_block_no"         PgBigInt      True
      , ColumnDef "last_committed_block_hash"       PgBytea       True
      , ColumnDef "last_snapshot_slot"              PgBigInt      True
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
      , ColumnDef "address_id_counter"              PgBigInt      False
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
      , ColumnDef "ada_pots_id_counter"             PgBigInt      False
      , ColumnDef "collateral_tx_out_id_counter"    PgBigInt      False
      , ColumnDef "schema_version_applied"          PgInteger     False
      , ColumnDef "ledger_enabled"                  PgBoolean     False
      , ColumnDef "sync_complete"                   PgBoolean     False
      , ColumnDef "updated_at"                      PgTimestampTz False
      ]
  , tdMode          = TableLogged
  , tdPrimaryKey    = Just ["id"]
  , tdChecks        = [ "\"id\" = 1" ]
  , tdColumnDefaults =
      ("id", "1")
        : ("sync_complete", "false")
        : ("updated_at", "now()")
        : map (\c -> (c, "1")) syncStateCounterColumns
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  }

-- ---------------------------------------------------------------------------
-- * Column-name helpers
-- ---------------------------------------------------------------------------

-- | All columns, in golden order. Drives the generated SELECT.
syncStateColumns :: [Text]
syncStateColumns = map cdName (tdColumns syncStateTableDef)

-- | The @*_id_counter@ subset, in golden order. One per current
-- 'DbSync.Id.Counter.IdCounters' field.
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
  , "address_id_counter"
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
  , "ada_pots_id_counter"
  , "collateral_tx_out_id_counter"
  ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- | Encoder for the consumer-owned columns. Order matches the
-- placeholder numbering in
-- 'DbSync.Db.Statement.SyncState.writeSyncStateStmt'.
syncStateRowEncoder :: E.Params SyncStateRow
syncStateRowEncoder =
     (fmap fromIntegral . ssrLastCommittedSlot     >$< E.param (E.nullable E.int8))
  <> (fmap fromIntegral . ssrLastCommittedBlockNo  >$< E.param (E.nullable E.int8))
  <> (ssrLastCommittedBlockHash                    >$< E.param (E.nullable E.bytea))
  <> (ssrBlockIdCounter                            >$< E.param (E.nonNullable E.int8))
  <> (ssrTxIdCounter                               >$< E.param (E.nonNullable E.int8))
  <> (ssrTxOutIdCounter                            >$< E.param (E.nonNullable E.int8))
  <> (ssrTxInIdCounter                             >$< E.param (E.nonNullable E.int8))
  <> (ssrCollateralTxInIdCounter                   >$< E.param (E.nonNullable E.int8))
  <> (ssrReferenceTxInIdCounter                    >$< E.param (E.nonNullable E.int8))
  <> (ssrTxMetadataIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrMaTxMintIdCounter                         >$< E.param (E.nonNullable E.int8))
  <> (ssrMaTxOutIdCounter                          >$< E.param (E.nonNullable E.int8))
  <> (ssrSlotLeaderIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrAddressIdCounter                          >$< E.param (E.nonNullable E.int8))
  <> (ssrStakeAddressIdCounter                     >$< E.param (E.nonNullable E.int8))
  <> (ssrPoolHashIdCounter                         >$< E.param (E.nonNullable E.int8))
  <> (ssrMultiAssetIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrScriptIdCounter                           >$< E.param (E.nonNullable E.int8))
  <> (ssrStakeRegistrationIdCounter                >$< E.param (E.nonNullable E.int8))
  <> (ssrStakeDeregistrationIdCounter              >$< E.param (E.nonNullable E.int8))
  <> (ssrDelegationIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrWithdrawalIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrPoolUpdateIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrPoolMetadataRefIdCounter                  >$< E.param (E.nonNullable E.int8))
  <> (ssrPoolOwnerIdCounter                        >$< E.param (E.nonNullable E.int8))
  <> (ssrPoolRetireIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrPoolRelayIdCounter                        >$< E.param (E.nonNullable E.int8))
  <> (ssrTxCborIdCounter                           >$< E.param (E.nonNullable E.int8))
  <> (ssrEpochSyncStatsIdCounter                   >$< E.param (E.nonNullable E.int8))
  <> (ssrAdaPotsIdCounter                          >$< E.param (E.nonNullable E.int8))
  <> (ssrCollateralTxOutIdCounter                  >$< E.param (E.nonNullable E.int8))
  <> (fromIntegral . ssrSchemaVersionApplied       >$< E.param (E.nonNullable E.int4))
  <> (ssrLedgerEnabled                             >$< E.param (E.nonNullable E.bool))

-- | Decoder for a row produced by
-- 'DbSync.Db.Statement.SyncState.readSyncStateStmt'.
--
-- Consumes every column of the table in 'tdColumns' order so the
-- statement can use a plain @SELECT *@. The leading @id@ and
-- trailing @updated_at@ are discarded — neither belongs in
-- 'SyncStateRow' (the id is fixed at 1 by CHECK; @updated_at@ is
-- managed by the SET clause).
syncStateRowDecoder :: D.Row SyncStateRow
syncStateRowDecoder =
       skipCol D.int2                                          -- id
    *> ( SyncStateRow
        <$> (fmap fromIntegral <$> D.column (D.nullable D.int8))   -- last_committed_slot
        <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))   -- last_committed_block_no
        <*> D.column (D.nullable D.bytea)                          -- last_committed_block_hash
        <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))   -- last_snapshot_slot
        <*> D.column (D.nonNullable D.int8)                        -- block_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- tx_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- tx_out_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- tx_in_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- collateral_tx_in_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- reference_tx_in_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- tx_metadata_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- ma_tx_mint_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- ma_tx_out_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- slot_leader_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- address_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- stake_address_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- pool_hash_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- multi_asset_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- script_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- stake_registration_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- stake_deregistration_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- delegation_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- withdrawal_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- pool_update_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- pool_metadata_ref_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- pool_owner_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- pool_retire_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- pool_relay_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- tx_cbor_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- epoch_sync_stats_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- ada_pots_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- collateral_tx_out_id_counter
        <*> (fromIntegral <$> D.column (D.nonNullable D.int4))     -- schema_version_applied
        <*> D.column (D.nonNullable D.bool)                        -- ledger_enabled
        <*> D.column (D.nonNullable D.bool)                        -- sync_complete
       )
    <* skipCol D.timestamptz                                   -- updated_at
  where
    -- Read a column at the current position and discard the value.
    -- The result type doesn't matter; we only care about advancing
    -- past the column in the row.
    skipCol :: D.Value a -> D.Row a
    skipCol = D.column . D.nonNullable
