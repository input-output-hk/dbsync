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

    -- * Column records (compile-time-safe column references)
  , SyncStateCols (..), syncStateCols, syncStateColsList

    -- * Per-module column-record registry
  , syncStateColumnRecords

    -- * Table-to-counter mapping
  , idCounterByTable

    -- * Hasql encoders \/ decoders
  , syncStateRowEncoder
  , syncStateRowDecoder
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.EpochBoundary (costModelTableDef)
import DbSync.Db.Schema.EpochSyncStats (epochSyncStatsTableDef)
import DbSync.Db.Schema.Governance
  ( committeeTableDef
  , constitutionTableDef
  , eventInfoTableDef
  , govActionProposalTableDef
  , paramProposalTableDef
  )
import DbSync.Db.Schema.MultiAsset (multiAssetTableDef)
import DbSync.Db.Schema.Pool
  ( poolHashTableDef
  , poolMetadataRefTableDef
  , poolUpdateTableDef
  )
import DbSync.Db.Schema.ScriptsDatums
  ( redeemerTableDef
  , scriptTableDef
  )
import DbSync.Db.Schema.StakeDelegation (stakeAddressTableDef)
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableColumn (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Schema.UTxO (collateralTxOutTableDef, txOutTableDef)

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
  , ssrSlotLeaderIdCounter           :: !Int64
  , ssrAddressIdCounter              :: !Int64
  , ssrStakeAddressIdCounter         :: !Int64
  , ssrPoolHashIdCounter             :: !Int64
  , ssrMultiAssetIdCounter           :: !Int64
  , ssrScriptIdCounter               :: !Int64
  , ssrPoolUpdateIdCounter           :: !Int64
  , ssrPoolMetadataRefIdCounter      :: !Int64
  , ssrCostModelIdCounter            :: !Int64
  , ssrRedeemerIdCounter             :: !Int64
  , ssrCollateralTxOutIdCounter      :: !Int64
  , ssrEpochSyncStatsIdCounter       :: !Int64
  , ssrGovActionProposalIdCounter    :: !Int64
  , ssrParamProposalIdCounter        :: !Int64
  , ssrCommitteeIdCounter            :: !Int64
  , ssrConstitutionIdCounter         :: !Int64
  , ssrEventInfoIdCounter            :: !Int64
  , ssrSchemaVersionApplied          :: !Int
  , ssrLedgerEnabled                 :: !Bool
  , ssrSyncComplete                  :: !Bool
  , ssrPendingRollbackSlot           :: !(Maybe Word64)
    -- ^ Target slot for a rollback that must run on next boot. Set
    -- by the ledger worker when a chainsync rollback target lies past
    -- the in-memory buffer, or by a crashed mid-rollback CLI request.
    -- Cleared by the boot path after the recovery rollback completes.
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table metadata
-- ---------------------------------------------------------------------------

syncStateTableName :: Text
syncStateTableName = "dbsync_sync_state"

-- | DDL definition. The counter columns are derived from
-- 'idCounterByTable' below, so the SQL shape stays in lock-step
-- with the cleanup mapping. Counters default to 1 so a freshly
-- seeded row is usable without further writes; @sync_complete@
-- defaults to false; @updated_at@ tracks the last write via
-- @now()@.
syncStateTableDef :: TableDef
syncStateTableDef = TableDef
  { tdName    = syncStateTableName
  , tdColumns =
      [ ColumnDef "id"                              PgSmallInt    False
      , ColumnDef "last_committed_slot"             PgBigInt      True
      , ColumnDef "last_committed_block_no"         PgBigInt      True
      , ColumnDef "last_committed_block_hash"       PgBytea       True
      , ColumnDef "last_snapshot_slot"              PgBigInt      True
      ]
      <> counterColumnDefs
      <>
      [ ColumnDef "schema_version_applied"          PgInteger     False
      , ColumnDef "ledger_enabled"                  PgBoolean     False
      , ColumnDef "sync_complete"                   PgBoolean     False
      , ColumnDef "pending_rollback_slot"           PgBigInt      True
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
  , tdIdentityColumns = []
  , tdForeignKeys = []
  }
  where
    counterColumnDefs =
      [ ColumnDef col PgBigInt False | col <- syncStateCounterColumns ]

-- ---------------------------------------------------------------------------
-- * Column-name helpers
-- ---------------------------------------------------------------------------

-- | All columns, in golden order. Drives the generated SELECT.
syncStateColumns :: [Text]
syncStateColumns = map cdName (tdColumns syncStateTableDef)

-- | The @*_id_counter@ subset, in golden order. Derived from
-- 'idCounterByTable' so a new entry there flows automatically into
-- the DDL and the COPY-defaults list.
syncStateCounterColumns :: [Text]
syncStateCounterColumns = map ((<> "_id_counter") . fst) idCounterByTable

-- ---------------------------------------------------------------------------
-- * Table-to-counter mapping
-- ---------------------------------------------------------------------------

-- | Data-table to its @next-id-to-assign@ selector on
-- 'SyncStateRow'. The single source of truth for which counter goes
-- with which table — drives both the resume-time cleanup in
-- 'DbSync.SyncState.Resume.deleteRowsPastSlot' and the derived
-- 'syncStateCounterColumns' / DDL above.
--
-- Order matches the field order in 'SyncStateRow' and the encoder /
-- decoder below; reordering here without matching those breaks the
-- on-disk schema layout.
--
-- @address@ is included even though its counter is owned by the
-- @AddressResolver@ worker rather than 'IdCounters': the worker's
-- own writes can leave rows past the @sync_state@ snapshot when a
-- crash lands between an INSERT and the next 'writeSyncState'.
idCounterByTable :: [(Text, SyncStateRow -> Int64)]
idCounterByTable =
  [ (tdName blockTableDef,                ssrBlockIdCounter)
  , (tdName txTableDef,                   ssrTxIdCounter)
  , (tdName txOutTableDef,                ssrTxOutIdCounter)
  , (tdName slotLeaderTableDef,           ssrSlotLeaderIdCounter)
  , (tdName addressTableDef,              ssrAddressIdCounter)
  , (tdName stakeAddressTableDef,         ssrStakeAddressIdCounter)
  , (tdName poolHashTableDef,             ssrPoolHashIdCounter)
  , (tdName multiAssetTableDef,           ssrMultiAssetIdCounter)
  , (tdName scriptTableDef,               ssrScriptIdCounter)
  , (tdName poolUpdateTableDef,           ssrPoolUpdateIdCounter)
  , (tdName poolMetadataRefTableDef,      ssrPoolMetadataRefIdCounter)
  , (tdName costModelTableDef,            ssrCostModelIdCounter)
  , (tdName redeemerTableDef,             ssrRedeemerIdCounter)
  , (tdName collateralTxOutTableDef,      ssrCollateralTxOutIdCounter)
  , (tdName epochSyncStatsTableDef,       ssrEpochSyncStatsIdCounter)
  , (tdName govActionProposalTableDef,    ssrGovActionProposalIdCounter)
  , (tdName paramProposalTableDef,        ssrParamProposalIdCounter)
  , (tdName committeeTableDef,            ssrCommitteeIdCounter)
  , (tdName constitutionTableDef,         ssrConstitutionIdCounter)
  , (tdName eventInfoTableDef,            ssrEventInfoIdCounter)
  ]

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data SyncStateCols = SyncStateCols
  { sscId                          :: !TableColumn
  , sscLastCommittedSlot           :: !TableColumn
  , sscLastCommittedBlockNo        :: !TableColumn
  , sscLastCommittedBlockHash      :: !TableColumn
  , sscLastSnapshotSlot            :: !TableColumn
  , sscBlockIdCounter              :: !TableColumn
  , sscTxIdCounter                 :: !TableColumn
  , sscTxOutIdCounter              :: !TableColumn
  , sscSlotLeaderIdCounter         :: !TableColumn
  , sscAddressIdCounter            :: !TableColumn
  , sscStakeAddressIdCounter       :: !TableColumn
  , sscPoolHashIdCounter           :: !TableColumn
  , sscMultiAssetIdCounter         :: !TableColumn
  , sscScriptIdCounter             :: !TableColumn
  , sscPoolUpdateIdCounter         :: !TableColumn
  , sscPoolMetadataRefIdCounter    :: !TableColumn
  , sscCostModelIdCounter          :: !TableColumn
  , sscRedeemerIdCounter           :: !TableColumn
  , sscCollateralTxOutIdCounter    :: !TableColumn
  , sscEpochSyncStatsIdCounter     :: !TableColumn
  , sscGovActionProposalIdCounter  :: !TableColumn
  , sscParamProposalIdCounter      :: !TableColumn
  , sscCommitteeIdCounter          :: !TableColumn
  , sscConstitutionIdCounter       :: !TableColumn
  , sscEventInfoIdCounter          :: !TableColumn
  , sscSchemaVersionApplied        :: !TableColumn
  , sscLedgerEnabled               :: !TableColumn
  , sscSyncComplete                :: !TableColumn
  , sscPendingRollbackSlot         :: !TableColumn
  , sscUpdatedAt                   :: !TableColumn
  }

syncStateCols :: SyncStateCols
syncStateCols =
  let c = TableColumn syncStateTableDef
  in SyncStateCols
       { sscId                         = c "id"
       , sscLastCommittedSlot          = c "last_committed_slot"
       , sscLastCommittedBlockNo       = c "last_committed_block_no"
       , sscLastCommittedBlockHash     = c "last_committed_block_hash"
       , sscLastSnapshotSlot           = c "last_snapshot_slot"
       , sscBlockIdCounter             = c "block_id_counter"
       , sscTxIdCounter                = c "tx_id_counter"
       , sscTxOutIdCounter             = c "tx_out_id_counter"
       , sscSlotLeaderIdCounter        = c "slot_leader_id_counter"
       , sscAddressIdCounter           = c "address_id_counter"
       , sscStakeAddressIdCounter      = c "stake_address_id_counter"
       , sscPoolHashIdCounter          = c "pool_hash_id_counter"
       , sscMultiAssetIdCounter        = c "multi_asset_id_counter"
       , sscScriptIdCounter            = c "script_id_counter"
       , sscPoolUpdateIdCounter        = c "pool_update_id_counter"
       , sscPoolMetadataRefIdCounter   = c "pool_metadata_ref_id_counter"
       , sscCostModelIdCounter         = c "cost_model_id_counter"
       , sscRedeemerIdCounter          = c "redeemer_id_counter"
       , sscCollateralTxOutIdCounter   = c "collateral_tx_out_id_counter"
       , sscEpochSyncStatsIdCounter    = c "epoch_sync_stats_id_counter"
       , sscGovActionProposalIdCounter = c "gov_action_proposal_id_counter"
       , sscParamProposalIdCounter     = c "param_proposal_id_counter"
       , sscCommitteeIdCounter         = c "committee_id_counter"
       , sscConstitutionIdCounter      = c "constitution_id_counter"
       , sscEventInfoIdCounter         = c "event_info_id_counter"
       , sscSchemaVersionApplied       = c "schema_version_applied"
       , sscLedgerEnabled              = c "ledger_enabled"
       , sscSyncComplete               = c "sync_complete"
       , sscPendingRollbackSlot        = c "pending_rollback_slot"
       , sscUpdatedAt                  = c "updated_at"
       }

syncStateColsList :: [TableColumn]
syncStateColsList =
  [ syncStateCols.sscId
  , syncStateCols.sscLastCommittedSlot
  , syncStateCols.sscLastCommittedBlockNo
  , syncStateCols.sscLastCommittedBlockHash
  , syncStateCols.sscLastSnapshotSlot
  , syncStateCols.sscBlockIdCounter
  , syncStateCols.sscTxIdCounter
  , syncStateCols.sscTxOutIdCounter
  , syncStateCols.sscSlotLeaderIdCounter
  , syncStateCols.sscAddressIdCounter
  , syncStateCols.sscStakeAddressIdCounter
  , syncStateCols.sscPoolHashIdCounter
  , syncStateCols.sscMultiAssetIdCounter
  , syncStateCols.sscScriptIdCounter
  , syncStateCols.sscPoolUpdateIdCounter
  , syncStateCols.sscPoolMetadataRefIdCounter
  , syncStateCols.sscCostModelIdCounter
  , syncStateCols.sscRedeemerIdCounter
  , syncStateCols.sscCollateralTxOutIdCounter
  , syncStateCols.sscEpochSyncStatsIdCounter
  , syncStateCols.sscGovActionProposalIdCounter
  , syncStateCols.sscParamProposalIdCounter
  , syncStateCols.sscCommitteeIdCounter
  , syncStateCols.sscConstitutionIdCounter
  , syncStateCols.sscEventInfoIdCounter
  , syncStateCols.sscSchemaVersionApplied
  , syncStateCols.sscLedgerEnabled
  , syncStateCols.sscSyncComplete
  , syncStateCols.sscPendingRollbackSlot
  , syncStateCols.sscUpdatedAt
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

syncStateColumnRecords :: [(TableDef, [TableColumn])]
syncStateColumnRecords =
  [ (syncStateTableDef, syncStateColsList)
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
  <> (ssrSlotLeaderIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrAddressIdCounter                          >$< E.param (E.nonNullable E.int8))
  <> (ssrStakeAddressIdCounter                     >$< E.param (E.nonNullable E.int8))
  <> (ssrPoolHashIdCounter                         >$< E.param (E.nonNullable E.int8))
  <> (ssrMultiAssetIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrScriptIdCounter                           >$< E.param (E.nonNullable E.int8))
  <> (ssrPoolUpdateIdCounter                       >$< E.param (E.nonNullable E.int8))
  <> (ssrPoolMetadataRefIdCounter                  >$< E.param (E.nonNullable E.int8))
  <> (ssrCostModelIdCounter                        >$< E.param (E.nonNullable E.int8))
  <> (ssrRedeemerIdCounter                         >$< E.param (E.nonNullable E.int8))
  <> (ssrCollateralTxOutIdCounter                  >$< E.param (E.nonNullable E.int8))
  <> (ssrEpochSyncStatsIdCounter                   >$< E.param (E.nonNullable E.int8))
  <> (ssrGovActionProposalIdCounter                >$< E.param (E.nonNullable E.int8))
  <> (ssrParamProposalIdCounter                    >$< E.param (E.nonNullable E.int8))
  <> (ssrCommitteeIdCounter                        >$< E.param (E.nonNullable E.int8))
  <> (ssrConstitutionIdCounter                     >$< E.param (E.nonNullable E.int8))
  <> (ssrEventInfoIdCounter                        >$< E.param (E.nonNullable E.int8))
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
        <*> D.column (D.nonNullable D.int8)                        -- slot_leader_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- address_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- stake_address_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- pool_hash_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- multi_asset_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- script_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- pool_update_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- pool_metadata_ref_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- cost_model_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- redeemer_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- collateral_tx_out_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- epoch_sync_stats_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- gov_action_proposal_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- param_proposal_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- committee_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- constitution_id_counter
        <*> D.column (D.nonNullable D.int8)                        -- event_info_id_counter
        <*> (fromIntegral <$> D.column (D.nonNullable D.int4))     -- schema_version_applied
        <*> D.column (D.nonNullable D.bool)                        -- ledger_enabled
        <*> D.column (D.nonNullable D.bool)                        -- sync_complete
        <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))   -- pending_rollback_slot
       )
    <* skipCol D.timestamptz                                   -- updated_at
  where
    -- Read a column at the current position and discard the value.
    -- The result type doesn't matter; we only care about advancing
    -- past the column in the row.
    skipCol :: D.Value a -> D.Row a
    skipCol = D.column . D.nonNullable
