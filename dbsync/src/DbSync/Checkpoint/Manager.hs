-- | Atomic epoch-boundary commit.
--
-- 'commitEpoch' is the one entry point the main ingestion pipeline
-- calls at each epoch boundary. It sequences three pieces of work so
-- that the system can safely crash and resume at any time:
--
--   1. Drain and commit every loader connection owned by the
--      'LoaderStream' — all data rows for the epoch flush to PG.
--   2. Update the 'dbsync_sync_state' singleton on the dedicated
--      'ControlConnection' — @last_committed_*@ and the counters
--      advance.
--   3. Reopen the loader streams so the next epoch can start writing.
--
-- If the sync-state write fails after the data flush succeeds, rows
-- exist past @last_committed_slot@. The resume flow on restart
-- issues @DELETE FROM <t> WHERE slot_no > s@, so the invariant
-- \"@last_committed_slot@ is never ahead of actual data\" holds by
-- construction.
module DbSync.Checkpoint.Manager
  ( commitEpoch
  , mkBoundarySyncStateRow
  , mkResumeExtractState
  ) where

import Cardano.Prelude

import DbSync.Checkpoint.SyncState (HasControlConnection, SyncStateRow (..), writeSyncState)
import DbSync.Db.Loader (LoaderStream (..), HasLoaderStream (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Id.Counter (IdCounter (..), IdCounters (..), mkIdCounter)

-- | Perform an atomic epoch-boundary commit.
--
-- Failure semantics:
--
--   * If step 1 ('lsCommit') throws, no sync-state update happens.
--     On restart the resume flow reads the stale sync state and
--     processing resumes from the previous epoch.
--   * If step 2 ('writeSyncState') throws, data is in PG past
--     @last_committed_slot@. Resume's @DELETE FROM <t> WHERE slot_no
--     > s@ removes it on restart.
--   * If step 3 ('lsReopen') throws, sync state is already advanced
--     and the loader connections are unusable. Caller should treat as
--     fatal and exit; a clean restart reopens the streams.
commitEpoch
  :: ( HasCallStack
     , HasLoaderStream env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => SyncStateRow
  -> m ()
commitEpoch newRow = do
  bs <- asks getLoaderStream
  liftIO (lsCommit bs)
  writeSyncState newRow
  liftIO (lsReopen bs)

-- | Build a 'SyncStateRow' from the boundary block's
-- @(slot, blockNo, hash)@, the current 'IdCounters' snapshot, the
-- address-resolver's in-process address counter, and the run-time
-- configuration. 'ssrLastSnapshotSlot' and 'ssrSyncComplete' are left
-- at their identity values; the @writeSyncState@ encoder ignores
-- those columns.
--
-- The address counter is passed in separately because it lives on the
-- 'DbSync.Address.Worker.AddressResolver' (the worker is the
-- sole allocator of @address.id@) rather than in 'IdCounters'.
mkBoundarySyncStateRow
  :: Word64        -- ^ Last committed slot (boundary block's slot)
  -> Word64        -- ^ Last committed block number
  -> ByteString    -- ^ Last committed block header hash
  -> IdCounters
  -> Int64         -- ^ Next @address.id@ from the address resolver
  -> Int           -- ^ Schema version applied
  -> Bool          -- ^ @ledger.enabled@ from config
  -> SyncStateRow
mkBoundarySyncStateRow slotNo blockNo blockHash counters addressIdCounter schemaVersion ledgerEnabled =
  SyncStateRow
    { ssrLastCommittedSlot             = Just slotNo
    , ssrLastCommittedBlockNo          = Just blockNo
    , ssrLastCommittedBlockHash        = Just blockHash
    , ssrLastSnapshotSlot              = Nothing
    , ssrBlockIdCounter                = icNext (icBlockId            counters)
    , ssrTxIdCounter                   = icNext (icTxId               counters)
    , ssrTxOutIdCounter                = icNext (icTxOutId            counters)
    , ssrTxInIdCounter                 = icNext (icTxInId             counters)
    , ssrCollateralTxInIdCounter       = icNext (icCollateralTxInId   counters)
    , ssrReferenceTxInIdCounter        = icNext (icReferenceTxInId    counters)
    , ssrTxMetadataIdCounter           = icNext (icTxMetadataId       counters)
    , ssrMaTxMintIdCounter             = icNext (icMaTxMintId         counters)
    , ssrMaTxOutIdCounter              = icNext (icMaTxOutId          counters)
    , ssrSlotLeaderIdCounter           = icNext (icSlotLeaderId       counters)
    , ssrAddressIdCounter              = addressIdCounter
    , ssrStakeAddressIdCounter         = icNext (icStakeAddressId     counters)
    , ssrPoolHashIdCounter             = icNext (icPoolHashId         counters)
    , ssrMultiAssetIdCounter           = icNext (icMultiAssetId       counters)
    , ssrScriptIdCounter               = icNext (icScriptId           counters)
    , ssrStakeRegistrationIdCounter    = icNext (icStakeRegistrationId   counters)
    , ssrStakeDeregistrationIdCounter  = icNext (icStakeDeregistrationId counters)
    , ssrDelegationIdCounter           = icNext (icDelegationId          counters)
    , ssrWithdrawalIdCounter           = icNext (icWithdrawalId          counters)
    , ssrPoolUpdateIdCounter           = icNext (icPoolUpdateId          counters)
    , ssrPoolMetadataRefIdCounter      = icNext (icPoolMetadataRefId     counters)
    , ssrPoolOwnerIdCounter            = icNext (icPoolOwnerId           counters)
    , ssrPoolRetireIdCounter           = icNext (icPoolRetireId          counters)
    , ssrPoolRelayIdCounter            = icNext (icPoolRelayId           counters)
    , ssrTxCborIdCounter               = icNext (icTxCborId              counters)
    , ssrEpochSyncStatsIdCounter       = icNext (icEpochSyncStatsId      counters)
    , ssrAdaPotsIdCounter              = icNext (icAdaPotsId             counters)
    , ssrCollateralTxOutIdCounter      = icNext (icCollateralTxOutId     counters)
    , ssrSchemaVersionApplied          = schemaVersion
    , ssrLedgerEnabled                 = ledgerEnabled
    , ssrSyncComplete                  = False
    }

-- | Build the consumer's initial 'ExtractState' from a 'SyncStateRow'
-- read at boot. Each counter resumes at the row's recorded "next id
-- to assign".
mkResumeExtractState :: SyncStateRow -> ExtractState
mkResumeExtractState row =
  ExtractState
    { esIdCounters = IdCounters
        { icBlockId               = mkIdCounter (ssrBlockIdCounter               row)
        , icTxId                  = mkIdCounter (ssrTxIdCounter                  row)
        , icTxOutId               = mkIdCounter (ssrTxOutIdCounter               row)
        , icTxInId                = mkIdCounter (ssrTxInIdCounter                row)
        , icCollateralTxInId      = mkIdCounter (ssrCollateralTxInIdCounter      row)
        , icReferenceTxInId       = mkIdCounter (ssrReferenceTxInIdCounter       row)
        , icTxMetadataId          = mkIdCounter (ssrTxMetadataIdCounter          row)
        , icMaTxMintId            = mkIdCounter (ssrMaTxMintIdCounter            row)
        , icMaTxOutId             = mkIdCounter (ssrMaTxOutIdCounter             row)
        , icSlotLeaderId          = mkIdCounter (ssrSlotLeaderIdCounter          row)
        , icAddressId             = mkIdCounter (ssrAddressIdCounter             row)
        , icStakeAddressId        = mkIdCounter (ssrStakeAddressIdCounter        row)
        , icPoolHashId            = mkIdCounter (ssrPoolHashIdCounter            row)
        , icMultiAssetId          = mkIdCounter (ssrMultiAssetIdCounter          row)
        , icScriptId              = mkIdCounter (ssrScriptIdCounter              row)
        , icStakeRegistrationId   = mkIdCounter (ssrStakeRegistrationIdCounter   row)
        , icStakeDeregistrationId = mkIdCounter (ssrStakeDeregistrationIdCounter row)
        , icDelegationId          = mkIdCounter (ssrDelegationIdCounter          row)
        , icWithdrawalId          = mkIdCounter (ssrWithdrawalIdCounter          row)
        , icPoolUpdateId          = mkIdCounter (ssrPoolUpdateIdCounter          row)
        , icPoolMetadataRefId     = mkIdCounter (ssrPoolMetadataRefIdCounter     row)
        , icPoolOwnerId           = mkIdCounter (ssrPoolOwnerIdCounter           row)
        , icPoolRetireId          = mkIdCounter (ssrPoolRetireIdCounter          row)
        , icPoolRelayId           = mkIdCounter (ssrPoolRelayIdCounter           row)
        , icTxCborId              = mkIdCounter (ssrTxCborIdCounter              row)
        , icEpochSyncStatsId      = mkIdCounter (ssrEpochSyncStatsIdCounter      row)
        , icAdaPotsId             = mkIdCounter (ssrAdaPotsIdCounter             row)
        , icCollateralTxOutId     = mkIdCounter (ssrCollateralTxOutIdCounter     row)
        }
    , esLastBlockId = Nothing
    }
