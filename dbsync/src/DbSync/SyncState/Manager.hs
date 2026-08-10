-- | Epoch-boundary commit. 'commitEpoch' is the single entry point
-- the ingestion pipeline calls at each boundary, and it orders the
-- work so the system can crash and resume at any point.
module DbSync.SyncState.Manager
  ( commitEpoch
  , mkBoundarySyncStateRow
  , mkResumeExtractState
  ) where

import Cardano.Prelude

import DbSync.SyncState.Row (HasControlConnection, SyncStateRow (..), writeSyncState)
import DbSync.Db.Loader (LoaderStream (..), HasLoaderStream (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounter (..), IdCounters (..), mkIdCounter)

-- | Flush the epoch's rows, advance the sync state, then reopen the
-- loader streams. Failure semantics per step:
--
--   * 'lsCommit' throws: no sync-state update happens, and the
--     restart resumes from the previous epoch.
--   * 'writeSyncState' throws: rows sit in PG past
--     @last_committed_slot@, and the resume DELETE removes them.
--   * 'lsReopen' throws: the sync state already advanced and the
--     loader connections are dead. The caller must exit, and a clean
--     restart reopens the streams.
commitEpoch
  :: ( HasLoaderStream env
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

-- | Build a 'SyncStateRow' for the boundary block.
-- 'ssrLastSnapshotSlot', 'ssrSyncComplete' and
-- 'ssrPendingRollbackSlot' stay at their identity values, because
-- the @writeSyncState@ encoder ignores those columns.
--
-- The address counter arrives separately: it lives on the
-- 'DbSync.Worker.TxOut.Worker.TxOutWorker', which is the only
-- allocator of @address.id@, not in 'IdCounters'.
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
    , ssrSlotLeaderIdCounter           = icNext (icSlotLeaderId       counters)
    , ssrAddressIdCounter              = addressIdCounter
    , ssrStakeAddressIdCounter         = icNext (icStakeAddressId     counters)
    , ssrPoolHashIdCounter             = icNext (icPoolHashId         counters)
    , ssrMultiAssetIdCounter           = icNext (icMultiAssetId       counters)
    , ssrScriptIdCounter               = icNext (icScriptId           counters)
    , ssrPoolUpdateIdCounter           = icNext (icPoolUpdateId          counters)
    , ssrPoolMetadataRefIdCounter      = icNext (icPoolMetadataRefId     counters)
    , ssrCostModelIdCounter            = icNext (icCostModelId           counters)
    , ssrRedeemerIdCounter             = icNext (icRedeemerId            counters)
    , ssrCollateralTxOutIdCounter      = icNext (icCollateralTxOutId     counters)
    , ssrEpochSyncStatsIdCounter       = icNext (icEpochSyncStatsId      counters)
    , ssrGovActionProposalIdCounter    = icNext (icGovActionProposalId   counters)
    , ssrParamProposalIdCounter        = icNext (icParamProposalId       counters)
    , ssrCommitteeIdCounter            = icNext (icCommitteeId           counters)
    , ssrConstitutionIdCounter         = icNext (icConstitutionId        counters)
    , ssrEventInfoIdCounter            = icNext (icEventInfoId           counters)
    , ssrSchemaVersionApplied          = schemaVersion
    , ssrLedgerEnabled                 = ledgerEnabled
    , ssrSyncComplete                  = False
    , ssrPendingRollbackSlot           = Nothing
    }

-- | Build the consumer's initial 'ExtractState' from the boot-time
-- 'SyncStateRow'. Each counter resumes at the row's recorded next id.
mkResumeExtractState :: SyncStateRow -> ExtractState
mkResumeExtractState row =
  ExtractState
    { esIdCounters = IdCounters
        { icBlockId               = mkIdCounter (ssrBlockIdCounter               row)
        , icTxId                  = mkIdCounter (ssrTxIdCounter                  row)
        , icTxOutId               = mkIdCounter (ssrTxOutIdCounter               row)
        , icSlotLeaderId          = mkIdCounter (ssrSlotLeaderIdCounter          row)
        , icAddressId             = mkIdCounter (ssrAddressIdCounter             row)
        , icStakeAddressId        = mkIdCounter (ssrStakeAddressIdCounter        row)
        , icPoolHashId            = mkIdCounter (ssrPoolHashIdCounter            row)
        , icMultiAssetId          = mkIdCounter (ssrMultiAssetIdCounter          row)
        , icScriptId              = mkIdCounter (ssrScriptIdCounter              row)
        , icPoolUpdateId          = mkIdCounter (ssrPoolUpdateIdCounter          row)
        , icPoolMetadataRefId     = mkIdCounter (ssrPoolMetadataRefIdCounter     row)
        , icCostModelId           = mkIdCounter (ssrCostModelIdCounter           row)
        , icRedeemerId            = mkIdCounter (ssrRedeemerIdCounter            row)
        , icCollateralTxOutId     = mkIdCounter (ssrCollateralTxOutIdCounter     row)
        , icEpochSyncStatsId      = mkIdCounter (ssrEpochSyncStatsIdCounter      row)
        , icGovActionProposalId   = mkIdCounter (ssrGovActionProposalIdCounter   row)
        , icParamProposalId       = mkIdCounter (ssrParamProposalIdCounter       row)
        , icCommitteeId           = mkIdCounter (ssrCommitteeIdCounter           row)
        , icConstitutionId        = mkIdCounter (ssrConstitutionIdCounter        row)
        , icEventInfoId           = mkIdCounter (ssrEventInfoIdCounter           row)
        }
    , esLastBlockId            = Nothing
    , esCostModelCache         = mempty
    , esGovActionProposalCache = mempty
    , esCurrentCommitteeId     = Nothing
    , esCurrentNoConfidenceId  = Nothing
    , esCurrentConstitutionId  = Nothing
    , esGovExpiresAfter        = Nothing
    }
