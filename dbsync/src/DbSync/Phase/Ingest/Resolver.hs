-- | Ingest-phase ID resolver.
--
-- Uses 'DedupStores' (LSM-backed) and 'IdCounters' ('IORef'-backed)
-- to assign IDs during 'IngestChainHistory'. No live database
-- queries on the hot path; dedup state lives in the shared
-- 'LsmSession' and counter state in an 'IORef' on 'ExtractState'.
--
-- This module is a thin composer: every method body lives in
-- @Resolver\/\<extractor\>.hs@ next door. 'mkIngestResolver' wires
-- them into a single 'IdResolver' record.
module DbSync.Phase.Ingest.Resolver
  ( mkIngestResolver
  ) where

import Cardano.Prelude

import Data.IORef (IORef, readIORef)

import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.DedupStore (DedupStores)
import qualified DbSync.Phase.Ingest.Resolver.Core as Core
import qualified DbSync.Phase.Ingest.Resolver.Epoch as Epoch
import qualified DbSync.Phase.Ingest.Resolver.EpochBoundary as EpochBoundary
import qualified DbSync.Phase.Ingest.Resolver.Governance as Governance
import qualified DbSync.Phase.Ingest.Resolver.MultiAsset as MultiAsset
import qualified DbSync.Phase.Ingest.Resolver.OffChainPools as OffChainPools
import qualified DbSync.Phase.Ingest.Resolver.OffChainVotes as OffChainVotes
import qualified DbSync.Phase.Ingest.Resolver.Pool as Pool
import qualified DbSync.Phase.Ingest.Resolver.ScriptsDatums as ScriptsDatums
import qualified DbSync.Phase.Ingest.Resolver.StakeDelegation as Stake
import qualified DbSync.Phase.Ingest.Resolver.UTxO as UTxO
import DbSync.Phase.Ingest.UtxoStore (UtxoStore)
import DbSync.Resolver (IdResolver (..))
import DbSync.Worker.TxOut.AddressBuffer (AddressBufferRef)
import DbSync.Worker.TxOut.ConsumedByBuffer (ConsumedByBufferRef)

-- | Build an 'IdResolver' for 'IngestChainHistory'.
--
-- Dedup operations look keys up in the LSM-backed 'DedupStores' and
-- allocate the next id from the in-process counter on miss.
-- Non-dedup counter operations use 'atomicModifyIORef'' on
-- 'ExtractState'.
--
-- @recordTxOutAddress@\/@recordCollateralTxOutAddress@ append to the
-- per-epoch 'AddressBufferRef'; the background
-- 'DbSync.Worker.TxOut.Worker' reads the buffer one epoch later,
-- writes the @address@ rows, and fills in
-- @tx_out.address_id@\/@collateral_tx_out.address_id@.
mkIngestResolver
  :: IORef ExtractState
  -> DedupStores
  -> AddressBufferRef
  -> UtxoStore
  -> Maybe ConsumedByBufferRef
  -- ^ 'Just' enables 'recordConsumed' to enqueue triples; 'Nothing'
  -- (feature off) drops them silently.
  -> IdResolver IO
mkIngestResolver extractStateRef dedupStores addrBufRef utxoStore mConsumedByBuf = IdResolver
  { -- Core
    assignBlockId     = Core.assignBlockIdIngest     extractStateRef
  , assignTxId        = Core.assignTxIdIngest        extractStateRef
  , assignTxOutId     = Core.assignTxOutIdIngest     extractStateRef
  , resolveSlotLeader = Core.resolveSlotLeaderIngest dedupStores
  , resolvePrevBlock  = Core.resolvePrevBlockIngest  extractStateRef
  , lookupLastBlockId = fmap (fmap BlockId . esLastBlockId) (readIORef extractStateRef)

    -- UTxO
  , recordTxOutAddress           = UTxO.recordTxOutAddressIngest           addrBufRef
  , recordCollateralTxOutAddress = UTxO.recordCollateralTxOutAddressIngest addrBufRef
  , resolveAddressId             = UTxO.resolveAddressIdIngest
  , assignCollateralTxOutId      = UTxO.assignCollateralTxOutIdIngest      extractStateRef
  , resolveInputValues           = UTxO.resolveInputValuesIngest           utxoStore
  , resolveInputUtxo             = UTxO.resolveInputUtxoIngest             utxoStore
  , recordTxOutputs              = UTxO.recordTxOutputsIngest              utxoStore
  , recordConsumed               = UTxO.recordConsumedIngest               mConsumedByBuf
  , deleteCachedUtxo             = UTxO.deleteCachedUtxoIngest             utxoStore

    -- MultiAsset
  , resolveMultiAsset = MultiAsset.resolveMultiAssetIngest dedupStores

    -- StakeDelegation (incl. pot rebalancing)
  , resolveStakeAddress         = Stake.resolveStakeAddressIngest         dedupStores

    -- Pool
  , resolvePoolHash         = Pool.resolvePoolHashIngest         dedupStores
  , assignPoolUpdateId      = Pool.assignPoolUpdateIdIngest      extractStateRef
  , assignPoolMetadataRefId = Pool.assignPoolMetadataRefIdIngest extractStateRef

    -- OffChainPools
  , enqueuePoolMetaFetch    = OffChainPools.enqueuePoolMetaFetchIngest

    -- OffChainVotes
  , enqueueVoteMetaFetch    = OffChainVotes.enqueueVoteMetaFetchIngest

    -- EpochSyncStats
  , assignEpochSyncStatsId = Epoch.assignEpochSyncStatsIdIngest extractStateRef

    -- EpochBoundary
  , resolveCostModel   = EpochBoundary.resolveCostModelIngest   extractStateRef

    -- ScriptsDatums
  , resolveDatum            = ScriptsDatums.resolveDatumIngest            dedupStores
  , resolveScript           = ScriptsDatums.resolveScriptIngest           dedupStores
  , resolveRedeemerData     = ScriptsDatums.resolveRedeemerDataIngest     dedupStores
  , assignRedeemerId        = ScriptsDatums.assignRedeemerIdIngest        extractStateRef

    -- Governance
  , resolveDrepHash             = Governance.resolveDrepHashIngest             dedupStores
  , resolveCommitteeHash        = Governance.resolveCommitteeHashIngest        dedupStores
  , resolveVotingAnchor         = Governance.resolveVotingAnchorIngest         dedupStores
  , assignGovActionProposalId   = Governance.assignGovActionProposalIdIngest   extractStateRef
  , assignParamProposalId       = Governance.assignParamProposalIdIngest       extractStateRef
  , assignCommitteeId           = Governance.assignCommitteeIdIngest           extractStateRef
  , assignConstitutionId        = Governance.assignConstitutionIdIngest        extractStateRef
  , assignEventInfoId           = Governance.assignEventInfoIdIngest           extractStateRef
  , lookupGovActionProposalId   = Governance.lookupGovActionProposalIdIngest   extractStateRef
  , recordGovActionProposalId   = Governance.recordGovActionProposalIdIngest   extractStateRef
  , readEnactedEpochStateIds    = Governance.readEnactedEpochStateIdsIngest    extractStateRef
  , writeEnactedEpochStateIds   = Governance.writeEnactedEpochStateIdsIngest   extractStateRef
  , readGovExpiresAfter         = Governance.readGovExpiresAfterIngest         extractStateRef
  , writeGovExpiresAfter        = Governance.writeGovExpiresAfterIngest        extractStateRef
  }
