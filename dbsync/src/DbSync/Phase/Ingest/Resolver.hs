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

import Data.IORef (IORef)

import DbSync.Extractor (ExtractState)
import DbSync.Phase.Ingest.DedupStore (DedupStores)
import qualified DbSync.Phase.Ingest.Resolver.Cbor as Cbor
import qualified DbSync.Phase.Ingest.Resolver.Core as Core
import qualified DbSync.Phase.Ingest.Resolver.Epoch as Epoch
import qualified DbSync.Phase.Ingest.Resolver.EpochBoundary as EpochBoundary
import qualified DbSync.Phase.Ingest.Resolver.Metadata as Metadata
import qualified DbSync.Phase.Ingest.Resolver.MultiAsset as MultiAsset
import qualified DbSync.Phase.Ingest.Resolver.Pool as Pool
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

    -- UTxO
  , recordTxOutAddress           = UTxO.recordTxOutAddressIngest           addrBufRef
  , recordCollateralTxOutAddress = UTxO.recordCollateralTxOutAddressIngest addrBufRef
  , resolveAddressId             = UTxO.resolveAddressIdIngest
  , assignTxInId                 = UTxO.assignTxInIdIngest                 extractStateRef
  , assignCollateralTxInId       = UTxO.assignCollateralTxInIdIngest       extractStateRef
  , assignCollateralTxOutId      = UTxO.assignCollateralTxOutIdIngest      extractStateRef
  , assignReferenceTxInId        = UTxO.assignReferenceTxInIdIngest        extractStateRef
  , resolveInputValues           = UTxO.resolveInputValuesIngest           utxoStore
  , resolveInputUtxo             = UTxO.resolveInputUtxoIngest             utxoStore
  , recordTxOutputs              = UTxO.recordTxOutputsIngest              utxoStore
  , recordConsumed               = UTxO.recordConsumedIngest               mConsumedByBuf
  , deleteCachedUtxo             = UTxO.deleteCachedUtxoIngest             utxoStore

    -- Metadata
  , assignTxMetadataId = Metadata.assignTxMetadataIdIngest extractStateRef

    -- MultiAsset
  , resolveMultiAsset = MultiAsset.resolveMultiAssetIngest dedupStores
  , assignMaTxMintId  = MultiAsset.assignMaTxMintIdIngest  extractStateRef
  , assignMaTxOutId   = MultiAsset.assignMaTxOutIdIngest   extractStateRef

    -- StakeDelegation (incl. pot rebalancing)
  , resolveStakeAddress         = Stake.resolveStakeAddressIngest         dedupStores
  , assignStakeRegistrationId   = Stake.assignStakeRegistrationIdIngest   extractStateRef
  , assignStakeDeregistrationId = Stake.assignStakeDeregistrationIdIngest extractStateRef
  , assignDelegationId          = Stake.assignDelegationIdIngest          extractStateRef
  , assignWithdrawalId          = Stake.assignWithdrawalIdIngest          extractStateRef
  , assignPotTransferId         = Stake.assignPotTransferIdIngest         extractStateRef
  , assignTreasuryId            = Stake.assignTreasuryIdIngest            extractStateRef
  , assignReserveId             = Stake.assignReserveIdIngest             extractStateRef

    -- Pool
  , resolvePoolHash         = Pool.resolvePoolHashIngest         dedupStores
  , assignPoolUpdateId      = Pool.assignPoolUpdateIdIngest      extractStateRef
  , assignPoolMetadataRefId = Pool.assignPoolMetadataRefIdIngest extractStateRef
  , assignPoolOwnerId       = Pool.assignPoolOwnerIdIngest       extractStateRef
  , assignPoolRetireId      = Pool.assignPoolRetireIdIngest      extractStateRef
  , assignPoolRelayId       = Pool.assignPoolRelayIdIngest       extractStateRef

    -- CBOR
  , assignTxCborId = Cbor.assignTxCborIdIngest extractStateRef

    -- EpochSyncStats
  , assignEpochSyncStatsId = Epoch.assignEpochSyncStatsIdIngest extractStateRef

    -- EpochBoundary
  , assignAdaPotsId    = EpochBoundary.assignAdaPotsIdIngest    extractStateRef
  , assignEpochParamId = EpochBoundary.assignEpochParamIdIngest extractStateRef
  , assignEpochStateId = EpochBoundary.assignEpochStateIdIngest extractStateRef
  , resolveCostModel   = EpochBoundary.resolveCostModelIngest   extractStateRef
  }
