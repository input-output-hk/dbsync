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
mkIngestResolver stRef dedupStores addrBufRef utxoStore mConsumedByBuf = IdResolver
  { -- Core
    assignBlockId     = Core.assignBlockIdIngest     stRef
  , assignTxId        = Core.assignTxIdIngest        stRef
  , assignTxOutId     = Core.assignTxOutIdIngest     stRef
  , resolveSlotLeader = Core.resolveSlotLeaderIngest dedupStores
  , resolvePrevBlock  = Core.resolvePrevBlockIngest  stRef

    -- UTxO
  , recordTxOutAddress           = UTxO.recordTxOutAddressIngest           addrBufRef
  , recordCollateralTxOutAddress = UTxO.recordCollateralTxOutAddressIngest addrBufRef
  , resolveAddressId             = UTxO.resolveAddressIdIngest
  , assignTxInId                 = UTxO.assignTxInIdIngest                 stRef
  , assignCollateralTxInId       = UTxO.assignCollateralTxInIdIngest       stRef
  , assignCollateralTxOutId      = UTxO.assignCollateralTxOutIdIngest      stRef
  , assignReferenceTxInId        = UTxO.assignReferenceTxInIdIngest        stRef
  , resolveInputValues           = UTxO.resolveInputValuesIngest           utxoStore
  , resolveInputUtxo             = UTxO.resolveInputUtxoIngest             utxoStore
  , recordTxOutputs              = UTxO.recordTxOutputsIngest              utxoStore
  , recordConsumed               = UTxO.recordConsumedIngest               mConsumedByBuf
  , deleteCachedUtxo             = UTxO.deleteCachedUtxoIngest             utxoStore

    -- Metadata
  , assignTxMetadataId = Metadata.assignTxMetadataIdIngest stRef

    -- MultiAsset
  , resolveMultiAsset = MultiAsset.resolveMultiAssetIngest dedupStores
  , assignMaTxMintId  = MultiAsset.assignMaTxMintIdIngest  stRef
  , assignMaTxOutId   = MultiAsset.assignMaTxOutIdIngest   stRef

    -- StakeDelegation (incl. pot rebalancing)
  , resolveStakeAddress         = Stake.resolveStakeAddressIngest         dedupStores
  , assignStakeRegistrationId   = Stake.assignStakeRegistrationIdIngest   stRef
  , assignStakeDeregistrationId = Stake.assignStakeDeregistrationIdIngest stRef
  , assignDelegationId          = Stake.assignDelegationIdIngest          stRef
  , assignWithdrawalId          = Stake.assignWithdrawalIdIngest          stRef
  , assignPotTransferId         = Stake.assignPotTransferIdIngest         stRef
  , assignTreasuryId            = Stake.assignTreasuryIdIngest            stRef
  , assignReserveId             = Stake.assignReserveIdIngest             stRef

    -- Pool
  , resolvePoolHash         = Pool.resolvePoolHashIngest         dedupStores
  , assignPoolUpdateId      = Pool.assignPoolUpdateIdIngest      stRef
  , assignPoolMetadataRefId = Pool.assignPoolMetadataRefIdIngest stRef
  , assignPoolOwnerId       = Pool.assignPoolOwnerIdIngest       stRef
  , assignPoolRetireId      = Pool.assignPoolRetireIdIngest      stRef
  , assignPoolRelayId       = Pool.assignPoolRelayIdIngest       stRef

    -- CBOR
  , assignTxCborId = Cbor.assignTxCborIdIngest stRef

    -- EpochSyncStats
  , assignEpochSyncStatsId = Epoch.assignEpochSyncStatsIdIngest stRef

    -- EpochBoundary
  , assignAdaPotsId    = EpochBoundary.assignAdaPotsIdIngest    stRef
  , assignEpochParamId = EpochBoundary.assignEpochParamIdIngest stRef
  , assignEpochStateId = EpochBoundary.assignEpochStateIdIngest stRef
  , resolveCostModel   = EpochBoundary.resolveCostModelIngest   stRef
  }
