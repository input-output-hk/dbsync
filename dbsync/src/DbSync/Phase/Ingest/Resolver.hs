{-# LANGUAGE OverloadedStrings #-}

-- | Ingest-phase ID resolver.
--
-- Uses 'DedupStores' (LSM-backed) and 'IdCounters' ('IORef'-backed)
-- to assign IDs during 'IngestChainHistory'. No live database
-- queries on the hot path; dedup state lives in the shared
-- 'LsmSession' and counter state in an 'IORef' on 'ExtractState'.
--
-- Dedup operations ('resolveSlotLeader', 'resolveMultiAsset', etc.)
-- are direct IO operations on the LSM tables. Non-dedup counter
-- operations use 'atomicModifyIORef'' on 'ExtractState'.
module DbSync.Phase.Ingest.Resolver
  ( -- * Construction
    mkIngestResolver
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', readIORef)
import qualified Data.Map.Strict as Map

import qualified Data.ByteString.Short as SBS

import DbSync.Db.Schema.Ids
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounter, IdCounters (..), nextId)
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert)
import DbSync.Phase.Ingest.UtxoStore (UtxoStore)
import qualified DbSync.Phase.Ingest.UtxoStore as UtxoStore
import DbSync.Resolver (IdResolver (..))
import DbSync.Worker.TxOut.AddressBuffer
  ( AddressBufferRef
  , recordCollateralTxOut
  , recordTxOut
  )
import DbSync.Worker.TxOut.ConsumedByBuffer (ConsumedByBufferRef, recordConsumedBy)

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Build an 'IdResolver' for 'IngestChainHistory'.
--
-- Dedup operations look keys up in the LSM-backed 'DedupStores'
-- and allocate the next id from the in-process counter on miss.
-- Non-dedup counter operations use 'atomicModifyIORef'' on
-- 'ExtractState'.
--
-- 'ByteString' keys from the blockchain are converted to
-- 'ShortByteString' at this boundary so the dedup-store keys stay
-- unpinned. Extractors and the 'IdResolver' interface remain
-- 'ByteString'-based.
--
-- @recordTxOutAddress@\/@recordCollateralTxOutAddress@ append to
-- the per-epoch 'AddressBufferRef'; the background 'AddressResolver'
-- worker reads the buffer one epoch later, writes the @address@ rows,
-- and fills in @tx_out.address_id@\/@collateral_tx_out.address_id@.
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
  { -- Core shared IDs
    assignBlockId = atomicModifyIORef' stRef $ \st ->
      let (bid, ctr') = nextId (icBlockId (esIdCounters st))
          st' = st
            { esIdCounters  = (esIdCounters st) { icBlockId = ctr' }
            , esLastBlockId = Just bid
            }
      in (st', BlockId bid)

  , assignTxId    = bump icTxId    (\cs c -> cs { icTxId    = c }) TxId
  , assignTxOutId = bump icTxOutId (\cs c -> cs { icTxOutId = c }) TxOutId

    -- Dedup: SlotLeader
  , resolveSlotLeader = \hash _leader -> do
      let !key = SBS.toShort hash
      (slId, isNew) <- lookupOrInsert key (dstSlotLeader dedupStores)
      pure (SlotLeaderId slId, isNew)

  , resolvePrevBlock = \_ -> do
      st <- readIORef stRef
      pure (BlockId <$> esLastBlockId st)

    -- Address: queue raw bytes + derived fields for the worker.
  , recordTxOutAddress           = recordTxOut addrBufRef
  , recordCollateralTxOutAddress = recordCollateralTxOut addrBufRef

    -- Follow-only entry point. Ingest extractors must record via the
    -- async worker so @tx_out.address_id@ is filled in one bulk UPDATE
    -- an epoch later rather than per-row.
  , resolveAddressId = \_ _ ->
      panic "Phase.Ingest.Resolver: resolveAddressId is Follow-only; use recordTxOutAddress"

    -- UTxO IDs
  , assignTxInId            = bump icTxInId            (\cs c -> cs { icTxInId            = c }) TxInId
  , assignCollateralTxInId  = bump icCollateralTxInId  (\cs c -> cs { icCollateralTxInId  = c }) CollateralTxInId
  , assignCollateralTxOutId = bump icCollateralTxOutId (\cs c -> cs { icCollateralTxOutId = c }) CollateralTxOutId
  , assignReferenceTxInId   = bump icReferenceTxInId   (\cs c -> cs { icReferenceTxInId   = c }) ReferenceTxInId

    -- Metadata IDs
  , assignTxMetadataId = bump icTxMetadataId (\cs c -> cs { icTxMetadataId = c }) TxMetadataId

    -- Dedup: MultiAsset
    -- Key arrives as ShortByteString (already unpinned) from the extractor.
  , resolveMultiAsset = \skey _ma -> do
      (maId, isNew) <- lookupOrInsert skey (dstMultiAsset dedupStores)
      pure (MultiAssetId maId, isNew)

  , assignMaTxMintId = bump icMaTxMintId (\cs c -> cs { icMaTxMintId = c }) MaTxMintId
  , assignMaTxOutId  = bump icMaTxOutId  (\cs c -> cs { icMaTxOutId  = c }) MaTxOutId

    -- Dedup: StakeAddress
  , resolveStakeAddress = \hash _sa -> do
      let !key = SBS.toShort hash
      (saId, isNew) <- lookupOrInsert key (dstStakeAddress dedupStores)
      pure (StakeAddressId saId, isNew)

  , assignStakeRegistrationId   = bump icStakeRegistrationId   (\cs c -> cs { icStakeRegistrationId   = c }) StakeRegistrationId
  , assignStakeDeregistrationId = bump icStakeDeregistrationId (\cs c -> cs { icStakeDeregistrationId = c }) StakeDeregistrationId
  , assignDelegationId          = bump icDelegationId          (\cs c -> cs { icDelegationId          = c }) DelegationId
  , assignWithdrawalId          = bump icWithdrawalId          (\cs c -> cs { icWithdrawalId          = c }) WithdrawalId

    -- Dedup: PoolHash
  , resolvePoolHash = \hash _ph -> do
      let !key = SBS.toShort hash
      (phId, isNew) <- lookupOrInsert key (dstPoolHash dedupStores)
      pure (PoolHashId phId, isNew)

  , assignPoolUpdateId      = bump icPoolUpdateId      (\cs c -> cs { icPoolUpdateId      = c }) PoolUpdateId
  , assignPoolMetadataRefId = bump icPoolMetadataRefId (\cs c -> cs { icPoolMetadataRefId = c }) PoolMetadataRefId
  , assignPoolOwnerId       = bump icPoolOwnerId       (\cs c -> cs { icPoolOwnerId       = c }) PoolOwnerId
  , assignPoolRetireId      = bump icPoolRetireId      (\cs c -> cs { icPoolRetireId      = c }) PoolRetireId
  , assignPoolRelayId       = bump icPoolRelayId       (\cs c -> cs { icPoolRelayId       = c }) PoolRelayId

    -- CBOR IDs
  , assignTxCborId = bump icTxCborId (\cs c -> cs { icTxCborId = c }) TxCborId

    -- EpochSyncStats IDs
  , assignEpochSyncStatsId = bump icEpochSyncStatsId (\cs c -> cs { icEpochSyncStatsId = c }) EpochSyncStatsId

    -- EpochBoundary IDs
  , assignAdaPotsId     = bump icAdaPotsId     (\cs c -> cs { icAdaPotsId     = c }) AdaPotsId
  , assignEpochParamId  = bump icEpochParamId  (\cs c -> cs { icEpochParamId  = c }) EpochParamId
  , assignEpochStateId  = bump icEpochStateId  (\cs c -> cs { icEpochStateId  = c }) EpochStateId
  , assignPotTransferId = bump icPotTransferId (\cs c -> cs { icPotTransferId = c }) PotTransferId
  , assignTreasuryId    = bump icTreasuryId    (\cs c -> cs { icTreasuryId    = c }) TreasuryId
  , assignReserveId     = bump icReserveId     (\cs c -> cs { icReserveId     = c }) ReserveId

    -- Dedup: cost_model. Cache lives in 'ExtractState' so resume
    -- pre-population from the database surfaces here without
    -- threading a separate IORef through the constructor.
  , resolveCostModel = \hash _cm -> atomicModifyIORef' stRef $ \st ->
      case Map.lookup hash (esCostModelCache st) of
        Just existing -> (st, (CostModelId existing, False))
        Nothing ->
          let (i, ctr') = nextId (icCostModelId (esIdCounters st))
              st' = st
                { esIdCounters     = (esIdCounters st) { icCostModelId = ctr' }
                , esCostModelCache = Map.insert hash i (esCostModelCache st)
                }
          in (st', (CostModelId i, True))

    -- UTxO lookups consult the in-process cache. A miss returns
    -- 'Nothing' and the row is written with @tx_out_id = NULL@; the
    -- post-load resolve handles the residual on cache-miss inputs.
  , resolveInputValues = \pairs ->
      forM pairs $ \(hash, idx) -> do
        m <- UtxoStore.lookupInput utxoStore hash idx
        pure (fmap (\(_, _, v) -> v) m)

  , resolveInputUtxo = UtxoStore.lookupInput utxoStore

  , recordTxOutputs = UtxoStore.recordTx utxoStore

  , recordConsumed = case mConsumedByBuf of
      Just ref -> recordConsumedBy ref
      Nothing  -> \_ _ -> pure ()

  , deleteCachedUtxo = UtxoStore.deleteConsumed utxoStore
  }
  where
    -- | Atomically allocate the next id from the supplied counter
    -- field and wrap it with the matching newtype constructor.
    -- The setter takes the existing 'IdCounters' first, then the new
    -- 'IdCounter', so per-field setters read as
    -- @\\cs c -> cs { fieldName = c }@ — matching the visual order
    -- of a record update.
    bump
      :: (IdCounters -> IdCounter)
      -> (IdCounters -> IdCounter -> IdCounters)
      -> (Int64 -> a)
      -> IO a
    bump getCtr setCtr wrapId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (getCtr (esIdCounters st))
          st' = st { esIdCounters = setCtr (esIdCounters st) ctr' }
      in (st', wrapId i)
