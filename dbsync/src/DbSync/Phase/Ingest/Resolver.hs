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

import qualified Data.ByteString.Short as SBS

import DbSync.Db.Schema.Ids
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..), nextId)
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
      let (bid, ctr') = nextId (icBlockId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icBlockId = ctr' }
                    , esLastBlockId = Just bid
                    }
      in (st', BlockId bid)

  , assignTxId = atomicModifyIORef' stRef $ \st ->
      let (tid, ctr') = nextId (icTxId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icTxId = ctr' } }
      in (st', TxId tid)

  , assignTxOutId = atomicModifyIORef' stRef $ \st ->
      let (oid, ctr') = nextId (icTxOutId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icTxOutId = ctr' } }
      in (st', TxOutId oid)

    -- Dedup: SlotLeader
  , resolveSlotLeader = \hash _leader -> do
      let !key = SBS.toShort hash
      (slId, isNew) <- lookupOrInsert key (dstSlotLeader dedupStores)
      pure (SlotLeaderId slId, isNew)

  , resolvePrevBlock = \_ -> do
      st <- readIORef stRef
      pure $ BlockId <$> esLastBlockId st

    -- Address: queue raw bytes + derived fields for the worker.
  , recordTxOutAddress = recordTxOut addrBufRef
  , recordCollateralTxOutAddress = recordCollateralTxOut addrBufRef

    -- Follow-only entry point. Ingest extractors must record via the
    -- async worker so @tx_out.address_id@ is filled in one bulk UPDATE
    -- an epoch later rather than per-row.
  , resolveAddressId = \_ _ ->
      panic "Phase.Ingest.Resolver: resolveAddressId is Follow-only; use recordTxOutAddress"

    -- UTxO IDs
  , assignTxInId = atomicModifyIORef' stRef $ \st ->
      let (iid, ctr') = nextId (icTxInId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icTxInId = ctr' } }
      in (st', TxInId iid)

  , assignCollateralTxInId = atomicModifyIORef' stRef $ \st ->
      let (iid, ctr') = nextId (icCollateralTxInId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icCollateralTxInId = ctr' } }
      in (st', CollateralTxInId iid)

  , assignCollateralTxOutId = atomicModifyIORef' stRef $ \st ->
      let (iid, ctr') = nextId (icCollateralTxOutId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icCollateralTxOutId = ctr' } }
      in (st', CollateralTxOutId iid)

  , assignReferenceTxInId = atomicModifyIORef' stRef $ \st ->
      let (iid, ctr') = nextId (icReferenceTxInId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icReferenceTxInId = ctr' } }
      in (st', ReferenceTxInId iid)

    -- Metadata IDs
  , assignTxMetadataId = atomicModifyIORef' stRef $ \st ->
      let (mid, ctr') = nextId (icTxMetadataId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icTxMetadataId = ctr' } }
      in (st', TxMetadataId mid)

    -- Dedup: MultiAsset
    -- Key arrives as ShortByteString (already unpinned) from the extractor.
  , resolveMultiAsset = \skey _ma -> do
      (maId, isNew) <- lookupOrInsert skey (dstMultiAsset dedupStores)
      pure (MultiAssetId maId, isNew)

  , assignMaTxMintId = atomicModifyIORef' stRef $ \st ->
      let (mid, ctr') = nextId (icMaTxMintId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icMaTxMintId = ctr' } }
      in (st', MaTxMintId mid)

  , assignMaTxOutId = atomicModifyIORef' stRef $ \st ->
      let (mid, ctr') = nextId (icMaTxOutId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icMaTxOutId = ctr' } }
      in (st', MaTxOutId mid)

    -- Dedup: StakeAddress
  , resolveStakeAddress = \hash _sa -> do
      let !key = SBS.toShort hash
      (saId, isNew) <- lookupOrInsert key (dstStakeAddress dedupStores)
      pure (StakeAddressId saId, isNew)

  , assignStakeRegistrationId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icStakeRegistrationId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icStakeRegistrationId = ctr' } }
      in (st', StakeRegistrationId i)

  , assignStakeDeregistrationId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icStakeDeregistrationId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icStakeDeregistrationId = ctr' } }
      in (st', StakeDeregistrationId i)

  , assignDelegationId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icDelegationId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icDelegationId = ctr' } }
      in (st', DelegationId i)

  , assignWithdrawalId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icWithdrawalId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icWithdrawalId = ctr' } }
      in (st', WithdrawalId i)

    -- Dedup: PoolHash
  , resolvePoolHash = \hash _ph -> do
      let !key = SBS.toShort hash
      (phId, isNew) <- lookupOrInsert key (dstPoolHash dedupStores)
      pure (PoolHashId phId, isNew)

  , assignPoolUpdateId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icPoolUpdateId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icPoolUpdateId = ctr' } }
      in (st', PoolUpdateId i)

  , assignPoolMetadataRefId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icPoolMetadataRefId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icPoolMetadataRefId = ctr' } }
      in (st', PoolMetadataRefId i)

  , assignPoolOwnerId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icPoolOwnerId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icPoolOwnerId = ctr' } }
      in (st', PoolOwnerId i)

  , assignPoolRetireId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icPoolRetireId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icPoolRetireId = ctr' } }
      in (st', PoolRetireId i)

  , assignPoolRelayId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icPoolRelayId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icPoolRelayId = ctr' } }
      in (st', PoolRelayId i)

    -- CBOR IDs
  , assignTxCborId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icTxCborId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icTxCborId = ctr' } }
      in (st', TxCborId i)

    -- EpochSyncStats IDs
  , assignEpochSyncStatsId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icEpochSyncStatsId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icEpochSyncStatsId = ctr' } }
      in (st', EpochSyncStatsId i)

    -- EpochBoundary IDs
  , assignAdaPotsId = atomicModifyIORef' stRef $ \st ->
      let (i, ctr') = nextId (icAdaPotsId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icAdaPotsId = ctr' } }
      in (st', AdaPotsId i)

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
