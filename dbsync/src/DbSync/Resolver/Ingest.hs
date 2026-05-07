{-# LANGUAGE OverloadedStrings #-}

-- | Ingest-phase ID resolver.
--
-- Uses mutable 'DedupMaps' and 'IdCounters' to assign IDs in-memory
-- during 'IngestChainHistory'. No database queries -- all state is
-- either in mutable hash tables (dedup maps) or an 'IORef' (counters).
--
-- Dedup operations ('resolveSlotLeader', 'resolveMultiAsset', etc.)
-- are direct IO mutations on the hash tables -- no CAS loop, no
-- path-copying. Non-dedup counter operations use 'atomicModifyIORef''
-- on 'ExtractState'.
module DbSync.Resolver.Ingest
  ( -- * Construction
    mkIngestResolver
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', readIORef)

import qualified Data.ByteString.Short as SBS

import DbSync.Db.Schema.Ids
import DbSync.Extractor (ExtractState (..))
import DbSync.Id.Counter (IdCounters (..), nextId)
import DbSync.Id.DedupMap (DedupMaps (..), lookupOrInsert)
import DbSync.Resolver (IdResolver (..))

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Build an 'IdResolver' for 'IngestChainHistory'.
--
-- Dedup operations mutate the mutable hash tables in 'DedupMaps'
-- directly (zero GC pressure, zero path-copying). Non-dedup counter
-- operations use 'atomicModifyIORef'' on 'ExtractState'.
--
-- 'ByteString' keys from the blockchain are converted to
-- 'ShortByteString' at the boundary -- this is the only place the
-- conversion happens. Extractors and the 'IdResolver' interface
-- remain 'ByteString'-based.
mkIngestResolver :: IORef ExtractState -> DedupMaps -> IdResolver IO
mkIngestResolver stRef dedupMaps = IdResolver
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

    -- Dedup: SlotLeader -- direct IO mutation, no atomicModifyIORef'
  , resolveSlotLeader = \hash _leader -> do
      let !key = SBS.toShort hash
      (slId, isNew) <- lookupOrInsert key (dmsSlotLeader dedupMaps)
      pure (SlotLeaderId slId, isNew)

  , resolvePrevBlock = \_ -> do
      st <- readIORef stRef
      pure $ BlockId <$> esLastBlockId st

    -- Dedup: Address — direct IO mutation
  , resolveAddress = \rawBytes _addr -> do
      let !key = SBS.toShort rawBytes
      (aid, isNew) <- lookupOrInsert key (dmsAddress dedupMaps)
      pure (AddressId aid, isNew)

    -- UTxO IDs
  , assignTxInId = atomicModifyIORef' stRef $ \st ->
      let (iid, ctr') = nextId (icTxInId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icTxInId = ctr' } }
      in (st', TxInId iid)

  , assignCollateralTxInId = atomicModifyIORef' stRef $ \st ->
      let (iid, ctr') = nextId (icCollateralTxInId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icCollateralTxInId = ctr' } }
      in (st', CollateralTxInId iid)

  , assignReferenceTxInId = atomicModifyIORef' stRef $ \st ->
      let (iid, ctr') = nextId (icReferenceTxInId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icReferenceTxInId = ctr' } }
      in (st', ReferenceTxInId iid)

    -- Metadata IDs
  , assignTxMetadataId = atomicModifyIORef' stRef $ \st ->
      let (mid, ctr') = nextId (icTxMetadataId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icTxMetadataId = ctr' } }
      in (st', TxMetadataId mid)

    -- Dedup: MultiAsset -- direct IO mutation
    -- Key arrives as ShortByteString (already unpinned) from the extractor.
  , resolveMultiAsset = \skey _ma -> do
      (maId, isNew) <- lookupOrInsert skey (dmsMultiAsset dedupMaps)
      pure (MultiAssetId maId, isNew)

  , assignMaTxMintId = atomicModifyIORef' stRef $ \st ->
      let (mid, ctr') = nextId (icMaTxMintId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icMaTxMintId = ctr' } }
      in (st', MaTxMintId mid)

  , assignMaTxOutId = atomicModifyIORef' stRef $ \st ->
      let (mid, ctr') = nextId (icMaTxOutId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icMaTxOutId = ctr' } }
      in (st', MaTxOutId mid)

    -- Dedup: StakeAddress -- direct IO mutation
  , resolveStakeAddress = \hash _sa -> do
      let !key = SBS.toShort hash
      (saId, isNew) <- lookupOrInsert key (dmsStakeAddress dedupMaps)
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

    -- Dedup: PoolHash -- direct IO mutation
  , resolvePoolHash = \hash _ph -> do
      let !key = SBS.toShort hash
      (phId, isNew) <- lookupOrInsert key (dmsPoolHash dedupMaps)
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
  }
