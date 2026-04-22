{-# LANGUAGE OverloadedStrings #-}

-- | Ingest-phase ID resolver.
--
-- Uses DedupMaps and IdCounters to assign IDs in-memory during
-- 'IngestChainHistory'. No database queries — all pure state
-- wrapped in 'IORef' for the 'IO' interface.
module DbSync.Resolver.Ingest
  ( -- * Construction
    mkIngestResolver
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', readIORef)

import DbSync.Db.Schema.Core (SlotLeader)
import DbSync.Db.Schema.MultiAsset (MultiAsset)
import DbSync.Db.Schema.StakeDelegation (StakeAddress)
import DbSync.Db.Schema.Pool (PoolHash)
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
-- All ID assignment uses in-memory counters and DedupMaps stored
-- in the given 'IORef'. No database queries are performed.
mkIngestResolver :: IORef ExtractState -> IdResolver IO
mkIngestResolver stRef = IdResolver
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

  , resolveSlotLeader = \hash _leader -> atomicModifyIORef' stRef $ \st ->
      let (slId, isNew, dedupMap') =
            lookupOrInsert hash (dmsSlotLeader $ esDedupMaps st)
          st' = st { esDedupMaps = (esDedupMaps st) { dmsSlotLeader = dedupMap' } }
      in (st', (SlotLeaderId slId, isNew))

  , resolvePrevBlock = \_ -> do
      st <- readIORef stRef
      pure $ BlockId <$> esLastBlockId st

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

    -- MultiAsset IDs
  , resolveMultiAsset = \key _ma -> atomicModifyIORef' stRef $ \st ->
      let (maId, isNew, dedupMap') =
            lookupOrInsert key (dmsMultiAsset $ esDedupMaps st)
          st' = st { esDedupMaps = (esDedupMaps st) { dmsMultiAsset = dedupMap' } }
      in (st', (MultiAssetId maId, isNew))

  , assignMaTxMintId = atomicModifyIORef' stRef $ \st ->
      let (mid, ctr') = nextId (icMaTxMintId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icMaTxMintId = ctr' } }
      in (st', MaTxMintId mid)

  , assignMaTxOutId = atomicModifyIORef' stRef $ \st ->
      let (mid, ctr') = nextId (icMaTxOutId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icMaTxOutId = ctr' } }
      in (st', MaTxOutId mid)

    -- StakeDelegation IDs
  , resolveStakeAddress = \hash _sa -> atomicModifyIORef' stRef $ \st ->
      let (saId, isNew, dedupMap') =
            lookupOrInsert hash (dmsStakeAddress $ esDedupMaps st)
          st' = st { esDedupMaps = (esDedupMaps st) { dmsStakeAddress = dedupMap' } }
      in (st', (StakeAddressId saId, isNew))

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

    -- Pool IDs
  , resolvePoolHash = \hash _ph -> atomicModifyIORef' stRef $ \st ->
      let (phId, isNew, dedupMap') =
            lookupOrInsert hash (dmsPoolHash $ esDedupMaps st)
          st' = st { esDedupMaps = (esDedupMaps st) { dmsPoolHash = dedupMap' } }
      in (st', (PoolHashId phId, isNew))

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
  }
