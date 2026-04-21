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
import DbSync.Db.Schema.Ids (BlockId (..), SlotLeaderId (..), TxId (..))
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
--
-- The 'resolvePrevBlock' function ignores the hash and returns
-- the last assigned 'BlockId' (blocks are processed sequentially).
mkIngestResolver :: IORef ExtractState -> IdResolver IO
mkIngestResolver stRef = IdResolver
  { assignBlockId = atomicModifyIORef' stRef $ \st ->
      let (bid, ctr') = nextId (icBlockId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icBlockId = ctr' }
                    , esLastBlockId = Just bid
                    }
      in (st', BlockId bid)

  , assignTxId = atomicModifyIORef' stRef $ \st ->
      let (tid, ctr') = nextId (icTxId $ esIdCounters st)
          st' = st { esIdCounters = (esIdCounters st) { icTxId = ctr' } }
      in (st', TxId tid)

  , resolveSlotLeader = \hash _leader -> atomicModifyIORef' stRef $ \st ->
      let (slId, isNew, dedupMap') =
            lookupOrInsert hash (dmsSlotLeader $ esDedupMaps st)
          st' = st { esDedupMaps = (esDedupMaps st) { dmsSlotLeader = dedupMap' } }
      in (st', (SlotLeaderId slId, isNew))

  , resolvePrevBlock = \_ -> do
      st <- readIORef stRef
      pure $ BlockId <$> esLastBlockId st
  }
