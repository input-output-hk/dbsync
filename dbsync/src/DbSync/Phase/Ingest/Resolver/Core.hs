-- | Ingest 'IdResolver' fragments for the @core@ extractor.
module DbSync.Phase.Ingest.Resolver.Core
  ( assignBlockIdIngest
  , assignTxIdIngest
  , assignTxOutIdIngest
  , resolveSlotLeaderIngest
  , resolvePrevBlockIngest
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, atomicModifyIORef', readIORef)

import DbSync.Db.Schema.Core (SlotLeader)
import DbSync.Db.Schema.Ids
  ( BlockId (..)
  , SlotLeaderId (..)
  , TxId (..)
  , TxOutId (..)
  )
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..), nextId)
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert)
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)

-- | Allocate the next 'BlockId' and stash it in 'esLastBlockId' so a
-- subsequent 'resolvePrevBlockIngest' can answer locally.
assignBlockIdIngest :: IORef ExtractState -> IO BlockId
assignBlockIdIngest extractStateRef = atomicModifyIORef' extractStateRef $ \st ->
  let (rawId, nextCounter) = nextId (icBlockId (esIdCounters st))
      st' = st
        { esIdCounters  = (esIdCounters st) { icBlockId = nextCounter }
        , esLastBlockId = Just rawId
        }
   in (st', BlockId rawId)

assignTxIdIngest :: IORef ExtractState -> IO TxId
assignTxIdIngest extractStateRef =
  allocateNextId extractStateRef icTxId (\cs c -> cs { icTxId = c }) TxId

assignTxOutIdIngest :: IORef ExtractState -> IO TxOutId
assignTxOutIdIngest extractStateRef =
  allocateNextId extractStateRef icTxOutId (\cs c -> cs { icTxOutId = c }) TxOutId

-- | Dedup lookup against the LSM-backed slot-leader table.
resolveSlotLeaderIngest
  :: DedupStores -> ByteString -> SlotLeader -> IO (SlotLeaderId, Bool)
resolveSlotLeaderIngest dedupStores hash _leader = do
  let !key = SBS.toShort hash
  (slId, isNew) <- lookupOrInsert key (dstSlotLeader dedupStores)
  pure (SlotLeaderId slId, isNew)

-- | Read the most-recently-assigned 'BlockId'. The hash argument is
-- ignored in Ingest: there is no fork, and the prior block id is
-- always the consumer's previous step.
resolvePrevBlockIngest :: IORef ExtractState -> ByteString -> IO (Maybe BlockId)
resolvePrevBlockIngest extractStateRef _ = do
  st <- readIORef extractStateRef
  pure (BlockId <$> esLastBlockId st)
