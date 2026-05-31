-- | Locally-observed Cardano hard-fork summary — STM updater.
--
-- The ChainSync receiver calls 'observeBlockSTM' once per delivered
-- block. This keeps the observed summary in 'StateQueryVar' in sync
-- with the chain history we've seen, so 'DbSync.StateQuery' can answer
-- 'SlotDetails' queries locally without a node round-trip while the
-- node's LedgerDB is still replaying.
module DbSync.StateQuery.Observe
  ( observeBlockSTM
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (readTVar, writeTVar)

import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import DbSync.StateQuery.ObservedSummary (ObservationResult, observeBlock)
import DbSync.StateQuery.Types (StateQueryVar (..))

-- | Atomically feed a block to the locally-observed summary.
--
-- Returns the 'ObservationResult' so the caller can trace era
-- transitions. Intended to be called once per block by the consumer,
-- /before/ the corresponding 'DbSync.StateQuery.getSlotDetails' call
-- so that the transition's epoch boundary is in the summary by the
-- time slot details are computed.
observeBlockSTM
  :: StateQueryVar
  -> CardanoBlock StandardCrypto
  -> STM ObservationResult
observeBlockSTM sqv blk = do
  os <- readTVar (sqvObservedVar sqv)
  let (result, os') = observeBlock blk os
  writeTVar (sqvObservedVar sqv) os'
  pure result
