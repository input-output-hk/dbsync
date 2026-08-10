-- | STM updater for the locally-observed hard-fork summary.
module DbSync.StateQuery.Observe
  ( observeBlockSTM
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (readTVar, writeTVar)

import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import DbSync.StateQuery.ObservedSummary (ObservationResult, observeBlock)
import DbSync.StateQuery.Types (StateQueryVar (..))

-- | Feed one block to the observed summary. The caller must call this
-- /before/ the matching 'DbSync.StateQuery.getSlotDetails', so a new
-- era boundary is already in the summary when the slot is resolved.
-- The 'ObservationResult' lets the caller trace era transitions.
observeBlockSTM
  :: StateQueryVar
  -> CardanoBlock StandardCrypto
  -> STM ObservationResult
observeBlockSTM sqv blk = do
  os <- readTVar (sqvObservedVar sqv)
  let (result, os') = observeBlock blk os
  writeTVar (sqvObservedVar sqv) os'
  pure result
