-- | Snapshot-derived interpreter seeding for 'StateQueryVar'.
--
-- 'hardForkSummary' over a loaded ledger state yields the same
-- 'Interpreter' the node returns for @GetInterpreter@, so seeding it
-- keeps 'getSlotDetails' local. The summary only reaches the
-- snapshot's tip; later slots fall through to the node.
module DbSync.StateQuery.Seed
  ( seedInterpreterFromLedgerState
  , isInterpreterCached
  ) where

import Cardano.Prelude hiding (atomically)

import Control.Concurrent.STM (atomically, readTVar, writeTVar)

import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.HardFork.Abstract (hardForkSummary)
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))

import DbSync.StateQuery.Types (HasStateQueryVar (..), StateQueryVar (..))

-- | Fill 'sqvInterpreterVar' from a ledger state's hard-fork summary.
-- A later seed simply overwrites the cell.
seedInterpreterFromLedgerState
  :: TopLevelConfig (CardanoBlock StandardCrypto)
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> StateQueryVar
  -> IO ()
seedInterpreterFromLedgerState topLevelCfg ExtLedgerState{ ledgerState = ls } sqv = do
  let summary = hardForkSummary (configLedger topLevelCfg) ls
      interp  = History.mkInterpreter summary
  atomically $ writeTVar (sqvInterpreterVar sqv) (Just interp)

-- | True once 'sqvInterpreterVar' holds an interpreter. Callers use
-- it to suppress misleading observed-summary diagnostics.
isInterpreterCached
  :: (HasStateQueryVar env, MonadReader env m, MonadIO m)
  => m Bool
isInterpreterCached = do
  sqv <- asks getStateQueryVar
  liftIO $ isJust <$> atomically (readTVar (sqvInterpreterVar sqv))
