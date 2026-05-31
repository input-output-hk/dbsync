-- | Snapshot-derived interpreter seeding for 'StateQueryVar'.
--
-- At boot the ledger worker loads its most-recent on-disk snapshot.
-- 'hardForkSummary' applied to that snapshot produces exactly the
-- 'Interpreter' the node would return via the LSQ @GetInterpreter@
-- query, so seeding it here lets per-block 'getSlotDetails' calls
-- serve locally from the start instead of round-tripping the node.
--
-- The summary only covers eras up to the snapshot's tip; queries past
-- its horizon fall through to the node fallback in
-- 'DbSync.StateQuery.getSlotDetailsIOWith' as before.
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

-- | Pre-fill 'sqvInterpreterVar' from a loaded ledger state's
-- hard-fork summary. Idempotent: a subsequent seed just overwrites
-- the cell.
seedInterpreterFromLedgerState
  :: TopLevelConfig (CardanoBlock StandardCrypto)
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> StateQueryVar
  -> IO ()
seedInterpreterFromLedgerState topLevelCfg ExtLedgerState{ ledgerState = ls } sqv = do
  let summary = hardForkSummary (configLedger topLevelCfg) ls
      interp  = History.mkInterpreter summary
  atomically $ writeTVar (sqvInterpreterVar sqv) (Just interp)

-- | True when 'sqvInterpreterVar' has been seeded (snapshot or node).
-- Lets callers suppress observed-summary fallback diagnostics that
-- would otherwise mislead.
isInterpreterCached
  :: (HasStateQueryVar env, MonadReader env m, MonadIO m)
  => m Bool
isInterpreterCached = do
  sqv <- asks getStateQueryVar
  liftIO $ isJust <$> atomically (readTVar (sqvInterpreterVar sqv))
