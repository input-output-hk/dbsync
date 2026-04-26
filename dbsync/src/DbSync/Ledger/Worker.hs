-- | Ledger worker thread.
--
-- Runs the ledger state machine in a dedicated thread, applying blocks
-- to maintain the ledger state needed for rewards, stake snapshots,
-- and protocol parameter queries.
module DbSync.Ledger.Worker
  ( -- TODO: startLedgerWorker, LedgerWorkerHandle
  ) where
