{- |
Module      : DbSync.Checkpoint.Manager
Description : Orchestrates the atomic epoch-boundary commit.

'commitEpoch' is the one entry point the main ingestion pipeline
calls at each epoch boundary. It sequences three pieces of work so
that the system can safely crash and resume at any time:

  1. Drain and commit every COPY connection owned by the
     'CopyWriter' — all data rows for the epoch flush to PG.
  2. Update the 'dbsync_sync_state' singleton on the dedicated
     'ControlConnection' — @last_committed_slot@ and the counters
     advance.
  3. Reopen the COPY streams so the next epoch can start writing.

The ordering matters: if step 2 fails after step 1 succeeds, we are
left with “rows present past @last_committed_slot@”. Boot flow Path B
(Phase 6) handles this by @DELETE …FROM block WHERE slot_no > s@ on
restart, so the invariant “@last_committed_slot@ is never ahead of
actual data” holds by construction.

See @LEDGER-PLAN.md §6@ Invariant I1 for the full atomicity model,
and §14.2 for how this fits into the Phase 1 deliverable.
-}
module DbSync.Checkpoint.Manager
  ( commitEpoch
  ) where

import Cardano.Prelude

import DbSync.Copy.Writer (CopyWriter (..))
import DbSync.Ledger.SyncState (ControlConnection, SyncStateRow, writeSyncState)

-- | Perform an atomic epoch-boundary commit.
--
-- Step-by-step:
--
--   1. 'cwCommit' sends the sentinel to every COPY queue, waits for
--      the workers to drain + @endCopy@, and @COMMIT@s each COPY
--      connection.
--   2. 'writeSyncState' UPSERTs the singleton row with the new
--      slot\/hash\/counters on the 'ControlConnection'. This is a
--      single UPDATE, atomic at the server.
--   3. 'cwReopen' starts new @BEGIN@ + @COPY FROM STDIN@ streams so
--      the main pipeline can write the next epoch's rows.
--
-- Failure semantics:
--
--   * If step 1 throws, no sync-state update happens. On restart,
--     Path B reads the stale sync state and processing resumes from
--     the previous epoch (some rows may need deleting — Path B
--     handles it).
--   * If step 2 throws, data is in PG past @last_committed_slot@.
--     Path B's @DELETE FROM block WHERE slot_no > s@ removes it on
--     restart. Safe but not free — re-extracts the affected epoch.
--   * If step 3 throws, the sync state is already advanced; the
--     COPY connections are in an unusable state. The caller should
--     treat this as fatal and let the process exit — a clean restart
--     will reopen the streams from scratch.
commitEpoch
  :: HasCallStack
  => CopyWriter
  -> ControlConnection
  -> SyncStateRow
  -> IO ()
commitEpoch cw controlConn newRow = do
  cwCommit cw
  writeSyncState controlConn newRow
  cwReopen cw
