{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : DbSync.Worker.Ledger.Worker
Description : Background thread that drains the ledger queue.

Reads 'ChainSyncMsg' values off 'leLedgerQueue':

  * 'MsgForward' — apply the block via 'applyBlockAndSnapshot', write
    the latest 'ApplyResult' into @leLatestApplyResult@, and signal
    epoch boundaries via 'leEpochReady'.
  * 'MsgRollback' — call 'loadLedgerAtPoint' to walk the in-memory
    buffer back to the target. Rollbacks deeper than the buffer
    (~100 blocks) panic with an operator-actionable message — the
    recovery path is to restart dbsync so the disk snapshot can be
    reloaded at the rollback point.

== Hook-based factoring

'runLedgerWorkerWith' separates the queue-draining loop from the
LSM-backed apply call. Tests use it directly with stub hooks to
exercise the coordination primitives without an LSM session.
Production goes through 'runLedgerWorker', which dispatches forward
and rollback messages around 'realWorkerHooks'.
-}
module DbSync.Worker.Ledger.Worker
  ( -- * Entry points
    runLedgerWorker
  , runLedgerWorkerWith
  , withLedgerThreads

    -- * Test hooks
  , WorkerHooks (..)
  , realWorkerHooks
  , chainSyncDispatchLoop
  ) where

import Cardano.Prelude

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Concurrent.STM (TBQueue, readTBQueue)
import qualified Data.Strict.Maybe as SMaybe

import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..))
import Control.Tracer (traceWith)
import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Shelley.HFEras ()                  -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol () -- LedgerSupportsProtocol orphans
import Ouroboros.Network.Block (pointSlot)

import DbSync.AppM (LedgerM, runAppM)
import DbSync.Parser.Types (CardanoPoint)
import DbSync.SyncState.Row (writePendingRollbackSlot)
import DbSync.Error (throwLedger)

import qualified DbSync.Worker.Ledger.EpochUpdate as Generic
import DbSync.Worker.Ledger.Snapshot (runLedgerStateWriteThread)
import DbSync.Worker.Ledger.State
  ( applyBlockAndSnapshot
  , getTopLevelConfig
  , loadLedgerAtPoint
  , readCurrentStateUnsafe
  )
import DbSync.Worker.Ledger.Types (ApplyResult (..), HasLedgerEnv (..), LedgerEnv (..))
import DbSync.ChainSync.Msg (ChainSyncMsg (..))
import DbSync.Phase.Current (readCurrentPhase)
import DbSync.StateQuery
  ( SlotDetails
  , StateQueryVar
  , getSlotDetailsIO
  , seedInterpreterFromLedgerState
  )
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..), logThreadExit)

-- ---------------------------------------------------------------------------
-- * Hooks
-- ---------------------------------------------------------------------------

-- | The per-block operations the worker performs, factored out so
-- tests can stub them without an LSM session. The production loop
-- ('runLedgerWorker') wraps these in a 'ChainSyncMsg' dispatcher
-- that also handles 'MsgRollback'.
--
-- Polymorphic in @blk@ so test stubs can use simpler types.
data WorkerHooks blk = WorkerHooks
  { whGetSlotDetails   :: !(blk -> IO SlotDetails)
  , whApplyAndSnapshot :: !(blk -> SlotDetails -> IO ApplyResult)
  }

-- | Build the production hook set from a 'LedgerEnv', a
-- 'StateQueryVar', and the optional resume replay boundary.
--
-- 'applyBlockAndSnapshot' receives the live 'SyncPhase' on every
-- apply so the orchestrator can flip the snapshot cadence (Ingest =
-- every 10 epochs, Follow = every epoch) just by transitioning the
-- phase.
realWorkerHooks
  :: LedgerEnv
  -> StateQueryVar
  -> Maybe SlotNo
  -> WorkerHooks (CardanoBlock StandardCrypto)
realWorkerHooks env sqv mReplayBoundary =
  WorkerHooks
    { whGetSlotDetails = \blk ->
        getSlotDetailsIO (leTracer env) sqv (leSystemStart env) (blockSlot blk)
    , whApplyAndSnapshot = \blk sd -> do
        phase <- readCurrentPhase (leCurrentPhase env)
        result <- runAppM env (applyBlockAndSnapshot blk sd phase mReplayBoundary)
        -- Re-seed the cached HFC interpreter from the post-apply state so
        -- the next getSlotDetailsIO stays inside the summary's horizon.
        newState <- runAppM env readCurrentStateUnsafe
        seedInterpreterFromLedgerState (getTopLevelConfig env) newState sqv
        pure result
    }

-- ---------------------------------------------------------------------------
-- * Entry points
-- ---------------------------------------------------------------------------

-- | Production worker entry point. Drains 'leLedgerQueue' and
-- dispatches each message: 'MsgForward' goes through the block
-- hooks; 'MsgRollback' walks the in-memory buffer via
-- 'loadLedgerAtPoint'.
--
-- 'StateQueryVar' lives on 'IngestEnv' rather than 'LedgerEnv', so
-- the caller pairs them up and invokes
-- @runAppM env (runLedgerWorker mReplayBoundary sqv)@.
runLedgerWorker
  :: Maybe SlotNo
  -> StateQueryVar
  -> LedgerM ()
runLedgerWorker mReplayBoundary sqv = do
  env <- ask
  liftIO $ chainSyncWorkerLoop env (realWorkerHooks env sqv mReplayBoundary)

-- | Production loop: build per-message handlers from the LSM-backed
-- 'LedgerEnv' and the block 'WorkerHooks', then drain the queue via
-- the generic 'chainSyncDispatchLoop'.
chainSyncWorkerLoop
  :: LedgerEnv
  -> WorkerHooks (CardanoBlock StandardCrypto)
  -> IO ()
chainSyncWorkerLoop env hooks = do
  traceWith (leTracer env) $ LogMsg Info "LedgerWorker"
    "starting (draining ledger queue)" Nothing
  chainSyncDispatchLoop
    (Just (leTracer env))
    (applyForward env hooks)
    (handleRollback env)
    (leLedgerQueue env)

-- | Production forward handler: apply the block, signal epoch
-- boundaries, and clear any pending epoch-wait flag.
applyForward
  :: LedgerEnv
  -> WorkerHooks (CardanoBlock StandardCrypto)
  -> CardanoBlock StandardCrypto
  -> IO ()
applyForward env hooks blk = do
  sd <- whGetSlotDetails hooks blk
  result <- whApplyAndSnapshot hooks blk sd
  case apNewEpoch result of
    SMaybe.Just ne -> do
      _ <- atomically $ Strict.tryPutTMVar (leEpochReady env) (Generic.neEpoch ne)
      pure ()
    SMaybe.Nothing -> pure ()
  _ <- atomically $ Strict.tryReadTMVar (leEpochWait env)
  pure ()

-- | Production rollback handler. Walks the in-memory buffer back to
-- the target on the common shallow case; on a deeper rollback the
-- buffer can't reach the target, so we persist the target on
-- @dbsync_sync_state.pending_rollback_slot@ and exit. The next boot
-- sees the marker and runs the cascade + snapshot cleanup from a
-- usable on-disk snapshot.
handleRollback :: LedgerEnv -> CardanoPoint -> IO ()
handleRollback env p = do
  result <- runAppM env (loadLedgerAtPoint p)
  case result of
    Right _ ->
      traceWith (leTracer env) $ LogMsg Info "LedgerWorker"
        ("rolled back to " <> show p) Nothing
    Left _ -> do
      let targetSlot = case pointSlot p of
            Origin        -> 0
            At (SlotNo s) -> s
      runAppM env (writePendingRollbackSlot targetSlot)
      traceWith (leTracer env) $ LogMsg Error "LedgerWorker"
        ( "chain rollback to " <> show p
            <> " crosses the k-safe rollback boundary "
            <> "(target precedes the oldest state in the in-memory ledger buffer). "
            <> "Recorded in dbsync_sync_state.pending_rollback_slot = "
            <> show targetSlot
            <> "; the next dbsync restart will replay the rollback from a disk snapshot."
        ) Nothing
      throwLedger $
        "chain rollback to slot " <> show targetSlot
          <> " crosses the k-safe rollback boundary "
          <> "(target precedes the oldest state in the in-memory ledger buffer); "
          <> "recorded in dbsync_sync_state.pending_rollback_slot; "
          <> "the next dbsync restart will replay it from a disk snapshot"

-- | Generic ChainSyncMsg dispatch loop. Production wires real
-- handlers ('applyForward', 'handleRollback') around this; tests
-- pass stubs to exercise the dispatch without an LSM session.
--
-- Crashes are logged at 'Error' severity (when a tracer is supplied)
-- and re-thrown so the supervising 'Async' propagates the failure.
chainSyncDispatchLoop
  :: Maybe AppTracer
  -> (CardanoBlock StandardCrypto -> IO ())
  -> (CardanoPoint -> IO ())
  -> TBQueue ChainSyncMsg
  -> IO ()
chainSyncDispatchLoop mTracer forwardH rollbackH queue =
  loop `catch` \(e :: SomeException) -> do
    for_ mTracer (logThreadExit "LedgerWorker" e)
    throwIO e
  where
    loop = forever $ do
      msg <- atomically $ readTBQueue queue
      case msg of
        MsgForward  blk -> forwardH blk
        MsgRollback p   -> rollbackH p

-- | Generic worker loop, parameterised by the per-block hooks. Used
-- by tests to inject a fake apply hook and exercise the coordination
-- primitives without an LSM session.
--
-- Any exception thrown by the loop is logged (when a tracer is
-- supplied) at 'Error' severity and re-thrown so the supervising
-- 'Async' propagates the failure. Tests pass 'Nothing' to keep the
-- output quiet.
runLedgerWorkerWith
  :: Maybe AppTracer
  -> WorkerHooks blk
  -> TBQueue blk
  -> Strict.StrictTMVar IO EpochNo                   -- ^ epochReady (out)
  -> Strict.StrictTMVar IO EpochNo                   -- ^ epochWait  (in)
  -> IO ()
runLedgerWorkerWith mTracer hooks queue epochReady epochWait =
  loop `catch` \(e :: SomeException) -> do
    for_ mTracer (logThreadExit "LedgerWorker" e)
    throwIO e
  where
    loop = forever $ do
      blk <- atomically $ readTBQueue queue
      sd  <- whGetSlotDetails hooks blk
      result <- whApplyAndSnapshot hooks blk sd

      -- Signal epoch boundary if the apply call detected one.
      case apNewEpoch result of
        SMaybe.Just ne -> do
          -- 'tryPutTMVar': non-blocking, so the worker doesn't stall
          -- when the main thread hasn't drained a previous signal yet.
          _ <- atomically $ Strict.tryPutTMVar epochReady (Generic.neEpoch ne)
          pure ()
        SMaybe.Nothing -> pure ()

      -- 'epochWait' is the transition signal — non-blocking here.
      _ <- atomically $ Strict.tryReadTMVar epochWait
      pure ()
{-# SCC runLedgerWorkerWith #-}

-- | Run the ledger worker + snapshot-writer asyncs for the duration
-- of the inner action. No-op when the ledger feature is disabled.
--
-- Cancellation propagates to both async children when the inner
-- action exits or raises.
withLedgerThreads
  :: HasLedgerEnv
  -> Maybe SlotNo
  -> StateQueryVar
  -> IO a
  -> IO a
withLedgerThreads (LedgerDisabled _) _ _ inner = inner
withLedgerThreads hasLE@(LedgerEnabled lenv) replayBoundary sqv inner =
  withAsync (runAppM lenv (runLedgerWorker replayBoundary sqv)) $ \w -> do
    link w
    withAsync (runLedgerStateWriteThread hasLE) $ \s -> do
      link s
      inner
