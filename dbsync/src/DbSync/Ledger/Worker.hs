{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : DbSync.Ledger.Worker
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
module DbSync.Ledger.Worker
  ( -- * Entry points
    runLedgerWorker
  , runLedgerWorkerWith

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
import DbSync.Block.Types (CardanoPoint)
import DbSync.Checkpoint.SyncState (writePendingRollbackSlot)
import DbSync.Error (throwLedger)
import DbSync.Phase.Type (isFollowPath)
import qualified DbSync.Ledger.EpochUpdate as Generic
import DbSync.Ledger.State
  ( applyBlockAndSnapshot
  , getTopLevelConfig
  , loadLedgerAtPoint
  , readCurrentStateUnsafe
  )
import DbSync.Ledger.Types (ApplyResult (..), LedgerEnv (..))
import DbSync.Node.ChainSyncMsg (ChainSyncMsg (..))
import DbSync.Phase.Current (readCurrentPhase)
import DbSync.StateQuery
  ( SlotDetails
  , StateQueryVar
  , getSlotDetailsIO
  , seedInterpreterFromLedgerState
  )
import DbSync.StateQuery.Types (sdSlotNo)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..), logThreadExit)
import DbSync.Trace.Watchdog (Watchdog, bumpWorker, setWorkerNote)

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
  , whApplyAndSnapshot :: !(blk -> SlotDetails -> IO (ApplyResult, Bool))
  }

-- | Build the production hook set from a 'LedgerEnv', a
-- 'StateQueryVar', a 'Watchdog' handle, and the optional resume
-- replay boundary.
--
-- The /consistent with tip/ flag passed to 'applyBlockAndSnapshot'
-- is derived from the shared 'CurrentPhase' on every apply, so the
-- orchestrator can flip the snapshot cadence (Ingest = every 10
-- epochs, Follow = every epoch) just by transitioning the phase.
--
-- The 'Watchdog' note is stamped around each hook call so a hang
-- inside @applyBlockAndSnapshot@ or @seedInterpreterFromLedgerState@
-- is visible in the watchdog log line.
realWorkerHooks
  :: LedgerEnv
  -> StateQueryVar
  -> Watchdog
  -> Maybe SlotNo
  -> WorkerHooks (CardanoBlock StandardCrypto)
realWorkerHooks env sqv wd mReplayBoundary =
  WorkerHooks
    { whGetSlotDetails = \blk -> do
        setWorkerNote wd "worker: getSlotDetailsIO"
        getSlotDetailsIO (leTracer env) sqv (leSystemStart env) (blockSlot blk)
    , whApplyAndSnapshot = \blk sd -> do
        setWorkerNote wd "worker: applyBlockAndSnapshot"
        consistent <- isFollowPath <$> readCurrentPhase (leCurrentPhase env)
        result <- runAppM env (applyBlockAndSnapshot blk sd consistent mReplayBoundary)
        -- Re-seed the cached HFC interpreter from the post-apply state so
        -- the next getSlotDetailsIO stays inside the summary's horizon.
        setWorkerNote wd "worker: readCurrentStateUnsafe (re-seed)"
        newState <- runAppM env readCurrentStateUnsafe
        setWorkerNote wd "worker: seedInterpreterFromLedgerState"
        seedInterpreterFromLedgerState (getTopLevelConfig env) newState sqv
        setWorkerNote wd "worker: post-apply"
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
-- 'StateQueryVar' and 'Watchdog' live on 'IngestEnv' rather than
-- 'LedgerEnv', so the caller pairs them up and invokes
-- @runAppM env (runLedgerWorker mReplayBoundary sqv wd)@.
runLedgerWorker
  :: Maybe SlotNo
  -> StateQueryVar
  -> Watchdog
  -> LedgerM ()
runLedgerWorker mReplayBoundary sqv wd = do
  env <- ask
  liftIO $ chainSyncWorkerLoop env (realWorkerHooks env sqv wd mReplayBoundary) wd

-- | Production loop: build per-message handlers from the LSM-backed
-- 'LedgerEnv' and the block 'WorkerHooks', then drain the queue via
-- the generic 'chainSyncDispatchLoop'.
chainSyncWorkerLoop
  :: LedgerEnv
  -> WorkerHooks (CardanoBlock StandardCrypto)
  -> Watchdog
  -> IO ()
chainSyncWorkerLoop env hooks wd = do
  traceWith (leTracer env) $ LogMsg Info "LedgerWorker"
    "starting (draining ledger queue)" Nothing
  chainSyncDispatchLoop
    (Just (leTracer env))
    (applyForward env hooks wd)
    (handleRollback env wd)
    (Just wd)
    (leLedgerQueue env)

-- | Production forward handler: apply the block, bump the watchdog,
-- signal epoch boundaries, and clear any pending epoch-wait flag.
applyForward
  :: LedgerEnv
  -> WorkerHooks (CardanoBlock StandardCrypto)
  -> Watchdog
  -> CardanoBlock StandardCrypto
  -> IO ()
applyForward env hooks wd blk = do
  sd <- whGetSlotDetails hooks blk
  (result, _tookSnap) <- whApplyAndSnapshot hooks blk sd
  bumpWorker wd (sdSlotNo sd)
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
handleRollback :: LedgerEnv -> Watchdog -> CardanoPoint -> IO ()
handleRollback env wd p = do
  setWorkerNote wd "worker: rollback (loadLedgerAtPoint)"
  result <- runAppM env (loadLedgerAtPoint p)
  case result of
    Right _ -> do
      traceWith (leTracer env) $ LogMsg Info "LedgerWorker"
        ("rolled back to " <> show p) Nothing
      bumpWorker wd (pointSlotNo p)
    Left _ -> do
      let SlotNo targetSlot = pointSlotNo p
      runAppM env (writePendingRollbackSlot targetSlot)
      traceWith (leTracer env) $ LogMsg Error "LedgerWorker"
        ( "rollback target " <> show p
            <> " is past the in-memory buffer; marker written to "
            <> "dbsync_sync_state.pending_rollback_slot = "
            <> show targetSlot
            <> ". Restarting dbsync will replay the rollback from a "
            <> "disk snapshot."
        ) Nothing
      throwLedger $
        "rollback to slot " <> show targetSlot
          <> " is past the in-memory buffer; recovery deferred to next boot"

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
  -> Maybe Watchdog
  -> TBQueue ChainSyncMsg
  -> IO ()
chainSyncDispatchLoop mTracer forwardH rollbackH mWatchdog queue =
  loop `catch` \(e :: SomeException) -> do
    for_ mTracer (logThreadExit "LedgerWorker" e)
    throwIO e
  where
    loop = forever $ do
      for_ mWatchdog $ \wd -> setWorkerNote wd "worker: readTBQueue (waiting for message)"
      msg <- atomically $ readTBQueue queue
      case msg of
        MsgForward  blk -> forwardH blk
        MsgRollback p   -> rollbackH p

-- | Slot of a 'CardanoPoint' for the watchdog bump. 'Origin'
-- (genesis) bumps with slot 0 — the watchdog only tracks monotonic
-- progress, not absolute values.
pointSlotNo :: CardanoPoint -> SlotNo
pointSlotNo p = case pointSlot p of
  Origin -> SlotNo 0
  At s   -> s

-- | Generic worker loop, parameterised by the per-block hooks. Used
-- by tests to inject a fake apply hook and exercise the coordination
-- primitives without an LSM session.
--
-- Any exception thrown by the loop is logged (when a tracer is
-- supplied) at 'Error' severity and re-thrown so the supervising
-- 'Async' propagates the failure. Tests pass 'Nothing' to keep the
-- output quiet.
--
-- When a 'Watchdog' handle is supplied, the worker bumps the
-- per-thread liveness counter after each successful apply. Tests
-- pass 'Nothing' so the loop runs unwatched.
runLedgerWorkerWith
  :: Maybe AppTracer
  -> WorkerHooks blk
  -> Maybe Watchdog
  -> TBQueue blk
  -> Strict.StrictTMVar IO EpochNo                   -- ^ epochReady (out)
  -> Strict.StrictTMVar IO EpochNo                   -- ^ epochWait  (in)
  -> IO ()
runLedgerWorkerWith mTracer hooks mWatchdog queue epochReady epochWait =
  loop `catch` \(e :: SomeException) -> do
    for_ mTracer (logThreadExit "LedgerWorker" e)
    throwIO e
  where
    loop = forever $ do
      for_ mWatchdog $ \wd -> setWorkerNote wd "worker: readTBQueue (waiting for block)"
      blk <- atomically $ readTBQueue queue
      sd  <- whGetSlotDetails hooks blk
      (result, _tookSnap) <- whApplyAndSnapshot hooks blk sd

      -- Watchdog bump: one applied block.
      for_ mWatchdog $ \wd -> bumpWorker wd (sdSlotNo sd)

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
