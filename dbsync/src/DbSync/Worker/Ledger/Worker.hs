{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Background thread that drains 'leLedgerQueue'.
--
-- 'MsgForward' applies the block through 'applyBlockAndSnapshot'.
-- 'MsgRollback' walks the checkpoint buffer back with
-- 'loadLedgerAtPoint'; see 'handleRollback' for the deep-rollback case.
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
import DbSync.Error.Render (logThreadExit)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Hooks
-- ---------------------------------------------------------------------------

-- | The per-block operations the worker performs, factored out so tests
-- can stub them without an LSM session. Polymorphic in @blk@ so a stub
-- can use a simpler type.
data WorkerHooks blk = WorkerHooks
  { whGetSlotDetails   :: !(blk -> IO SlotDetails)
  , whApplyAndSnapshot :: !(blk -> SlotDetails -> IO ApplyResult)
  }

-- | 'applyBlockAndSnapshot' takes the live 'SyncPhase' on every apply,
-- so a phase transition alone flips the snapshot cadence.
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

-- | Production worker entry point.
--
-- 'StateQueryVar' lives on 'IngestEnv', not 'LedgerEnv', so the caller
-- pairs them and calls
-- @runAppM env (runLedgerWorker mReplayBoundary sqv)@.
runLedgerWorker
  :: Maybe SlotNo
  -> StateQueryVar
  -> LedgerM ()
runLedgerWorker mReplayBoundary sqv = do
  env <- ask
  liftIO $ chainSyncWorkerLoop env (realWorkerHooks env sqv mReplayBoundary)

-- | Build the per-message handlers, then drain the queue through
-- 'chainSyncDispatchLoop'.
chainSyncWorkerLoop
  :: LedgerEnv
  -> WorkerHooks (CardanoBlock StandardCrypto)
  -> IO ()
chainSyncWorkerLoop env hooks = do
  traceWith (leTracer env) $ LogMsg Info "LedgerWorker"
    "starting (draining ledger queue)"
  chainSyncDispatchLoop
    (Just (leTracer env))
    (applyForward env hooks)
    (handleRollback env)
    (leLedgerQueue env)

-- | Production forward handler. Applies the block; the two TMVar
-- writes feed 'leEpochReady' \/ 'leEpochWait', which nothing reads.
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

-- | Walks the checkpoint buffer back to the target on the common
-- shallow case.
--
-- A deeper rollback is out of the buffer's reach, so this records the
-- target on @dbsync_sync_state.pending_rollback_slot@ and exits. The
-- next boot reads the marker and runs the cascade and snapshot cleanup
-- from a usable on-disk snapshot.
handleRollback :: LedgerEnv -> CardanoPoint -> IO ()
handleRollback env p = do
  result <- runAppM env (loadLedgerAtPoint p)
  case result of
    Right _ ->
      traceWith (leTracer env) $ LogMsg Info "LedgerWorker"
        ("rolled back to " <> show p)
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
        )
      throwLedger $
        "chain rollback to slot " <> show targetSlot
          <> " crosses the k-safe rollback boundary "
          <> "(target precedes the oldest state in the in-memory ledger buffer); "
          <> "recorded in dbsync_sync_state.pending_rollback_slot; "
          <> "the next dbsync restart will replay it from a disk snapshot"

-- | 'ChainSyncMsg' dispatch loop. Tests pass stub handlers to exercise
-- the dispatch without an LSM session.
--
-- A crash logs at 'Error' and re-throws, so the supervising 'Async'
-- propagates the failure.
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

-- | Worker loop parameterised by the per-block hooks. Tests inject a
-- fake apply hook and pass 'Nothing' for the tracer to stay quiet.
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

      -- Both TMVars are vestigial: nothing reads 'epochReady' and
      -- nothing writes 'epochWait'. Kept non-blocking so neither can
      -- stall the worker.
      case apNewEpoch result of
        SMaybe.Just ne -> do
          _ <- atomically $ Strict.tryPutTMVar epochReady (Generic.neEpoch ne)
          pure ()
        SMaybe.Nothing -> pure ()

      _ <- atomically $ Strict.tryReadTMVar epochWait
      pure ()
{-# SCC runLedgerWorkerWith #-}

-- | Run the ledger worker and snapshot-writer asyncs around the inner
-- action, and cancel both when it exits or raises. A no-op when the
-- ledger feature is disabled.
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
