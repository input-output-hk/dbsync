{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : DbSync.Ledger.Worker
Description : Background thread that drains the ledger queue and applies blocks.

The 'IngestChainHistory' phase has two parallel block consumers:

  * The /main pipeline/ (parser → extractors → COPY) — consumes
    @ieBlockQueue@.
  * The /ledger worker/ (this module) — consumes 'leLedgerQueue',
    applies each block to the LSM-backed ledger via
    'applyBlockAndSnapshot', writes the latest 'ApplyResult' into
    @leLatestApplyResult@, and signals epoch boundaries via
    'leEpochReady'.

The two consumers are intentionally not lock-stepped: the worker can
fall a few blocks behind without back-pressuring the main pipeline,
and the main pipeline does not block on the worker's progress until
the Ingest→Follow transition (Phase 7).

== Hook-based factoring

'runLedgerWorkerWith' separates the queue-draining loop from the
LSM-backed apply call. The production entry point
'runLedgerWorker' supplies the real hooks; tests can supply a fake
@whApplyAndSnapshot@ to exercise the coordination primitives without
needing an LSM session.
-}
module DbSync.Ledger.Worker
  ( -- * Entry points
    runLedgerWorker
  , runLedgerWorkerWith

    -- * Test hooks
  , WorkerHooks (..)
  , realWorkerHooks
  ) where

import Cardano.Prelude

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Concurrent.STM (TBQueue, readTBQueue)
import qualified Data.Strict.Maybe as SMaybe

import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Tracer (traceWith)
import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Shelley.HFEras ()                  -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol () -- LedgerSupportsProtocol orphans

import DbSync.AppM (LedgerM, runAppM)
import qualified DbSync.Era.Shelley.Generic.EpochUpdate as Generic
import DbSync.Ledger.State (applyBlockAndSnapshot)
import DbSync.Ledger.Types (ApplyResult (..), LedgerEnv (..))
import DbSync.StateQuery (SlotDetails, StateQueryVar, getSlotDetailsIO)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Hooks
-- ---------------------------------------------------------------------------

-- | The two operations the worker performs per block, factored out
-- so tests can stub them. In production these resolve to
-- 'getSlotDetails' and 'applyBlockAndSnapshot' respectively.
--
-- Polymorphic in @blk@ so test stubs can use simpler types
-- ('()' is convenient).
data WorkerHooks blk = WorkerHooks
  { whGetSlotDetails   :: !(blk -> IO SlotDetails)
  , whApplyAndSnapshot :: !(blk -> SlotDetails -> IO (ApplyResult, Bool))
  }

-- | Build the production hook set from a 'LedgerEnv', a
-- 'StateQueryVar', and the optional resume replay boundary.
--
-- The 'consistent' flag passed to 'applyBlockAndSnapshot' is
-- conservatively 'False' during Ingest (we treat the chain tip as
-- far away); Phase 7 will set it 'True' once the receiver knows
-- we're inside @k=2160@ of the node tip.
realWorkerHooks
  :: LedgerEnv
  -> StateQueryVar
  -> Maybe SlotNo
  -> WorkerHooks (CardanoBlock StandardCrypto)
realWorkerHooks env sqv mReplayBoundary =
  WorkerHooks
    { whGetSlotDetails = \blk ->
        getSlotDetailsIO (leTracer env) sqv (leSystemStart env) (blockSlot blk)
    , whApplyAndSnapshot = \blk sd ->
        runAppM env (applyBlockAndSnapshot blk sd False mReplayBoundary)
    }

-- ---------------------------------------------------------------------------
-- * Entry points
-- ---------------------------------------------------------------------------

-- | Production worker entry point — drains 'leLedgerQueue', applies
-- each block to the LSM-backed ledger, and signals 'leEpochReady'
-- on every epoch boundary.
--
-- Runs in 'LedgerM' so the 'LedgerEnv' comes from the 'MonadReader'
-- context. 'StateQueryVar' lives on 'IngestEnv' rather than
-- 'LedgerEnv', so the caller pairs them up and invokes
-- @runAppM env (runLedgerWorker mReplayBoundary sqv)@. The
-- replay boundary is forwarded to 'applyBlockAndSnapshot'.
runLedgerWorker
  :: Maybe SlotNo
  -> StateQueryVar
  -> LedgerM ()
runLedgerWorker mReplayBoundary sqv = do
  env <- ask
  liftIO $ do
    traceWith (leTracer env) $ LogMsg Info "LedgerWorker"
      "starting (draining ledger queue)" Nothing
    runLedgerWorkerWith
      (Just (leTracer env))
      (realWorkerHooks env sqv mReplayBoundary)
      (leLedgerQueue env)
      (leEpochReady env)
      (leEpochWait env)

-- | Generic worker loop, parameterised by the per-block hooks. The
-- production path uses 'realWorkerHooks'; tests inject a fake hook
-- to verify the coordination shape without spinning up an LSM
-- session.
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
    for_ mTracer $ \tracer ->
      traceWith tracer $ LogMsg Error "LedgerWorker"
        ("crashed: " <> show e) Nothing
    throwIO e
  where
    loop = forever $ do
      blk <- atomically $ readTBQueue queue
      sd  <- whGetSlotDetails hooks blk
      (result, _tookSnap) <- whApplyAndSnapshot hooks blk sd

      -- Signal epoch boundary if the apply call detected one.
      case apNewEpoch result of
        SMaybe.Just ne -> do
          -- 'tryPutTMVar': non-blocking, so the worker doesn't stall
          -- when the main thread hasn't drained a previous signal yet.
          _ <- atomically $ Strict.tryPutTMVar epochReady (Generic.neEpoch ne)
          pure ()
        SMaybe.Nothing -> pure ()

      -- 'epochWait' is the Phase 7 transition signal — non-blocking
      -- here. When set, Phase 7 logic will arrange the actual handoff.
      _ <- atomically $ Strict.tryReadTMVar epochWait
      pure ()
{-# SCC runLedgerWorkerWith #-}
