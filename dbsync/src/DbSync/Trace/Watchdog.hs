{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Liveness watchdog. Receiver / ledger worker / consumer each bump
-- their own counter per block; a sampler reads them every
-- 'watchdogInterval' and traces deltas plus per-thread note slots.
-- A @(+0)@ delta marks a stalled thread.
--
-- Diagnostic-only: output is 'Debug' severity and the whole subsystem
-- short-circuits to a no-op when the configured minimum severity is
-- above 'Debug'.
module DbSync.Trace.Watchdog
  ( -- * Types
    Watchdog (..)
  , WatchdogState (..)
  , newWatchdog

    -- * Hot-path bumps
  , bumpConsumer
  , bumpWorker
  , bumpReceiver
  , setConsumerNote
  , setWorkerNote
  , setReceiverNote

    -- * Sampler
  , runWatchdog
  , watchdogInterval
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (SlotNo (..))
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TBQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

data Watchdog
  = WatchdogDisabled
  | WatchdogEnabled !WatchdogState

-- | Per-thread counters; writers use 'atomicModifyIORef'' so
-- concurrent bumps never lose updates. The sampler tolerates lossy
-- snapshots — only deltas matter.
data WatchdogState = WatchdogState
  { wsConsumerBlocks :: !(IORef Word64)
  , wsConsumerSlot   :: !(IORef Word64)
  , wsConsumerNote   :: !(IORef Text)
  , wsWorkerBlocks   :: !(IORef Word64)
  , wsWorkerSlot     :: !(IORef Word64)
  , wsWorkerNote     :: !(IORef Text)
  , wsReceiverBlocks :: !(IORef Word64)
  , wsReceiverSlot   :: !(IORef Word64)
  , wsReceiverNote   :: !(IORef Text)
  }

-- | Enabled if the configured minimum severity admits 'Debug'.
newWatchdog :: Severity -> IO Watchdog
newWatchdog minSeverity
  | minSeverity > Debug = pure WatchdogDisabled
  | otherwise =
      fmap WatchdogEnabled $
        WatchdogState
          <$> newIORef 0
          <*> newIORef 0
          <*> newIORef "(start)"
          <*> newIORef 0
          <*> newIORef 0
          <*> newIORef "(start)"
          <*> newIORef 0
          <*> newIORef 0
          <*> newIORef "(start)"

-- ---------------------------------------------------------------------------
-- * Hot-path bumps
-- ---------------------------------------------------------------------------

-- | Record that the consumer has finished processing one block.
bumpConsumer :: Watchdog -> SlotNo -> IO ()
bumpConsumer WatchdogDisabled    _ = pure ()
bumpConsumer (WatchdogEnabled s) (SlotNo slot) = do
  atomicModifyIORef' (wsConsumerBlocks s) $ \ !n -> (n + 1, ())
  writeIORef (wsConsumerSlot s) slot

-- | Record that the ledger worker has finished applying one block.
bumpWorker :: Watchdog -> SlotNo -> IO ()
bumpWorker WatchdogDisabled    _ = pure ()
bumpWorker (WatchdogEnabled s) (SlotNo slot) = do
  atomicModifyIORef' (wsWorkerBlocks s) $ \ !n -> (n + 1, ())
  writeIORef (wsWorkerSlot s) slot

-- | Record that the receiver has accepted one block from the node.
bumpReceiver :: Watchdog -> SlotNo -> IO ()
bumpReceiver WatchdogDisabled    _ = pure ()
bumpReceiver (WatchdogEnabled s) (SlotNo slot) = do
  atomicModifyIORef' (wsReceiverBlocks s) $ \ !n -> (n + 1, ())
  writeIORef (wsReceiverSlot s) slot

-- | Replace the consumer's free-text annotation.
setConsumerNote :: Watchdog -> Text -> IO ()
setConsumerNote WatchdogDisabled    _    = pure ()
setConsumerNote (WatchdogEnabled s) note = writeIORef (wsConsumerNote s) note

-- | Replace the ledger worker's free-text annotation.
setWorkerNote :: Watchdog -> Text -> IO ()
setWorkerNote WatchdogDisabled    _    = pure ()
setWorkerNote (WatchdogEnabled s) note = writeIORef (wsWorkerNote s) note

-- | Replace the receiver's free-text annotation.
setReceiverNote :: Watchdog -> Text -> IO ()
setReceiverNote WatchdogDisabled    _    = pure ()
setReceiverNote (WatchdogEnabled s) note = writeIORef (wsReceiverNote s) note

-- ---------------------------------------------------------------------------
-- * Sampler
-- ---------------------------------------------------------------------------

-- | Sample interval in seconds.
watchdogInterval :: Int
watchdogInterval = 5

-- | Background loop: every 'watchdogInterval' seconds, sample every
-- counter and log a single diagnostic line. Always emits at 'Debug'
-- severity, including the @[STALL]@ variant — the watchdog is a
-- diagnostic tool, not an operator alert.
--
-- When the watchdog is 'WatchdogDisabled' this returns immediately;
-- the caller's surrounding 'withAsync' then has a child that exits
-- straight away, leaving the rest of the orchestration unchanged.
runWatchdog
  :: AppTracer
  -> Watchdog
  -> TBQueue a
  -- ^ block queue (receiver → consumer)
  -> Maybe (TBQueue b)
  -- ^ ledger queue (receiver → worker), or 'Nothing' when ledger disabled
  -> IO ()
runWatchdog _      WatchdogDisabled    _      _        = pure ()
runWatchdog tracer (WatchdogEnabled s) blockQ mLedgerQ = do
  -- Seed: capture the initial counter values so the first interval
  -- reports a real delta rather than the lifetime total.
  initConsumerB <- readIORef (wsConsumerBlocks s)
  initWorkerB   <- readIORef (wsWorkerBlocks s)
  initReceiverB <- readIORef (wsReceiverBlocks s)
  loop initConsumerB initWorkerB initReceiverB
  where
    loop !prevConsumerB !prevWorkerB !prevReceiverB = do
      threadDelay (watchdogInterval * 1_000_000)

      consumerB    <- readIORef (wsConsumerBlocks s)
      consumerS    <- readIORef (wsConsumerSlot s)
      consumerNote <- readIORef (wsConsumerNote s)
      workerB      <- readIORef (wsWorkerBlocks s)
      workerS      <- readIORef (wsWorkerSlot s)
      workerNote   <- readIORef (wsWorkerNote s)
      receiverB    <- readIORef (wsReceiverBlocks s)
      receiverS    <- readIORef (wsReceiverSlot s)
      receiverNote <- readIORef (wsReceiverNote s)

      qBlock   <- atomically $ STM.lengthTBQueue blockQ
      qLedger  <- traverse (atomically . STM.lengthTBQueue) mLedgerQ

      let dConsumer = consumerB - prevConsumerB
          dWorker   = workerB   - prevWorkerB
          dReceiver = receiverB - prevReceiverB
          stalled   = dConsumer == 0 || dWorker == 0 || dReceiver == 0
          marker    = if stalled then " [STALL]" else ""

          renderQ :: Text -> Maybe Natural -> Text
          renderQ name Nothing  = name <> "=-"
          renderQ name (Just n) = name <> "=" <> show n

          renderThread :: Text -> Word64 -> Word64 -> Word64 -> Text -> Text
          renderThread name slot blocks delta note =
            name <> "=slot:" <> show slot
              <> " blk:" <> show blocks
              <> " (+" <> show delta <> ")"
              <> " note=" <> note

          msg =
            renderThread "recv" receiverS receiverB dReceiver receiverNote
              <> " | "
              <> renderThread "worker" workerS workerB dWorker workerNote
              <> " | "
              <> renderThread "consumer" consumerS consumerB dConsumer consumerNote
              <> " | qd "
              <> renderQ "block" (Just qBlock)
              <> " "
              <> renderQ "ledger" qLedger
              <> marker

      traceWith tracer $ LogMsg Debug "Watchdog" msg Nothing

      loop consumerB workerB receiverB
