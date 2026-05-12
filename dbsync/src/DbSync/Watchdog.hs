{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Liveness watchdog for the 'IngestChainHistory' pipeline.
--
-- Each of the three hot-path threads — receiver, ledger worker, and
-- consumer — bumps a cheap counter (single 'atomicModifyIORef'') as
-- it advances through a block. A background sampler reads those
-- counters every 'watchdogInterval' seconds and emits a single
-- diagnostic line:
--
-- @
-- recv=slot:50312000 blk:83205 (+1311) note=ok | worker=slot:50312000 blk:83205 (+1311) note=applyBlockAndSnapshot | consumer=slot:50284917 blk:83000 (+0) note=processBlock [STALL] | qd block=0 ledger=4 applied=0
-- @
--
-- When any of @recv@, @worker@, @consumer@ shows @(+0)@ — no
-- progress since the previous tick — the line is emitted at
-- 'Warning' severity with a @[STALL]@ marker so the stuck thread is
-- immediately visible.
--
-- Each thread has its own free-text note slot. Producers stamp the
-- last hook they entered (e.g. @"applyBlockAndSnapshot"@,
-- @"awaitDrained"@), and the watchdog renders all three side-by-side
-- so a coordinated stall shows /where each thread/ stopped, not just
-- whoever wrote the note most recently.
--
-- Per-block overhead is one IORef write per counter; the sampler
-- runs at human-readable cadence (default 5 s) so it imposes no
-- measurable cost on the hot path.
module DbSync.Watchdog
  ( -- * Types
    Watchdog (..)
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

-- | Per-thread liveness counters. Writers use 'atomicModifyIORef''
-- so concurrent bumps never lose updates; the sampler uses
-- 'readIORef' (lossy snapshot is fine — we only care about the
-- delta between ticks).
data Watchdog = Watchdog
  { wdConsumerBlocks :: !(IORef Word64)
  , wdConsumerSlot   :: !(IORef Word64)
  , wdConsumerNote   :: !(IORef Text)
  , wdWorkerBlocks   :: !(IORef Word64)
  , wdWorkerSlot     :: !(IORef Word64)
  , wdWorkerNote     :: !(IORef Text)
  , wdReceiverBlocks :: !(IORef Word64)
  , wdReceiverSlot   :: !(IORef Word64)
  , wdReceiverNote   :: !(IORef Text)
    -- ^ Free-text annotation set by producers at each call-site of
    -- interest (e.g. @"applyBlockAndSnapshot"@). Each thread owns its
    -- own note ref so a coordinated stall reveals /per-thread/ stuck
    -- locations, not just whichever thread wrote last.
  }

-- | Allocate a zeroed watchdog. Call once at startup.
newWatchdog :: IO Watchdog
newWatchdog =
  Watchdog
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
bumpConsumer wd (SlotNo s) = do
  atomicModifyIORef' (wdConsumerBlocks wd) $ \ !n -> (n + 1, ())
  writeIORef (wdConsumerSlot wd) s

-- | Record that the ledger worker has finished applying one block.
bumpWorker :: Watchdog -> SlotNo -> IO ()
bumpWorker wd (SlotNo s) = do
  atomicModifyIORef' (wdWorkerBlocks wd) $ \ !n -> (n + 1, ())
  writeIORef (wdWorkerSlot wd) s

-- | Record that the receiver has accepted one block from the node.
bumpReceiver :: Watchdog -> SlotNo -> IO ()
bumpReceiver wd (SlotNo s) = do
  atomicModifyIORef' (wdReceiverBlocks wd) $ \ !n -> (n + 1, ())
  writeIORef (wdReceiverSlot wd) s

-- | Replace the consumer's free-text annotation.
setConsumerNote :: Watchdog -> Text -> IO ()
setConsumerNote wd = writeIORef (wdConsumerNote wd)

-- | Replace the ledger worker's free-text annotation.
setWorkerNote :: Watchdog -> Text -> IO ()
setWorkerNote wd = writeIORef (wdWorkerNote wd)

-- | Replace the receiver's free-text annotation.
setReceiverNote :: Watchdog -> Text -> IO ()
setReceiverNote wd = writeIORef (wdReceiverNote wd)

-- ---------------------------------------------------------------------------
-- * Sampler
-- ---------------------------------------------------------------------------

-- | Sample interval in seconds.
watchdogInterval :: Int
watchdogInterval = 5

-- | Background loop: every 'watchdogInterval' seconds, sample every
-- counter and log a single diagnostic line. Any counter that has
-- not moved since the previous tick is annotated @(+0)@ and the
-- whole line is upgraded to 'Warning' severity with a @[STALL]@
-- marker.
--
-- The block queue is mandatory; the ledger / applied queues are
-- only present when the ledger feature is enabled.
runWatchdog
  :: AppTracer
  -> Watchdog
  -> TBQueue a
  -- ^ block queue (receiver → consumer)
  -> Maybe (TBQueue b)
  -- ^ ledger queue (receiver → worker), or 'Nothing' when ledger disabled
  -> Maybe (TBQueue c)
  -- ^ applied queue (worker → consumer), or 'Nothing' when ledger disabled
  -> IO ()
runWatchdog tracer wd blockQ mLedgerQ mAppliedQ = do
  -- Seed: capture the initial counter values so the first interval
  -- reports a real delta rather than the lifetime total.
  initConsumerB <- readIORef (wdConsumerBlocks wd)
  initWorkerB   <- readIORef (wdWorkerBlocks wd)
  initReceiverB <- readIORef (wdReceiverBlocks wd)
  loop initConsumerB initWorkerB initReceiverB
  where
    loop !prevConsumerB !prevWorkerB !prevReceiverB = do
      threadDelay (watchdogInterval * 1_000_000)

      consumerB    <- readIORef (wdConsumerBlocks wd)
      consumerS    <- readIORef (wdConsumerSlot wd)
      consumerNote <- readIORef (wdConsumerNote wd)
      workerB      <- readIORef (wdWorkerBlocks wd)
      workerS      <- readIORef (wdWorkerSlot wd)
      workerNote   <- readIORef (wdWorkerNote wd)
      receiverB    <- readIORef (wdReceiverBlocks wd)
      receiverS    <- readIORef (wdReceiverSlot wd)
      receiverNote <- readIORef (wdReceiverNote wd)

      qBlock   <- atomically $ STM.lengthTBQueue blockQ
      qLedger  <- traverse (atomically . STM.lengthTBQueue) mLedgerQ
      qApplied <- traverse (atomically . STM.lengthTBQueue) mAppliedQ

      let dConsumer = consumerB - prevConsumerB
          dWorker   = workerB   - prevWorkerB
          dReceiver = receiverB - prevReceiverB
          stalled   = dConsumer == 0 || dWorker == 0 || dReceiver == 0
          severity  = if stalled then Warning else Info
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
              <> " "
              <> renderQ "applied" qApplied
              <> marker

      traceWith tracer $ LogMsg severity "Watchdog" msg Nothing

      loop consumerB workerB receiverB
