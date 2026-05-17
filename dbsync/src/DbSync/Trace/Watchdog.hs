{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Liveness watchdog. Receiver / ledger worker / consumer each bump
-- their own counter per block; a sampler reads them every
-- 'watchdogInterval' and traces deltas plus per-thread note slots.
--
-- The per-sample line traces at 'Debug'. When a thread fails to
-- advance for 'stallWarnThreshold' consecutive intervals the sampler
-- escalates to one 'Warning' line per stuck thread so the alert is
-- visible against the surrounding Debug noise.
--
-- Diagnostic-only: the whole subsystem short-circuits to a no-op when
-- the configured minimum severity is above 'Debug', so the per-block
-- bumps cost nothing in production-default 'Info' setups.
module DbSync.Trace.Watchdog
  ( -- * Types
    Watchdog (..)
  , WatchdogState (..)
  , HasWatchdog (..)
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
  , runWatchdogIO
  , watchdogInterval
  , stallWarnThreshold
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (SlotNo (..))
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TBQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

data Watchdog
  = WatchdogDisabled
  | WatchdogEnabled !WatchdogState

-- | Access the watchdog from env. Implemented by 'IngestEnv' and
-- 'FollowEnv'.
class HasWatchdog env where
  getWatchdog :: env -> Watchdog

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
-- The watchdog is opt-in diagnostics; at 'Info' or above the
-- per-block bumps and the sampler thread are pure overhead, so the
-- whole subsystem short-circuits to a no-op.
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

-- | How many consecutive zero-delta samples count as a stall worth
-- a 'Warning'. Three intervals at 'watchdogInterval' = 5s is 15s of
-- no progress on a thread — long enough to be confident it isn't
-- just a slow block.
stallWarnThreshold :: Int
stallWarnThreshold = 3

-- | Per-thread sampler iteration state: previous block count plus
-- the current streak of consecutive zero-delta samples.
data ThreadSample = ThreadSample
  { tsPrevBlocks :: !Word64
  , tsStreak     :: !Int
  }

-- | Result of advancing one thread's sample by one interval.
data ThreadAdvance = ThreadAdvance
  { taNext    :: !ThreadSample
  , taDelta   :: !Word64
  , taCrossed :: !Bool
    -- ^ 'True' on the iteration that pushes the streak from below to
    -- at-or-above 'stallWarnThreshold'. Only this iteration emits a
    -- 'Warning'; later iterations on the same stuck-streak stay
    -- silent so the operator gets one alert per stuck-period, not a
    -- repeating flood.
  }

advanceThread :: ThreadSample -> Word64 -> ThreadAdvance
advanceThread ts curBlocks =
  ThreadAdvance
    { taNext    = ThreadSample { tsPrevBlocks = curBlocks, tsStreak = streak' }
    , taDelta   = delta
    , taCrossed = tsStreak ts < stallWarnThreshold
                    && streak' >= stallWarnThreshold
    }
  where
    !delta   = curBlocks - tsPrevBlocks ts
    !streak' = if delta == 0 then tsStreak ts + 1 else 0

-- | Background loop: every 'watchdogInterval' seconds, sample every
-- counter and log one Debug context line. Per-thread stall streaks
-- escalate to a 'Warning' once they cross 'stallWarnThreshold'.
--
-- When the watchdog is 'WatchdogDisabled' this returns immediately;
-- the caller's surrounding 'withAsync' then has a child that exits
-- straight away, leaving the rest of the orchestration unchanged.
runWatchdog
  :: (HasTracer env, HasWatchdog env, MonadReader env m, MonadIO m)
  => TBQueue a
  -- ^ block queue (receiver → consumer)
  -> Maybe (TBQueue b)
  -- ^ ledger queue (receiver → worker), or 'Nothing' when ledger disabled
  -> m ()
runWatchdog blockQ mLedgerQ = do
  tracer  <- asks getTracer
  wd      <- asks getWatchdog
  liftIO (runWatchdogIO tracer wd blockQ mLedgerQ)

-- | Bare-IO entry point. The polymorphic 'runWatchdog' delegates to
-- this; direct callers should prefer the polymorphic one.
runWatchdogIO
  :: AppTracer
  -> Watchdog
  -> TBQueue a
  -> Maybe (TBQueue b)
  -> IO ()
runWatchdogIO _      WatchdogDisabled    _      _        = pure ()
runWatchdogIO tracer (WatchdogEnabled s) blockQ mLedgerQ = do
  -- Seed: capture the initial counter values so the first interval
  -- reports a real delta rather than the lifetime total.
  initConsumerB <- readIORef (wsConsumerBlocks s)
  initWorkerB   <- readIORef (wsWorkerBlocks s)
  initReceiverB <- readIORef (wsReceiverBlocks s)
  let seed b = ThreadSample { tsPrevBlocks = b, tsStreak = 0 }
  loop (seed initConsumerB) (seed initWorkerB) (seed initReceiverB)
  where
    loop !consumerPrev !workerPrev !receiverPrev = do
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

      let consumerAdv = advanceThread consumerPrev consumerB
          workerAdv   = advanceThread workerPrev   workerB
          receiverAdv = advanceThread receiverPrev receiverB

          dConsumer = taDelta consumerAdv
          dWorker   = taDelta workerAdv
          dReceiver = taDelta receiverAdv

          anyStalled = dConsumer == 0 || dWorker == 0 || dReceiver == 0
          marker     = if anyStalled then " [STALL]" else ""

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

      when (taCrossed receiverAdv) $
        emitStallWarning "receiver" receiverS receiverNote
      when (taCrossed workerAdv) $
        emitStallWarning "ledger worker" workerS workerNote
      when (taCrossed consumerAdv) $
        emitStallWarning "consumer" consumerS consumerNote

      loop (taNext consumerAdv) (taNext workerAdv) (taNext receiverAdv)

    emitStallWarning :: Text -> Word64 -> Text -> IO ()
    emitStallWarning threadName slot note =
      traceWith tracer $ LogMsg Warning "Watchdog"
        ( threadName <> " has not advanced in "
            <> show (stallWarnThreshold * watchdogInterval) <> "s"
            <> " (last slot " <> show slot
            <> ", note=" <> note <> ")"
        ) Nothing
