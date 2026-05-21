{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Liveness watchdog. Receiver / ledger worker / consumer each bump
-- their own counter per block; a sampler reads them every
-- 'watchdogInterval' and traces deltas plus per-thread note slots.
--
-- During 'IngestChainHistory' the sampler also reads the consumer's
-- 'PipelineStats' and the receiver's 'ReceiverStats' to surface
-- drain-size averages and the writes-blocked count as interval
-- deltas. These additional samples are 'Nothing' in 'FollowingChainTip'
-- where the COPY pipeline isn't running.
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
  , WatchdogIngestSamples (..)
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

import DbSync.Phase.Ingest.PipelineStats (PipelineStats (..))
import qualified DbSync.Phase.Ingest.ReceiverStats as Recv
import DbSync.Phase.Ingest.ReceiverStats (ReceiverStats)
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

-- | Optional Ingest-only samples surfaced by the watchdog.
-- 'Nothing' in 'FollowingChainTip' (no COPY pipeline running).
data WatchdogIngestSamples = WatchdogIngestSamples
  { wisPipelineStats :: !(IORef PipelineStats)
  , wisReceiverStats :: !ReceiverStats
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
    !delta   = monotonicDelta (tsPrevBlocks ts) curBlocks
    !streak' = if delta == 0 then tsStreak ts + 1 else 0

-- | Previously-seen pipeline / receiver counters carried between
-- sampler iterations so the next iteration can compute interval
-- deltas. Mirrors 'PipelineStats' + the receiver writes-blocked
-- counter.
data PipelineSample = PipelineSample
  { psPrevDrainTotal   :: !Word64
  , psPrevDrainCount   :: !Word64
  , psPrevSingleDrains :: !Word64
  , psPrevFullDrains   :: !Word64
  , psPrevWritesBlocked :: !Word64
  }

emptyPipelineSample :: PipelineSample
emptyPipelineSample = PipelineSample 0 0 0 0 0

-- | Subtract @prev@ from @cur@, treating @cur < prev@ as a fresh
-- counter (return @cur@). Handles the consumer's per-epoch reset of
-- 'PipelineStats' without producing nonsense deltas.
monotonicDelta :: Word64 -> Word64 -> Word64
monotonicDelta prev cur
  | cur >= prev = cur - prev
  | otherwise   = cur

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
  -> Maybe WatchdogIngestSamples
  -- ^ Ingest-only samples; 'Nothing' in Follow phase
  -> m ()
runWatchdog blockQ mLedgerQ mSamples = do
  tracer  <- asks getTracer
  wd      <- asks getWatchdog
  liftIO (runWatchdogIO tracer wd blockQ mLedgerQ mSamples)

-- | Bare-IO entry point. The polymorphic 'runWatchdog' delegates to
-- this; direct callers should prefer the polymorphic one.
runWatchdogIO
  :: AppTracer
  -> Watchdog
  -> TBQueue a
  -> Maybe (TBQueue b)
  -> Maybe WatchdogIngestSamples
  -> IO ()
runWatchdogIO _      WatchdogDisabled    _      _        _        = pure ()
runWatchdogIO tracer (WatchdogEnabled s) blockQ mLedgerQ mSamples = do
  -- Seed: capture the initial counter values so the first interval
  -- reports a real delta rather than the lifetime total.
  initConsumerB <- readIORef (wsConsumerBlocks s)
  initWorkerB   <- readIORef (wsWorkerBlocks s)
  initReceiverB <- readIORef (wsReceiverBlocks s)
  let seed b = ThreadSample { tsPrevBlocks = b, tsStreak = 0 }
  loop (seed initConsumerB) (seed initWorkerB) (seed initReceiverB)
       emptyPipelineSample
  where
    loop !consumerPrev !workerPrev !receiverPrev !pipelinePrev = do
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

      -- Pipeline + receiver samples for the Ingest path. In Follow
      -- mode 'mSamples' is 'Nothing' and we emit only the liveness
      -- fields.
      mPipelineNow <- traverse readPipeline mSamples

      let consumerAdv = advanceThread consumerPrev consumerB
          workerAdv   = advanceThread workerPrev   workerB
          receiverAdv = advanceThread receiverPrev receiverB

          dConsumer = taDelta consumerAdv
          dWorker   = taDelta workerAdv
          dReceiver = taDelta receiverAdv

          anyStalled = dConsumer == 0 || dWorker == 0 || dReceiver == 0
          marker     = if anyStalled then " [STALL]" else ""

          (pipelineSeg, pipelineNext) = case mPipelineNow of
            Nothing -> ("", pipelinePrev)
            Just now ->
              let dBlocked = monotonicDelta (psPrevWritesBlocked pipelinePrev)
                                            (psPrevWritesBlocked now)
                  drainSeg = renderDrain pipelinePrev now
                  blockedSeg
                    | dBlocked == 0 = ""
                    | otherwise = " blocked=+" <> show dBlocked
              in ( blockedSeg <> drainSeg, now )

          -- One tip-slot reference rather than three identical
          -- 'slot:S' fields. The receiver's slot is the leading edge
          -- of what we know about the chain.
          tipSlot = receiverS

          renderQ :: Text -> Maybe Natural -> Text
          renderQ name Nothing  = name <> "=-"
          renderQ name (Just n) = name <> "=" <> show n

          headline =
            "tip=" <> show tipSlot
              <> " | recv +" <> show dReceiver
              <> " | worker +" <> show dWorker
              <> " | consumer +" <> show dConsumer
              <> pipelineSeg
              <> " | qd " <> renderQ "block" (Just qBlock)
              <> " " <> renderQ "ledger" qLedger
              <> marker

          notesLine
            | allStart consumerNote workerNote receiverNote = ""
            | otherwise =
                "\nnotes: recv=\"" <> receiverNote
                  <> "\" worker=\""  <> workerNote
                  <> "\" consumer=\"" <> consumerNote <> "\""

      traceWith tracer $ LogMsg Debug "Watchdog" (headline <> notesLine) Nothing

      when (taCrossed receiverAdv) $
        emitStallWarning "receiver" receiverS receiverNote
      when (taCrossed workerAdv) $
        emitStallWarning "ledger worker" workerS workerNote
      when (taCrossed consumerAdv) $
        emitStallWarning "consumer" consumerS consumerNote

      loop (taNext consumerAdv) (taNext workerAdv) (taNext receiverAdv)
           pipelineNext

    -- Render the consumer-side drain segment as
    -- @ drain avg=A max=M (full=+F single=+S)@. Returns @""@ if no
    -- drains occurred this interval — there's nothing useful to
    -- report.
    renderDrain :: PipelineSample -> PipelineSample -> Text
    renderDrain prev now
      | dCount == 0 = ""
      | otherwise =
          " drain avg=" <> show (dTotal `div` dCount)
            <> " (full=+" <> show dFull
            <> " single=+" <> show dSingle <> ")"
      where
        dTotal  = monotonicDelta (psPrevDrainTotal prev)   (psPrevDrainTotal now)
        dCount  = monotonicDelta (psPrevDrainCount prev)   (psPrevDrainCount now)
        dSingle = monotonicDelta (psPrevSingleDrains prev) (psPrevSingleDrains now)
        dFull   = monotonicDelta (psPrevFullDrains prev)   (psPrevFullDrains now)

    -- Read the current absolute counter values from PipelineStats +
    -- ReceiverStats and stash them in a 'PipelineSample' so the next
    -- iteration can compute a delta.
    readPipeline :: WatchdogIngestSamples -> IO PipelineSample
    readPipeline (WatchdogIngestSamples psRef rs) = do
      ps   <- readIORef psRef
      rsSn <- Recv.readSnapshot rs
      pure PipelineSample
        { psPrevDrainTotal    = psDrainTotal ps
        , psPrevDrainCount    = psDrainCount ps
        , psPrevSingleDrains  = psSingleDrains ps
        , psPrevFullDrains    = psFullDrains ps
        , psPrevWritesBlocked = Recv.snWritesBlocked rsSn
        }

    -- Suppress the notes line when no thread has ever written a real
    -- note. Avoids two near-empty lines at startup before anything
    -- has happened.
    allStart :: Text -> Text -> Text -> Bool
    allStart a b c = a == "(start)" && b == "(start)" && c == "(start)"

    emitStallWarning :: Text -> Word64 -> Text -> IO ()
    emitStallWarning threadName slot note =
      traceWith tracer $ LogMsg Warning "Watchdog"
        ( threadName <> " has not advanced in "
            <> show (stallWarnThreshold * watchdogInterval) <> "s"
            <> " (last slot " <> show slot
            <> ", note=" <> note <> ")"
        ) Nothing
