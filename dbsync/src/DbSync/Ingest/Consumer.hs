{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Block consumer for 'IngestChainHistory'.
--
-- Reads 'CardanoBlock' values from the 'TBQueue' on the env, parses them
-- into 'GenericBlock', runs the enabled extractors, and writes rows to
-- PostgreSQL via the 'CopyWriter'. Detects epoch boundaries via
-- 'sdEpochNo' comparison and triggers commit + reopen cycles.
--
-- == Pipeline diagnostics
--
-- At each epoch boundary, the consumer logs one consolidated line:
--
-- @
-- Epoch 265 | 21,427 blk in 41s (526 blk/s) | recv 526/s blocked=0 | drain 85/100 (full=180 single=2) | commit 0.45s | EXTRACT GROWING (55x vs e2)
-- @
--
-- Reading the line:
--
-- * @recv X\/s blocked=N@ — receiver-side: blocks delivered by the node
--   per second this epoch, plus how many times the receiver had to wait
--   on a full block queue. @blocked=0@ with low @drain@ averages means
--   the upstream node is the bottleneck. @blocked>0@ means the consumer
--   side is occasionally the bottleneck.
-- * @drain X\/100@ is the /average/ drain size; the @full=@ and
--   @single=@ counts let you distinguish a steady mid-range average (low
--   variance) from a bimodal pattern of bursty fills and starvations
--   (high variance). When @single@ dominates, the queue is empty most
--   of the time we look at it.
--
-- Diagnostics use only drain-size counters (zero per-block overhead)
-- and epoch-level wall-clock timing. No per-block system calls.
module DbSync.Ingest.Consumer
  ( -- * Running
    runConsumer

    -- * Queue utilities
  , drainTBQueue
  , drainAppliedQueue

    -- * Replay-progress logging (exported for tests)
  , ReplayLogState (..)
  , ReplayProgress (..)
  , ReplayAdvance (..)
  , ReplayLog (..)
  , advanceReplay
  , progressLogInterval
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Concurrent.STM (TBQueue, readTBQueue, tryReadTBQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Strict.Maybe as SMaybe
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, NominalDiffTime)
import GHC.Stats (RTSStats (..), GCDetails (..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import DbSync.AppM (IngestM)
import DbSync.Block.Parser (parseBlock)
import DbSync.Block.Types (GenericBlock (..))
import DbSync.Checkpoint.Manager (mkBoundarySyncStateRow)
import DbSync.Checkpoint.SyncState (writeSyncState)
import DbSync.Config.Types (LedgerConfig (..), SyncConfig (..))
import DbSync.Copy.Writer (CopyWriter (..))
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats (..), SyncPhase (..))
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Env (HasConfig (..), IngestEnv (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Extractor.EpochBoundary (runEpochBoundary)
import DbSync.Id.DedupMap (dedupMapSizes)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Ingest.ReceiverStats (EpochSnapshot (..), readAndResetEpoch)
import DbSync.Ledger.Types
  ( ApplyResult (..)
  , HasLedgerEnv (..)
  , LedgerEnv (..)
  )
import DbSync.Resolver (IdResolver (..))
import DbSync.Resolver.AddressBuffer (takeAndReset)
import DbSync.Resolver.AddressWorker
  ( ResolveJob (..)
  , awaitDrained
  , enqueueResolveJob
  , readAddressIdCounter
  )
import DbSync.StateQuery
  ( ObservationResult (..)
  , ObservedTransition (..)
  , SlotDetails (..)
  , getSlotDetails
  , isInterpreterCached
  , observeBlockSTM
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Watchdog (bumpConsumer, setConsumerNote)
import DbSync.Writer (Writer (..))

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Shelley.HFEras ()                -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()  -- 'LedgerSupportsProtocol' orphans

-- ---------------------------------------------------------------------------
-- * Pipeline statistics (zero per-block overhead)
-- ---------------------------------------------------------------------------

-- | Per-epoch pipeline statistics. Only tracks drain sizes (integer
-- increments, no system calls). Reset at each epoch boundary.
data PipelineStats = PipelineStats
  { psDrainTotal   :: !Word64  -- ^ Sum of all drain sizes
  , psDrainCount   :: !Word64  -- ^ Number of drain calls
  , psDrainMax     :: !Int     -- ^ Largest drain size seen
  , psSingleDrains :: !Word64  -- ^ Times drain returned exactly 1 block
  , psFullDrains   :: !Word64  -- ^ Times drain returned batchSize blocks
  }

emptyPipelineStats :: PipelineStats
emptyPipelineStats = PipelineStats 0 0 0 0 0

-- | Baseline blocks/sec captured from the first fast epoch.
-- Used to detect slowdowns via the "Nx vs baseline" indicator.
data BaselineRef = BaselineRef
  { brBlocksPerSec :: !Double  -- ^ Baseline throughput
  , brEpoch        :: !Word64  -- ^ Which epoch the baseline was captured from
  }

-- ---------------------------------------------------------------------------
-- * Replay-progress logging
-- ---------------------------------------------------------------------------

-- | State machine driving the @LedgerReplay@ log channel during a
-- ledger-enabled resume\'s replay window. Without it the consumer
-- emits no progress for the duration of the window (it skips
-- 'processBlock' for replayed slots) and the operator can\'t tell
-- a slow replay from a hang.
data ReplayLogState
  = NoReplay
    -- ^ No replay configured, or the window has been exited.
  | ReplayPending
    -- ^ Replay configured; no block observed yet.
  | InReplay !ReplayProgress
    -- ^ Inside the replay window; counters drive log cadence.
  deriving stock (Eq, Show)

-- | Block counter and log-cadence timestamps carried inside 'InReplay'.
data ReplayProgress = ReplayProgress
  { rpStartTime     :: !UTCTime
  , rpBlocksApplied :: !Word64
  , rpLastLogTime   :: !UTCTime
  }
  deriving stock (Eq, Show)

-- | Result of advancing 'ReplayLogState' for one received block.
data ReplayAdvance = ReplayAdvance
  { raNewState :: !ReplayLogState
  , raLog      :: !ReplayLog
  }
  deriving stock (Eq, Show)

-- | Log directive produced by 'advanceReplay'. The caller emits the
-- trace; keeping the decision pure makes it trivial to unit-test.
data ReplayLog
  = ReplayLogNothing
  | ReplayLogProgress !Word64
    -- ^ Emit a progress line — \"applied @N@ blocks so far\".
  | ReplayLogComplete !Word64 !NominalDiffTime
    -- ^ Emit a completion line — \"@N@ blocks replayed in @T@s\".
  deriving stock (Eq, Show)

-- | Wall-clock cadence between progress lines. Five seconds keeps
-- short replays silent while still flagging liveness on long ones.
progressLogInterval :: NominalDiffTime
progressLogInterval = 5

-- | Render a slot-progress percentage of the form @\" (~37%)\"@.
-- Empty string when bounds are missing or the window has zero
-- width. Uses /slot/ progress, not /block/ progress, since Cardano
-- slots can be empty so the total block count is unknown up front.
renderReplayPercent :: Maybe SlotNo -> Maybe SlotNo -> SlotNo -> Text
renderReplayPercent (Just (SlotNo start)) (Just (SlotNo endBound)) (SlotNo cur)
  | endBound > start =
      let span'   = endBound - start
          done    = if cur > endBound then span'
                    else if cur > start then cur - start
                                        else 0
          pct     = (done * 100) `div` span'
      in " (~" <> show pct <> "%)"
renderReplayPercent _ _ _ = ""

-- | Advance the replay-log state machine given the just-arrived
-- block\'s slot, the resume boundary (@'Nothing'@ = no replay) and
-- the current wall-clock time. Pure; the caller mutates the IORef
-- and emits any indicated trace.
advanceReplay
  :: SlotNo
  -> Maybe SlotNo
  -> UTCTime
  -> ReplayLogState
  -> ReplayAdvance
advanceReplay _    Nothing  _   s =
  ReplayAdvance s ReplayLogNothing
advanceReplay slot (Just bs) now s =
  let inReplay = slot <= bs
  in case s of
       NoReplay ->
         ReplayAdvance NoReplay ReplayLogNothing
       ReplayPending
         | inReplay  ->
             let p = ReplayProgress
                       { rpStartTime     = now
                       , rpBlocksApplied = 1
                       , rpLastLogTime   = now
                       }
             in ReplayAdvance (InReplay p) ReplayLogNothing
         | otherwise ->
             -- First block already past the boundary — degenerate
             -- replay window of zero blocks. Skip straight to
             -- 'NoReplay' without firing any log.
             ReplayAdvance NoReplay ReplayLogNothing
       InReplay p
         | inReplay ->
             let p' = p { rpBlocksApplied = rpBlocksApplied p + 1 }
                 elapsedSinceLog = diffUTCTime now (rpLastLogTime p)
             in if elapsedSinceLog >= progressLogInterval
                  then ReplayAdvance
                         (InReplay p' { rpLastLogTime = now })
                         (ReplayLogProgress (rpBlocksApplied p'))
                  else ReplayAdvance (InReplay p') ReplayLogNothing
         | otherwise ->
             let totalElapsed = diffUTCTime now (rpStartTime p)
             in ReplayAdvance NoReplay
                  (ReplayLogComplete (rpBlocksApplied p) totalElapsed)

-- ---------------------------------------------------------------------------
-- * Number formatting
-- ---------------------------------------------------------------------------

-- | Format a Double with 2 decimal places, no scientific notation.
fmtF2 :: Double -> Text
fmtF2 d = toS (printf "%.2f" d :: [Char])

-- | Format a large integer with comma separators.
fmtInt :: Word64 -> Text
fmtInt n
  | n < 1000  = show n
  | otherwise =
      let s :: [Char]
          s = toS (show n :: Text)
          len = length s
          (prefix, rest) = splitAt (len `mod` 3) s
          groups = chunksOf3 rest
          allGroups = if null prefix then groups else prefix : groups
      in toS (commaJoin allGroups)
  where
    chunksOf3 :: [a] -> [[a]]
    chunksOf3 [] = []
    chunksOf3 xs = let (h, tl) = splitAt 3 xs in h : chunksOf3 tl

    commaJoin :: [[Char]] -> [Char]
    commaJoin [] = []
    commaJoin [x] = x
    commaJoin (x:xs) = x ++ "," ++ commaJoin xs

-- | Format a @(name, count)@ list as @"name=N1,234 …"@ for log lines.
renderDedupCounts :: [(Text, Int)] -> Text
renderDedupCounts = Text.intercalate " " . map one
  where
    one (n, c) = n <> "=" <> fmtInt (fromIntegral c)

-- | Sample the GHC runtime's view of memory usage at the moment of call.
--
-- Returns @(liveBytes, totalCommittedBytes)@, where:
--
--   * @liveBytes@ is the live data after the most recent GC (the
--     working set GHC actually retains)
--   * @totalCommittedBytes@ is the largest amount of memory GHC has
--     committed during the run (closest approximation to peak RSS
--     attributable to the Haskell heap)
--
-- Requires @+RTS -T -RTS@; safe to call from any thread. Returns
-- 'Nothing' if RTS stats aren't enabled.
sampleHeapBytes :: IO (Maybe (Word64, Word64))
sampleHeapBytes = do
  enabled <- getRTSStatsEnabled
  if enabled
    then do
      s <- getRTSStats
      pure $ Just (gcdetails_live_bytes (gc s), max_mem_in_use_bytes s)
    else pure Nothing

-- | Render a byte count as a short human-readable string, e.g.
-- @123MB@, @1.4GB@.
fmtBytes :: Word64 -> Text
fmtBytes b
  | b >= gib = Text.pack (printf "%.1fGB" (fromIntegral b / fromIntegral gib :: Double))
  | b >= mib = Text.pack (printf "%dMB"   (b `div` mib))
  | b >= kib = Text.pack (printf "%dKB"   (b `div` kib))
  | otherwise = show b <> "B"
  where
    kib, mib, gib :: Word64
    kib = 1024
    mib = 1024 * 1024
    gib = 1024 * 1024 * 1024

-- | Format seconds as human-readable duration.
fmtDuration :: Double -> Text
fmtDuration secs
  | secs < 60 = show (round secs :: Int) <> "s"
  | secs < 3600 =
      let t = round secs :: Int
      in show (t `div` 60) <> "m " <> show (t `mod` 60) <> "s"
  | otherwise =
      let t = round secs :: Int
      in show (t `div` 3600) <> "h " <> show ((t `mod` 3600) `div` 60) <> "m"

-- ---------------------------------------------------------------------------
-- * Running
-- ---------------------------------------------------------------------------

-- | Run the consumer loop in 'IngestM'.
--
-- Pulls everything it needs (tracer, queue, resolver, writer, copyWriter,
-- state-query handle, system start) from the 'IngestEnv'. The hot inner
-- loop runs in 'IngestM' itself rather than dropping back to raw 'IO' so
-- the env-aware 'processBlock' call can stay polymorphic.
--
-- Zero per-block overhead beyond the existing IORef bookkeeping: timing
-- still happens only at epoch boundaries via 'getCurrentTime', and drain
-- sizes are tracked with simple integer increments.
runConsumer :: IngestM ()
runConsumer = do
  prevEpochRef  <- liftIO $ newIORef (Nothing :: Maybe EpochNo)
  blockCountRef <- liftIO $ newIORef (0 :: Word64)
  epochStartRef <- liftIO $ getCurrentTime >>= newIORef
  statsRef      <- liftIO $ newIORef emptyPipelineStats
  baselineRef   <- liftIO $ newIORef (Nothing :: Maybe BaselineRef)
  -- (slot, blockNo, hash) of the most recently processed block;
  -- the resume point captured by 'commitEpoch' at each boundary.
  lastBlockRef  <- liftIO $ newIORef (Nothing :: Maybe (Word64, Word64, ByteString))
  -- Replay-progress state machine. Seeded as 'ReplayPending' iff a
  -- replay boundary was supplied at boot; otherwise 'NoReplay'.
  bootSlot      <- asks ieLastCommittedSlotAtBoot
  replayRef     <- liftIO $ newIORef $ case bootSlot of
                     Just _  -> ReplayPending
                     Nothing -> NoReplay
  loop prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef replayRef
  where
    batchSize :: Int
    batchSize = 100

    loop
      :: IORef (Maybe EpochNo)
      -> IORef Word64
      -> IORef UTCTime
      -> IORef PipelineStats
      -> IORef (Maybe BaselineRef)
      -> IORef (Maybe (Word64, Word64, ByteString))
      -> IORef ReplayLogState
      -> IngestM ()
    loop prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef replayRef = do
      queue <- asks ieBlockQueue

      -- 1. Drain a batch of blocks (no timing — just count)
      blocks <- liftIO $ drainTBQueue queue batchSize
      let !drainSize = length blocks

      -- Update drain stats (integer ops only, no syscalls)
      liftIO $ modifyIORef' statsRef $ \ps -> ps
        { psDrainTotal   = psDrainTotal ps + fromIntegral drainSize
        , psDrainCount   = psDrainCount ps + 1
        , psDrainMax     = max (psDrainMax ps) drainSize
        , psSingleDrains = psSingleDrains ps + if drainSize == 1 then 1 else 0
        , psFullDrains   = psFullDrains ps + if drainSize >= batchSize then 1 else 0
        }

      -- 2. Process batch (releases each CardanoBlock after parsing)
      processBatch prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef replayRef blocks

      -- 3. Loop
      loop prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef replayRef

    processBatch
      :: IORef (Maybe EpochNo)
      -> IORef Word64
      -> IORef UTCTime
      -> IORef PipelineStats
      -> IORef (Maybe BaselineRef)
      -> IORef (Maybe (Word64, Word64, ByteString))
      -> IORef ReplayLogState
      -> [CardanoBlock StandardCrypto]
      -> IngestM ()
    processBatch _ _ _ _ _ _ _ [] = pure ()
    processBatch prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef replayRef (cardanoBlock : rest) = do
      tracer        <- asks getTracer
      sqv           <- asks ieStateQueryVar
      resolver      <- asks ieResolver
      writer        <- asks ieWriter
      copyWriter    <- asks ieCopyWriter
      receiverStats <- asks ieReceiverStats
      hasLedger     <- asks ieHasLedgerEnv
      extractStRef  <- asks ieExtractState
      dedupMaps     <- asks ieDedupMaps
      addressBuffer <- asks ieAddressBuffer
      addressResolver <- asks ieAddressResolver
      ctrlConn      <- asks ieControlConnection
      bootSlot      <- asks ieLastCommittedSlotAtBoot
      replayStart   <- asks ieReplayStartSlot
      watchdog      <- asks ieWatchdog
      cfg           <- asks getConfig
      let ledgerEnabledCfg = lcEnabled (scLedger cfg)
          schemaVersion    = 1 :: Int
          slot             = blockSlot cardanoBlock
          isReplay         = case bootSlot of
            Just bs -> slot <= bs
            Nothing -> False

      -- Advance the replay-log state machine before the 'unless
      -- isReplay' branch so 'ReplayLogComplete' fires on the first
      -- /non/-replay block, just before normal processing resumes.
      nowForReplay <- liftIO getCurrentTime
      logEvent <- liftIO $ atomicModifyIORef' replayRef $ \prev ->
        let advance = advanceReplay slot bootSlot nowForReplay prev
        in (raNewState advance, raLog advance)
      let traceReplay msg =
            liftIO $ traceWith tracer $ LogMsg Info "LedgerReplay" msg Nothing
      case logEvent of
        ReplayLogNothing -> pure ()
        ReplayLogProgress n ->
          traceReplay $
            "applied " <> fmtInt n <> " blocks; current slot "
              <> show (unSlotNo slot)
              <> renderReplayPercent replayStart bootSlot slot
        ReplayLogComplete n elapsed ->
          traceReplay $
            "replay complete; applied " <> fmtInt n
              <> " blocks in " <> fmtF2 (realToFrac elapsed :: Double)
              <> "s, resuming COPY at slot " <> show (unSlotNo slot)

      -- Drain one leAppliedQueue entry per replayed block; otherwise
      -- the bounded queue fills and the worker deadlocks.
      when isReplay $ case hasLedger of
        LedgerEnabled lenv -> liftIO $ drainAppliedQueue (leAppliedQueue lenv)
        LedgerDisabled _   -> pure ()

      -- Replayed blocks are already in PG; skip processBlock.
      unless isReplay $ do
        -- Update the observed summary before 'getSlotDetails' so
        -- any era-boundary transition is in scope when the slot
        -- details are computed.
        obsResult <- liftIO $ atomically $ observeBlockSTM sqv cardanoBlock
        case obsResult of
          NewTransition t ->
            liftIO $ traceWith tracer $ LogMsg Info "StateQuery"
              ( "Observed era transition "
                  <> show (otFromEra t) <> " → " <> show (otToEra t)
                  <> " at slot " <> show (unSlotNo (otAtSlot t))
                  <> " (epoch " <> show (unEpochNo (otAtEpoch t)) <> ")"
              ) Nothing
          ObservationBroken fromEra toEra -> do
            -- Suppress the misleading "falling back to node" warning
            -- when 'sqvInterpreterVar' is already seeded — the
            -- observed-summary path isn't actually used in that case.
            cached <- liftIO $ isInterpreterCached sqv
            unless cached $
              liftIO $ traceWith tracer $ LogMsg Warning "StateQuery"
                ( "Observed era jump too large ("
                    <> show fromEra <> " → " <> show toEra
                    <> "); falling back to node interpreter"
                ) Nothing
          Unchanged -> pure ()

        sd <- getSlotDetails slot
        let !genBlock = parseBlock sd cardanoBlock
            !blockEpoch = sdEpochNo sd

        -- Epoch boundary check
        prevEpoch <- liftIO $ readIORef prevEpochRef
        case prevEpoch of
          Just prev | prev /= blockEpoch -> do
            -- Wall-clock for the entire epoch (one syscall)
            now        <- liftIO getCurrentTime
            epochStart <- liftIO $ readIORef epochStartRef
            blockCount <- liftIO $ readIORef blockCountRef
            let elapsed = diffUTCTime now epochStart
                blocksPerSec :: Double
                blocksPerSec = if elapsed > 0
                  then fromIntegral blockCount / realToFrac elapsed
                  else 0
                elapsedSec :: Double
                elapsedSec = realToFrac elapsed

            -- Write epoch sync stats to DB (before commit)
            essId <- liftIO $ assignEpochSyncStatsId resolver
            let ess = EpochSyncStats
                  { epochSyncStatsEpochNo        = unEpochNo prev
                  , epochSyncStatsBlocksProcessed = blockCount
                  , epochSyncStatsBlocksPerSec    = blocksPerSec
                  , epochSyncStatsElapsedSec      = elapsedSec
                  , epochSyncStatsSyncedAt        = now
                  , epochSyncStatsPhase           = IngestChainHistory
                  }
            liftIO $ writeEpochSyncStats writer essId ess

            -- Build the resume row from the last fully-extracted
            -- block of this epoch and the current ID counters.
            mLastBlock      <- liftIO $ readIORef lastBlockRef
            extractState    <- liftIO $ readIORef extractStRef
            let counters    = esIdCounters extractState

            -- Atomic epoch boundary: flush COPY → enqueue + await the
            -- address resolver → advance sync_state → reopen streams.
            -- The order matters: sync_state only advances after the
            -- worker has resolved every @tx_out.address_id@ FK for
            -- this epoch, so a crash after this point leaves the DB
            -- in a fully-resolved state up to @last_committed_slot@.
            commitStart <- liftIO getCurrentTime
            liftIO $ do
              -- 1. Flush COPY streams — tx_outs durable, address_id = NULL.
              setConsumerNote watchdog "consumer: cwCommit (flushing COPY)"
              cwCommit copyWriter

              -- 2. Hand the per-epoch address-resolution buffer to the worker.
              --    'enqueueResolveJob' blocks if the worker queue is at its
              --    bound, back-pressuring the main pipeline.
              setConsumerNote watchdog "consumer: enqueueResolveJob"
              buf <- takeAndReset addressBuffer
              enqueueResolveJob addressResolver (ResolveJob prev buf)

              -- 3. Block until the worker has processed all queued jobs.
              --    After this, every tx_out / collateral_tx_out through
              --    this epoch has its address_id populated.
              setConsumerNote watchdog "consumer: awaitDrained (address resolver)"
              awaitDrained addressResolver

              -- 4. Safe to advance sync_state: COPY data is durable AND
              --    address_id FKs are resolved. The address counter is
              --    read from the resolver (not 'counters') because the
              --    worker is its sole allocator.
              setConsumerNote watchdog "consumer: writeSyncState"
              addressIdCounter <- readAddressIdCounter addressResolver
              case mLastBlock of
                Just (lastSlot, lastBlockNo, lastHash) -> do
                  let row = mkBoundarySyncStateRow
                              lastSlot lastBlockNo lastHash
                              counters addressIdCounter
                              schemaVersion ledgerEnabledCfg
                  writeSyncState ctrlConn row
                Nothing -> pure ()

              -- 5. Reopen COPY streams for the next epoch.
              setConsumerNote watchdog "consumer: cwReopen"
              cwReopen copyWriter
              setConsumerNote watchdog "consumer: post-commit"
            commitEnd <- liftIO getCurrentTime
            let commitSec :: NominalDiffTime
                commitSec = diffUTCTime commitEnd commitStart

            -- Epoch boundary commit just completed. Run major GC if this was a heavy epoch.
            -- Gated at >10s to avoid penalizing fast Byron epochs (2-3s each).
            when (elapsedSec > 10.0) $ liftIO performMajorGC

            -- Log single consolidated line
            ps       <- liftIO $ readIORef statsRef
            baseline <- liftIO $ readIORef baselineRef
            recvSnap <- liftIO $ readAndResetEpoch receiverStats

            let status = diagnose batchSize blocksPerSec ps baseline
                recvPerSec :: Int
                recvPerSec
                  | elapsedSec > 0 =
                      round (fromIntegral (esBlocksReceived recvSnap) / elapsedSec :: Double)
                  | otherwise = 0

            -- Capture baseline from first fast epoch
            when (isNothing baseline && blocksPerSec > 500) $
              liftIO $ writeIORef baselineRef (Just (BaselineRef blocksPerSec (unEpochNo prev)))

            liftIO $ traceWith tracer $ LogMsg Info "Ingest"
              ( "Epoch " <> show (unEpochNo prev)
                <> " | " <> fmtInt blockCount <> " blk in " <> fmtDuration elapsedSec
                <> " (" <> show (round blocksPerSec :: Int) <> " blk/s)"
                <> " | recv " <> show recvPerSec <> "/s blocked="
                <> fmtInt (esWritesBlocked recvSnap)
                <> " | drain " <> show (avgDrain ps) <> "/" <> show batchSize
                <> " (full=" <> fmtInt (psFullDrains ps)
                <> " single=" <> fmtInt (psSingleDrains ps) <> ")"
                <> " | commit " <> fmtF2 (realToFrac commitSec :: Double) <> "s"
                <> " | " <> status
              ) Nothing

            -- Dedup-map size + heap-usage trace: helps diagnose RAM
            -- growth driven by per-entity hash-table accumulation
            -- (address, multi_asset, …). 'heap=' is live bytes after
            -- the most recent GC; 'peak=' is the high-water mark of
            -- memory GHC has committed (≈ peak heap-attributed RSS,
            -- once @--disable-delayed-os-memory-return@ is off).
            dedupCounts <- liftIO $ dedupMapSizes dedupMaps
            heapInfo    <- liftIO sampleHeapBytes
            let heapText = case heapInfo of
                  Just (live, peak) ->
                    " | heap=" <> fmtBytes live <> " peak=" <> fmtBytes peak
                  Nothing -> ""
            liftIO $ traceWith tracer $ LogMsg Info "Dedup"
              ( "Epoch " <> show (unEpochNo prev)
                <> " | " <> renderDedupCounts dedupCounts
                <> heapText
              ) Nothing

            -- Reset for next epoch
            liftIO $ writeIORef statsRef emptyPipelineStats
            liftIO $ writeIORef blockCountRef 0
            liftIO $ writeIORef epochStartRef commitEnd  -- start AFTER commit completes
          _ -> pure ()

        -- Run extractors + write to COPY queues
        liftIO $ setConsumerNote watchdog "consumer: processBlock"
        processBlock genBlock

        -- Boundary-block extractor (epoch-table writes that depend
        -- on the ledger worker's apNewEpoch).
        case prevEpoch of
          Just prev | prev /= blockEpoch ->
            case hasLedger of
              LedgerEnabled lenv -> do
                liftIO $ setConsumerNote watchdog "consumer: waitForApplyResultAt (boundary)"
                applyResult <- liftIO $ waitForApplyResultAt lenv slot
                mLastBlockId <- liftIO $ esLastBlockId <$> readIORef extractStRef
                case mLastBlockId of
                  Just lastBid -> do
                    liftIO $ setConsumerNote watchdog "consumer: runEpochBoundary"
                    liftIO $ runEpochBoundary applyResult (BlockId lastBid) resolver writer
                  Nothing -> pure ()
              LedgerDisabled _ ->
                pure ()
          _ -> pure ()

        -- Update counters
        liftIO $ modifyIORef' blockCountRef (+ 1)
        liftIO $ writeIORef prevEpochRef (Just blockEpoch)
        -- Record this block's identity for the next boundary commit.
        liftIO $ writeIORef lastBlockRef $ Just
          ( unSlotNo (blkSlotNo genBlock)
          , unBlockNo (blkBlockNo genBlock)
          , blkHash genBlock
          )

      -- Watchdog bump: per iteration, replay or not, so the
      -- watchdog still sees forward progress during the replay
      -- window (where 'processBlock' is skipped).
      liftIO $ bumpConsumer watchdog slot

      -- Recurse, whether the block was processed or skipped.
      processBatch prevEpochRef blockCountRef epochStartRef statsRef baselineRef lastBlockRef replayRef rest

-- ---------------------------------------------------------------------------
-- * Diagnosis
-- ---------------------------------------------------------------------------

-- | Compute average drain size as an integer.
avgDrain :: PipelineStats -> Int
avgDrain ps
  | psDrainCount ps == 0 = 0
  | otherwise = fromIntegral (psDrainTotal ps) `div` fromIntegral (psDrainCount ps)

-- | Throughput-aware diagnosis. Returns a short status string.
--
-- Check order matters — drain level is checked before wait/throughput:
--
--   1. High throughput (>1000 blk/s) → 'HEALTHY' regardless
--   2. Low drain (<5) → 'NODE STARVED' (queue empty, waiting for node)
--   3. High drain + slowing vs baseline → 'SLOWING (Nx vs eY)'
--   4. Medium drain → 'BALANCED'
--   5. High drain, steady → 'SATURATED'
diagnose
  :: Int              -- ^ batchSize
  -> Double           -- ^ blocks/sec this epoch
  -> PipelineStats    -- ^ drain stats this epoch
  -> Maybe BaselineRef
  -> Text
diagnose batchSz bps ps mBaseline
  -- Fast — no concern
  | bps > 1000 = "HEALTHY"

  -- Queue nearly empty — node can't keep up
  | avg < 5 = "NODE STARVED"

  -- Queue full + throughput declining vs baseline
  | avg > highDrain
  , Just bl <- mBaseline
  , brBlocksPerSec bl > 0
  , bps < brBlocksPerSec bl * 0.5 =
      let ratio = brBlocksPerSec bl / max bps 1
      in "SLOWING (" <> fmtF2 ratio <> "x slower vs e" <> show (brEpoch bl) <> ")"

  -- Queue partially full — balanced
  | avg >= 5 && avg <= highDrain = "BALANCED"

  -- Queue consistently full — pipeline at capacity
  | otherwise = "SATURATED"
  where
    avg = avgDrain ps
    highDrain = (batchSz * 4) `div` 5  -- 80% of batchSize

-- ---------------------------------------------------------------------------
-- * LedgerWorker coordination
-- ---------------------------------------------------------------------------

-- | Block until the 'LedgerWorker' has produced an 'ApplyResult' whose
-- slot is at-or-past @targetSlot@, then return it.
--
-- Used at epoch boundaries to fetch the @apNewEpoch@ payload from
-- 'leLatestApplyResult'. STM 'retry' suspends the consumer thread
-- until the worker writes a fresh 'ApplyResult'.
--
-- The worker writes 'leLatestApplyResult' on every successful
-- 'applyBlock' (DbSync.Ledger.State), so the wait progresses
-- deterministically — no polling, no sleep loops.
waitForApplyResultAt :: LedgerEnv -> SlotNo -> IO ApplyResult
waitForApplyResultAt lenv targetSlot = Strict.atomically $ do
  mAR <- Strict.readTVar (leLatestApplyResult lenv)
  case mAR of
    SMaybe.Just ar
      | sdSlotNo (apSlotDetails ar) >= targetSlot -> pure ar
    _ -> retry

-- ---------------------------------------------------------------------------
-- * Queue utilities
-- ---------------------------------------------------------------------------

-- | Drain up to @maxN@ blocks from the queue.
-- Blocks until at least one is available, then takes as many as
-- are immediately available (up to @maxN@) without waiting.
drainTBQueue :: forall a. TBQueue a -> Int -> IO [a]
drainTBQueue q maxN = atomically $ do
  hd <- readTBQueue q
  rest <- go (maxN - 1)
  pure (hd : rest)
  where
    go :: Int -> STM [a]
    go 0 = pure []
    go n = do
      mVal <- tryReadTBQueue q
      case mVal of
        Nothing  -> pure []
        Just val -> (val :) <$> go (n - 1)

-- | Pop and discard one entry from the worker → consumer apply-result queue.
drainAppliedQueue :: TBQueue ApplyResult -> IO ()
drainAppliedQueue q = atomically $ void $ readTBQueue q
