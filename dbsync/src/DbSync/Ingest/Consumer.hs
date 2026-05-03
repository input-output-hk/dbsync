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
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Concurrent.STM (TBQueue, readTBQueue, tryReadTBQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Strict.Maybe as SMaybe
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, NominalDiffTime)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import DbSync.AppM (IngestM)
import DbSync.Block.Parser (parseBlock)
import DbSync.Block.Types ()
import DbSync.Copy.Writer (CopyWriter (..))
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats (..), SyncPhase (..))
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Env (IngestEnv (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Extractor.EpochBoundary (runEpochBoundary)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Ingest.ReceiverStats (EpochSnapshot (..), readAndResetEpoch)
import DbSync.Ledger.Types
  ( ApplyResult (..)
  , HasLedgerEnv (..)
  , LedgerEnv (..)
  )
import DbSync.Resolver (IdResolver (..))
import DbSync.StateQuery
  ( ObservationResult (..)
  , ObservedTransition (..)
  , SlotDetails (..)
  , getSlotDetails
  , observeBlockSTM
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (LogMsg (..), Severity (..))
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
  loop prevEpochRef blockCountRef epochStartRef statsRef baselineRef
  where
    batchSize :: Int
    batchSize = 100

    loop
      :: IORef (Maybe EpochNo)
      -> IORef Word64
      -> IORef UTCTime
      -> IORef PipelineStats
      -> IORef (Maybe BaselineRef)
      -> IngestM ()
    loop prevEpochRef blockCountRef epochStartRef statsRef baselineRef = do
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
      processBatch prevEpochRef blockCountRef epochStartRef statsRef baselineRef blocks

      -- 3. Loop
      loop prevEpochRef blockCountRef epochStartRef statsRef baselineRef

    processBatch
      :: IORef (Maybe EpochNo)
      -> IORef Word64
      -> IORef UTCTime
      -> IORef PipelineStats
      -> IORef (Maybe BaselineRef)
      -> [CardanoBlock StandardCrypto]
      -> IngestM ()
    processBatch _ _ _ _ _ [] = pure ()
    processBatch prevEpochRef blockCountRef epochStartRef statsRef baselineRef (cardanoBlock : rest) = do
      tracer        <- asks getTracer
      sqv           <- asks ieStateQueryVar
      resolver      <- asks ieResolver
      writer        <- asks ieWriter
      copyWriter    <- asks ieCopyWriter
      receiverStats <- asks ieReceiverStats
      hasLedger     <- asks ieHasLedgerEnv
      extractStRef  <- asks ieExtractState

      let slot = blockSlot cardanoBlock

      -- Update the locally-observed summary BEFORE computing slot
      -- details, so that any era-boundary transition is reflected in
      -- the summary by the time it's queried.
      obsResult <- liftIO $ atomically $ observeBlockSTM sqv cardanoBlock
      case obsResult of
        NewTransition t ->
          liftIO $ traceWith tracer $ LogMsg Info "StateQuery"
            ( "Observed era transition "
                <> show (otFromEra t) <> " → " <> show (otToEra t)
                <> " at slot " <> show (unSlotNo (otAtSlot t))
                <> " (epoch " <> show (unEpochNo (otAtEpoch t)) <> ")"
            ) Nothing
        ObservationBroken fromEra toEra ->
          liftIO $ traceWith tracer $ LogMsg Warning "StateQuery"
            ( "Observed era jump too large ("
                <> show fromEra <> " → " <> show toEra
                <> "); falling back to node interpreter"
            ) Nothing
        Unchanged -> pure ()

      sd <- getSlotDetails slot
      let !genBlock = parseBlock sd cardanoBlock
          !blockEpoch = sdEpochNo sd
      -- cardanoBlock is now unreferenced (genBlock doesn't retain it)

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

          -- Timed commit + reopen (one timing measurement per epoch)
          commitStart <- liftIO getCurrentTime
          liftIO $ cwCommit copyWriter
          liftIO $ cwReopen copyWriter
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

          -- Reset for next epoch
          liftIO $ writeIORef statsRef emptyPipelineStats
          liftIO $ writeIORef blockCountRef 0
          liftIO $ writeIORef epochStartRef commitEnd  -- start AFTER commit completes
        _ -> pure ()

      -- Run extractors + write to COPY queues
      processBlock genBlock

      -- After processBlock, if this was an epoch-boundary block, run the
      -- EpochBoundary extractor.  The boundary block's row has now been
      -- written to the new-epoch COPY transaction; its 'BlockId' lives in
      -- 'esLastBlockId' and is the FK target for the boundary tables
      -- (currently @ada_pots@; LEDGER-PLAN.md §15.5 lists the rest).
      --
      -- We wait synchronously for the LedgerWorker's
      -- 'leLatestApplyResult' to be at-or-past the boundary block's
      -- slot. The worker is generally not lock-stepped with the consumer
      -- (LEDGER-PLAN.md §5), but at boundary blocks the boundary-table
      -- writes need 'apNewEpoch' which only exists after the worker has
      -- applied the boundary block. The wait is bounded by the worker's
      -- single-block apply latency (sub-second on mainnet).
      case prevEpoch of
        Just prev | prev /= blockEpoch ->
          case hasLedger of
            LedgerEnabled lenv -> do
              applyResult <- liftIO $ waitForApplyResultAt lenv slot
              mLastBlockId <- liftIO $ esLastBlockId <$> readIORef extractStRef
              case mLastBlockId of
                Just lastBid ->
                  liftIO $ runEpochBoundary applyResult (BlockId lastBid) resolver writer
                Nothing -> pure ()  -- Should not happen: processBlock just assigned a BlockId
            LedgerDisabled _ ->
              -- Ledger feature off; boundary tables are not populated.
              pure ()
        _ -> pure ()

      -- Update counters
      liftIO $ modifyIORef' blockCountRef (+ 1)
      liftIO $ writeIORef prevEpochRef (Just blockEpoch)

      -- Recurse — previous cardanoBlock and genBlock now collectible
      processBatch prevEpochRef blockCountRef epochStartRef statsRef baselineRef rest

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
