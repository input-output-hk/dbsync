{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Block consumer for 'IngestChainHistory'.
--
-- Reads 'CardanoBlock' values from the 'TQueue', parses them into
-- 'GenericBlock', runs the enabled extractors, and writes rows to
-- PostgreSQL via the 'CopyWriter'. Detects epoch boundaries via
-- 'sdEpochNo' comparison and triggers commit + reopen cycles.
--
-- == Pipeline diagnostics
--
-- At each epoch boundary, the consumer logs one consolidated line:
--
-- @
-- Epoch 265 | 21,427 blk in 41s (526 blk/s) | drain 85/100 | commit 0.45s | EXTRACT GROWING (55x vs e2)
-- @
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

import Cardano.Slotting.Slot (EpochNo (..))

import Control.Concurrent.STM (TQueue, readTQueue, tryReadTQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, NominalDiffTime)
import Text.Printf (printf)

import DbSync.Block.Parser (parseBlock)
import DbSync.Block.Types ()
import DbSync.Copy.Writer (CopyWriter (..))
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats (..), SyncPhase (..))
import DbSync.Extractor (ExtractState, ExtractorDef)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Node.Connection (CardanoBlock)
import DbSync.Resolver (IdResolver (..))
import DbSync.StateQuery (SlotDetails (..), StateQueryVar, getSlotDetails)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Writer (Writer (..))

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

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

-- | Run the consumer loop.
--
-- Zero per-block overhead: no system calls inside the block processing loop.
-- Timing is measured only at epoch boundaries via 'getCurrentTime'.
-- Drain sizes are tracked with simple integer increments.
runConsumer
  :: AppTracer
  -> StateQueryVar
  -> SystemStart
  -> [ExtractorDef]
  -> TQueue (CardanoBlock StandardCrypto)
  -> IdResolver IO
  -> Writer IO
  -> CopyWriter
  -> IORef ExtractState
  -> IO ()
runConsumer tracer sqv systemStart extractors queue resolver writer copyWriter _stRef = do
  prevEpochRef  <- newIORef (Nothing :: Maybe EpochNo)
  blockCountRef <- newIORef (0 :: Word64)
  epochStartRef <- newIORef =<< getCurrentTime
  statsRef      <- newIORef emptyPipelineStats
  baselineRef   <- newIORef (Nothing :: Maybe BaselineRef)
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
      -> IO ()
    loop prevEpochRef blockCountRef epochStartRef statsRef baselineRef = do
      -- 1. Drain a batch of blocks (no timing — just count)
      blocks <- drainTBQueue queue batchSize
      let !drainSize = length blocks

      -- Update drain stats (integer ops only, no syscalls)
      modifyIORef' statsRef $ \ps -> ps
        { psDrainTotal   = psDrainTotal ps + fromIntegral drainSize
        , psDrainCount   = psDrainCount ps + 1
        , psDrainMax     = max (psDrainMax ps) drainSize
        , psSingleDrains = psSingleDrains ps + if drainSize == 1 then 1 else 0
        , psFullDrains   = psFullDrains ps + if drainSize >= batchSize then 1 else 0
        }

      -- 2. Process each block (no per-block timing)
      forM_ blocks $ \cardanoBlock -> do
        let slot = blockSlot cardanoBlock
        sd <- getSlotDetails sqv systemStart slot
        let !genBlock = parseBlock sd cardanoBlock
            blockEpoch = sdEpochNo sd

        -- Epoch boundary check
        prevEpoch <- readIORef prevEpochRef
        case prevEpoch of
          Just prev | prev /= blockEpoch -> do
            -- Wall-clock for the entire epoch (one syscall)
            now <- getCurrentTime
            epochStart <- readIORef epochStartRef
            blockCount <- readIORef blockCountRef
            let elapsed = diffUTCTime now epochStart
                blocksPerSec :: Double
                blocksPerSec = if elapsed > 0
                  then fromIntegral blockCount / realToFrac elapsed
                  else 0
                elapsedSec :: Double
                elapsedSec = realToFrac elapsed

            -- Write epoch sync stats to DB (before commit)
            essId <- assignEpochSyncStatsId resolver
            let ess = EpochSyncStats
                  { epochSyncStatsEpochNo        = unEpochNo prev
                  , epochSyncStatsBlocksProcessed = blockCount
                  , epochSyncStatsBlocksPerSec    = blocksPerSec
                  , epochSyncStatsElapsedSec      = elapsedSec
                  , epochSyncStatsSyncedAt        = now
                  , epochSyncStatsPhase           = IngestChainHistory
                  }
            writeEpochSyncStats writer essId ess

            -- Timed commit + reopen (one timing measurement per epoch)
            commitStart <- getCurrentTime
            cwCommit copyWriter
            cwReopen copyWriter
            commitEnd <- getCurrentTime
            let commitSec :: NominalDiffTime
                commitSec = diffUTCTime commitEnd commitStart

            -- Log single consolidated line
            ps <- readIORef statsRef
            baseline <- readIORef baselineRef

            let status = diagnose batchSize blocksPerSec ps baseline

            -- Capture baseline from first fast epoch
            when (isNothing baseline && blocksPerSec > 500) $
              writeIORef baselineRef (Just (BaselineRef blocksPerSec (unEpochNo prev)))

            traceWith tracer $ LogMsg Info "Ingest"
              ( "Epoch " <> show (unEpochNo prev)
                <> " | " <> fmtInt blockCount <> " blk in " <> fmtDuration elapsedSec
                <> " (" <> show (round blocksPerSec :: Int) <> " blk/s)"
                <> " | drain " <> show (avgDrain ps) <> "/" <> show batchSize
                <> " | commit " <> fmtF2 (realToFrac commitSec :: Double) <> "s"
                <> " | " <> status
              ) Nothing

            -- Reset for next epoch
            writeIORef statsRef emptyPipelineStats
            writeIORef blockCountRef 0
            writeIORef epochStartRef commitEnd  -- start AFTER commit completes
          _ -> pure ()

        -- Run extractors + write to COPY queues
        processBlock resolver writer extractors genBlock

        -- Update counters
        modifyIORef' blockCountRef (+ 1)
        writeIORef prevEpochRef (Just blockEpoch)

      -- 3. Loop
      loop prevEpochRef blockCountRef epochStartRef statsRef baselineRef

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
-- * Queue utilities
-- ---------------------------------------------------------------------------

-- | Drain up to @maxN@ blocks from the queue.
-- Blocks until at least one is available, then takes as many as
-- are immediately available (up to @maxN@) without waiting.
drainTBQueue :: forall a. TQueue a -> Int -> IO [a]
drainTBQueue q maxN = atomically $ do
  hd <- readTQueue q
  rest <- go (maxN - 1)
  pure (hd : rest)
  where
    go :: Int -> STM [a]
    go 0 = pure []
    go n = do
      mVal <- tryReadTQueue q
      case mVal of
        Nothing  -> pure []
        Just val -> (val :) <$> go (n - 1)
