{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Block consumer for 'IngestChainHistory'.
--
-- Reads 'CardanoBlock' values from the 'TQueue' (fed by the
-- 'BlockReceiver'), queries 'SlotDetails' from the HardFork
-- Interpreter, parses them into 'GenericBlock', runs the
-- enabled extractors, and writes rows to PostgreSQL via the
-- 'CopyWriter'. Detects epoch boundaries via 'sdEpochNo'
-- comparison and triggers commit + reopen cycles.
module DbSync.Ingest.Consumer
  ( -- * Running
    runConsumer

    -- * Queue utilities
  , drainTBQueue
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import Control.Concurrent.STM (STM, TQueue, atomically, readTQueue, tryReadTQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

import DbSync.Block.Parser (parseBlock)
import DbSync.Block.Types (GenericBlock (..))
import DbSync.Copy.Writer (CopyWriter (..))
import DbSync.Extractor (ExtractState, ExtractorDef)
import DbSync.Ingest.Pipeline (processBlock)
import DbSync.Node.Connection (CardanoBlock)
import DbSync.Resolver (IdResolver)
import DbSync.StateQuery (SlotDetails (..), StateQueryVar, getSlotDetails)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Writer (Writer)

import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

-- ---------------------------------------------------------------------------
-- * Running
-- ---------------------------------------------------------------------------

-- | Run the consumer loop.
--
-- Reads blocks from the 'TQueue' in batches, queries 'SlotDetails' from the
-- HardFork Interpreter, parses them, runs extractors, and writes to
-- PostgreSQL via the 'Writer'. Detects epoch boundaries via
-- 'sdEpochNo' comparison and triggers 'cwCommit' + 'cwReopen'.
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
  prevEpochRef <- newIORef (Nothing :: Maybe EpochNo)
  blockCountRef <- newIORef (0 :: Word64)
  epochStartRef <- newIORef =<< getCurrentTime
  loop prevEpochRef blockCountRef epochStartRef
  where
    -- | Maximum blocks to drain from the queue per batch.
    batchSize :: Int
    batchSize = 100

    loop :: IORef (Maybe EpochNo) -> IORef Word64 -> IORef UTCTime -> IO ()
    loop prevEpochRef blockCountRef epochStartRef = do
      -- 1. Drain a batch of blocks from the queue
      blocks <- drainTBQueue queue batchSize

      -- 2. Process each block in the batch
      forM_ blocks $ \cardanoBlock -> do
        let slot = blockSlot cardanoBlock
        sd <- getSlotDetails sqv systemStart slot
        let !genBlock = parseBlock sd cardanoBlock
            blockEpoch = sdEpochNo sd

        -- Check for epoch boundary BEFORE processing
        prevEpoch <- readIORef prevEpochRef
        case prevEpoch of
          Just prev | prev /= blockEpoch -> do
            now <- getCurrentTime
            epochStart <- readIORef epochStartRef
            blockCount <- readIORef blockCountRef
            let elapsed = diffUTCTime now epochStart
                blocksPerSec :: Double
                blocksPerSec = if elapsed > 0
                  then fromIntegral blockCount / realToFrac elapsed
                  else 0
            traceWith tracer $ LogMsg Info "Ingest"
              ( "Epoch " <> show (unEpochNo prev) <> " complete | "
                <> show blockCount <> " blocks | "
                <> show (round blocksPerSec :: Int) <> " blocks/sec | "
                <> show (round (realToFrac elapsed :: Double) :: Int) <> "s"
              ) Nothing
            cwCommit copyWriter
            cwReopen copyWriter
            writeIORef blockCountRef 0
            writeIORef epochStartRef now
          _ -> pure ()

        -- Process the block through all extractors
        processBlock resolver writer extractors genBlock

        -- Update state
        modifyIORef' blockCountRef (+ 1)
        writeIORef prevEpochRef (Just blockEpoch)

      -- 3. Loop
      loop prevEpochRef blockCountRef epochStartRef

-- ---------------------------------------------------------------------------
-- * Queue utilities
-- ---------------------------------------------------------------------------

-- | Drain up to @maxN@ blocks from the queue.
-- Blocks until at least one is available, then takes as many as
-- are immediately available (up to @maxN@) without waiting.
drainTBQueue :: forall a. TQueue a -> Int -> IO [a]
drainTBQueue q maxN = atomically $ do
  first <- readTQueue q
  rest <- go (maxN - 1)
  pure (first : rest)
  where
    go :: Int -> STM [a]
    go 0 = pure []
    go n = do
      mVal <- tryReadTQueue q
      case mVal of
        Nothing  -> pure []
        Just val -> (val :) <$> go (n - 1)
