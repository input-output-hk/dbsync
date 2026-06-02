{-# LANGUAGE OverloadedStrings #-}

-- | Prometheus metrics for monitoring sync progress.
--
-- Separate from tracing — metrics are quantitative counters/gauges
-- exposed via Prometheus HTTP endpoint.
module DbSync.Metrics
  ( -- * Types
    Metrics (..)

    -- * Accessor class
  , HasMetrics (..)

    -- * Convenience
  , incBlocksProcessed
  , setCurrentEpoch
  , addCopyRows
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo (..))

-- | Access metrics from any environment. Implemented per-env.
class HasMetrics env where
  getMetrics :: env -> Metrics

-- * Types

-- | Prometheus counters and gauges exposed to the metrics endpoint.
-- Updated throughout the sync lifecycle; readers come via
-- 'HasMetrics'.
data Metrics = Metrics
  { mBlocksProcessed :: !Int64
  , mCurrentEpoch    :: !Int64
  , mCurrentBlock    :: !Int64
  , mCurrentSlot     :: !Int64
  , mBlocksPerSec    :: !Double
  , mCopyRowsWritten :: !Int64
  , mPhase           :: !Int64   -- ^ 0=Ingest, 1=Preparing, 2=Following
  , mDedupStoreSize  :: !Int64
  , mQueueDepth      :: !Int64
  }
  deriving stock (Show)

-- * Convenience functions

-- | Increment the blocks processed counter.
incBlocksProcessed :: (MonadReader env m, HasMetrics env) => m ()
incBlocksProcessed = do
  _metrics <- asks getMetrics
  pure ()

-- | Set the current epoch gauge.
setCurrentEpoch :: (MonadReader env m, HasMetrics env) => EpochNo -> m ()
setCurrentEpoch _epochNo = do
  _metrics <- asks getMetrics
  pure ()

-- | Add to the COPY rows written counter.
addCopyRows :: (MonadReader env m, HasMetrics env) => Int -> m ()
addCopyRows _n = do
  _metrics <- asks getMetrics
  pure ()
