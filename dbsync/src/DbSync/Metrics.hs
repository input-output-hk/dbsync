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

-- | Prometheus counters and gauges for monitoring.
-- These are mutable references updated throughout the sync lifecycle.
data Metrics = Metrics
  { mBlocksProcessed :: !Int64   -- ^ TODO: replace with Prometheus Counter
  , mCurrentEpoch    :: !Int64   -- ^ TODO: replace with Prometheus Gauge
  , mCurrentBlock    :: !Int64
  , mCurrentSlot     :: !Int64
  , mBlocksPerSec    :: !Double
  , mCopyRowsWritten :: !Int64
  , mPhase           :: !Int64   -- ^ 0=Ingest, 1=Preparing, 2=Following
  , mDedupMapSize    :: !Int64
  , mQueueDepth      :: !Int64
  }
  deriving stock (Show)

-- Note: In the real implementation, these fields will be Prometheus
-- Counter/Gauge types from the 'prometheus' package. For now they are
-- placeholder Int64/Double values to get the type signatures compiling.

-- * Convenience functions
--
-- NOTE: 'MonadIO m' will be reinstated on these signatures once the real
-- Prometheus Counter\/Gauge calls (which run in 'IO') are wired up. For now
-- the bodies are pure stubs, so the constraint is redundant.

-- | Increment the blocks processed counter.
incBlocksProcessed :: (MonadReader env m, HasMetrics env) => m ()
incBlocksProcessed = do
  _metrics <- asks getMetrics
  pure () -- TODO: Prometheus.incCounter (mBlocksProcessed metrics)

-- | Set the current epoch gauge.
setCurrentEpoch :: (MonadReader env m, HasMetrics env) => EpochNo -> m ()
setCurrentEpoch _epochNo = do
  _metrics <- asks getMetrics
  pure () -- TODO: Prometheus.setGauge (mCurrentEpoch metrics) (fromIntegral $ unEpochNo epochNo)

-- | Add to the COPY rows written counter.
addCopyRows :: (MonadReader env m, HasMetrics env) => Int -> m ()
addCopyRows _n = do
  _metrics <- asks getMetrics
  pure () -- TODO: Prometheus.addCounter (mCopyRowsWritten metrics) (fromIntegral n)
