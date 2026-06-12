{-# LANGUAGE OverloadedStrings #-}

-- | Prometheus counters and gauges exposed via the metrics HTTP endpoint.
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

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Prometheus counters and gauges exposed to the metrics endpoint.
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

-- ---------------------------------------------------------------------------
-- * Accessor class
-- ---------------------------------------------------------------------------

-- | Access metrics from any environment.
class HasMetrics env where
  getMetrics :: env -> Metrics

-- ---------------------------------------------------------------------------
-- * Convenience
-- ---------------------------------------------------------------------------

incBlocksProcessed :: (MonadReader env m, HasMetrics env) => m ()
incBlocksProcessed = do
  _metrics <- asks getMetrics
  pure ()

setCurrentEpoch :: (MonadReader env m, HasMetrics env) => EpochNo -> m ()
setCurrentEpoch _epochNo = do
  _metrics <- asks getMetrics
  pure ()

addCopyRows :: (MonadReader env m, HasMetrics env) => Int -> m ()
addCopyRows _n = do
  _metrics <- asks getMetrics
  pure ()
