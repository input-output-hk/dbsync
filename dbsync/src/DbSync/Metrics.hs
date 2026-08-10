{-# LANGUAGE OverloadedStrings #-}

-- | Metrics are not implemented. The 'Metrics' record is a
-- placeholder, the mutators below record nothing, and no endpoint
-- exports anything. The shape exists so call sites can stay in place
-- until a real backend lands.
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

-- | Every field is zero in the only value ever built,
-- @DbSync.App.Setup.placeholderMetrics@.
data Metrics = Metrics
  { mBlocksProcessed :: !Int64   -- ^ Unused. Nothing reads this field.
  , mCurrentEpoch    :: !Int64   -- ^ Unused. Nothing reads this field.
  , mCurrentBlock    :: !Int64   -- ^ Unused. Nothing reads this field.
  , mCurrentSlot     :: !Int64   -- ^ Unused. Nothing reads this field.
  , mBlocksPerSec    :: !Double  -- ^ Unused. Nothing reads this field.
  , mCopyRowsWritten :: !Int64   -- ^ Unused. Nothing reads this field.
  , mPhase           :: !Int64   -- ^ Unused. Intended as 0=Ingest, 1=Preparing, 2=Following.
  , mDedupStoreSize  :: !Int64   -- ^ Unused. Nothing reads this field.
  , mQueueDepth      :: !Int64   -- ^ Unused. Nothing reads this field.
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- * Accessor class
-- ---------------------------------------------------------------------------

class HasMetrics env where
  getMetrics :: env -> Metrics

-- ---------------------------------------------------------------------------
-- * Convenience
-- ---------------------------------------------------------------------------

-- Each mutator below discards its argument and returns @()@. They
-- keep the call sites typed until a real metrics backend lands.

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
