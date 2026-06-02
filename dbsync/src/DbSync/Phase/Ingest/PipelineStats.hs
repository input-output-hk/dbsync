{-# LANGUAGE BangPatterns #-}

-- | Drain-size counters tracked by the Ingest consumer.
--
-- Integer-only counters incremented on every queue drain and sampled
-- by the watchdog at each interval, which computes deltas (current
-- vs last-seen) for its Debug-level diagnostics. The consumer resets
-- the record to zero at each epoch boundary; the watchdog tolerates
-- the reset by treating @current < last_seen@ as a fresh interval.
module DbSync.Phase.Ingest.PipelineStats
  ( PipelineStats (..)
  , emptyPipelineStats
  ) where

import Cardano.Prelude

-- | Per-epoch pipeline drain counters.
data PipelineStats = PipelineStats
  { psDrainTotal   :: !Word64  -- ^ Sum of all drain sizes
  , psDrainCount   :: !Word64  -- ^ Number of drain calls
  , psSingleDrains :: !Word64  -- ^ Times drain returned exactly 1 block
  , psFullDrains   :: !Word64  -- ^ Times drain returned batchSize blocks
  }

emptyPipelineStats :: PipelineStats
emptyPipelineStats = PipelineStats 0 0 0 0
