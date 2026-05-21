{-# LANGUAGE BangPatterns #-}

-- | Drain-size counters tracked by the Ingest consumer.
--
-- These are integer-only counters incremented on every queue drain
-- and sampled by two readers:
--
-- * The consumer's @diagnose@ at each epoch boundary, to derive the
--   @HEALTHY@ \/ @NODE STARVED@ \/ @BALANCED@ \/ @SATURATED@ \/
--   @SLOWING@ status string.
-- * The watchdog at each sample interval, which computes interval
--   deltas (current vs last-seen) for its Debug-level diagnostics.
--
-- The consumer resets the record to zero at each epoch boundary. The
-- watchdog tolerates the reset: when @current < last_seen@ it treats
-- the interval delta as @current@ (a fresh interval).
module DbSync.Phase.Ingest.PipelineStats
  ( PipelineStats (..)
  , emptyPipelineStats
  ) where

import Cardano.Prelude

-- | Per-epoch pipeline drain counters. Only tracks drain sizes
-- (integer increments, no system calls).
data PipelineStats = PipelineStats
  { psDrainTotal   :: !Word64  -- ^ Sum of all drain sizes
  , psDrainCount   :: !Word64  -- ^ Number of drain calls
  , psDrainMax     :: !Int     -- ^ Largest drain size seen
  , psSingleDrains :: !Word64  -- ^ Times drain returned exactly 1 block
  , psFullDrains   :: !Word64  -- ^ Times drain returned batchSize blocks
  }

emptyPipelineStats :: PipelineStats
emptyPipelineStats = PipelineStats 0 0 0 0 0
