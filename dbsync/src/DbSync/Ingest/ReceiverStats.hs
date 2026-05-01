{-# LANGUAGE BangPatterns #-}

-- | Receiver-thread statistics shared with the consumer for diagnostics.
--
-- The ChainSync receiver in 'DbSync.Node.Connection' increments these
-- counters as it pulls blocks from the node. The consumer reads them at
-- each epoch boundary so the @Ingest:@ log line can show what the
-- /upstream/ side of the queue is doing, not just what the consumer
-- (downstream) side sees.
--
-- Why this matters
--
-- The existing @drain X\/100@ stat in the consumer's log line tells you
-- the average number of blocks pulled per drain call. A consistently low
-- value (e.g. @drain 1\/100@) is consistent with two very different
-- scenarios:
--
-- 1. The node feeds us slowly (upstream-limited).
-- 2. The consumer is fast enough to drain the queue between every
--    arriving block, regardless of the upstream rate.
--
-- The receiver-side counters here disambiguate: 'rsBlocksReceived'
-- gives the upstream rate directly, and 'rsWritesBlocked' tells you
-- whether the receiver ever had to wait for the queue to drain (which
-- would mean the consumer is the bottleneck, not the node).
module DbSync.Ingest.ReceiverStats
  ( -- * Stats
    ReceiverStats (..)
  , newReceiverStats

    -- * Producer-side updates (used by the receiver thread)
  , recordBlockReceived
  , recordWriteBlocked

    -- * Consumer-side reads
  , EpochSnapshot (..)
  , readAndResetEpoch
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef)

-- | Mutable counters owned by the receiver thread but read (and reset)
-- by the consumer at each epoch boundary.
--
-- Two separate 'IORef's rather than one record-of-fields IORef so the
-- receiver's increment hot path doesn't have to re-write unrelated
-- fields. Each counter is updated with 'atomicModifyIORef'' so we never
-- lose updates if both threads happen to touch them concurrently
-- (currently only the receiver writes; the consumer only reads-and-resets).
data ReceiverStats = ReceiverStats
  { rsBlocksReceived :: !(IORef Word64)
    -- ^ Blocks delivered by the node since the last reset.
  , rsWritesBlocked  :: !(IORef Word64)
    -- ^ Times the receiver had to wait on a full block queue since the
    --   last reset. Non-zero values indicate the consumer is the
    --   bottleneck rather than the upstream node.
  }

-- | Allocate a fresh, zeroed stats record. Call once at startup.
newReceiverStats :: IO ReceiverStats
newReceiverStats =
  ReceiverStats
    <$> newIORef 0
    <*> newIORef 0

-- | Increment the received-blocks counter by one. O(1), lock-free under
-- the typical single-receiver pattern.
recordBlockReceived :: ReceiverStats -> IO ()
recordBlockReceived rs =
  atomicModifyIORef' (rsBlocksReceived rs) $ \ !n -> (n + 1, ())

-- | Increment the writes-blocked counter by one. Called when
-- 'tryWriteTBQueue' returns 'False' before the (necessarily blocking)
-- retry on the underlying 'writeTBQueue'.
recordWriteBlocked :: ReceiverStats -> IO ()
recordWriteBlocked rs =
  atomicModifyIORef' (rsWritesBlocked rs) $ \ !n -> (n + 1, ())

-- | Snapshot of receiver counters returned to the consumer.
-- Field order matches the @Ingest:@ log line.
data EpochSnapshot = EpochSnapshot
  { esBlocksReceived :: !Word64
  , esWritesBlocked  :: !Word64
  }
  deriving stock (Show, Eq)

-- | Atomically read and reset both counters.
--
-- Returns the values they held just before the reset. Counters are
-- reset independently — if a block arrives between the two reads we
-- might miss it from the snapshot but it /will/ be counted in the next
-- epoch's snapshot, so totals across epochs remain consistent.
readAndResetEpoch :: ReceiverStats -> IO EpochSnapshot
readAndResetEpoch rs = do
  blocks  <- atomicModifyIORef' (rsBlocksReceived rs) $ \n -> (0, n)
  blocked <- atomicModifyIORef' (rsWritesBlocked rs) $ \n -> (0, n)
  pure EpochSnapshot
    { esBlocksReceived = blocks
    , esWritesBlocked  = blocked
    }
