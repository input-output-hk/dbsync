{-# LANGUAGE BangPatterns #-}

-- | Receiver-thread cumulative counters, sampled by the watchdog.
--
-- The ChainSync receiver in 'DbSync.Node.Connection' increments these
-- counters as it pulls blocks from the node. The watchdog reads them
-- at each sample interval, computes its own deltas against the
-- previously-seen value, and surfaces @blocked=+N@ at Debug level
-- alongside the rest of the pipeline state.
--
-- Counters are monotonic — nobody resets them. Wrap-around is
-- ignored: a 'Word64' here would take centuries to exhaust at any
-- realistic block rate.
module DbSync.Phase.Ingest.ReceiverStats
  ( -- * Stats
    ReceiverStats (..)
  , newReceiverStats

    -- * Producer-side updates (used by the receiver thread)
  , recordBlockReceived
  , recordWriteBlocked

    -- * Reader-side
  , Snapshot (..)
  , readSnapshot
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

-- | Mutable counters owned by the receiver thread.
--
-- Two separate 'IORef's rather than one record-of-fields IORef so the
-- receiver's increment hot path doesn't have to re-write unrelated
-- fields. Each counter is updated with 'atomicModifyIORef'' so we
-- never lose updates if multiple threads happen to touch them
-- concurrently.
data ReceiverStats = ReceiverStats
  { rsBlocksReceived :: !(IORef Word64)
    -- ^ Blocks delivered by the node since process start.
  , rsWritesBlocked  :: !(IORef Word64)
    -- ^ Times the receiver had to wait on a full block queue since
    --   process start. Non-zero deltas in the watchdog indicate the
    --   consumer is the bottleneck rather than the upstream node.
  }

-- | Allocate a fresh, zeroed stats record. Call once at startup.
newReceiverStats :: IO ReceiverStats
newReceiverStats =
  ReceiverStats
    <$> newIORef 0
    <*> newIORef 0

-- | Increment the received-blocks counter by one. O(1), lock-free
-- under the typical single-receiver pattern.
recordBlockReceived :: ReceiverStats -> IO ()
recordBlockReceived rs =
  atomicModifyIORef' (rsBlocksReceived rs) $ \ !n -> (n + 1, ())

-- | Increment the writes-blocked counter by one. Called when
-- 'tryWriteTBQueue' returns 'False' before the (necessarily blocking)
-- retry on the underlying 'writeTBQueue'.
recordWriteBlocked :: ReceiverStats -> IO ()
recordWriteBlocked rs =
  atomicModifyIORef' (rsWritesBlocked rs) $ \ !n -> (n + 1, ())

-- | Snapshot of receiver counters returned to readers.
data Snapshot = Snapshot
  { snBlocksReceived :: !Word64
  , snWritesBlocked  :: !Word64
  }
  deriving stock (Show, Eq)

-- | Read both counters without resetting them. Readers compute
-- their own deltas against a previously-seen 'Snapshot'.
readSnapshot :: ReceiverStats -> IO Snapshot
readSnapshot rs = do
  blocks  <- readIORef (rsBlocksReceived rs)
  blocked <- readIORef (rsWritesBlocked rs)
  pure Snapshot
    { snBlocksReceived = blocks
    , snWritesBlocked  = blocked
    }
