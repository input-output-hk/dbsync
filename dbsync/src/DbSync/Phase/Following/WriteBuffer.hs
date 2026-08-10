{-# LANGUAGE OverloadedStrings #-}

-- | Per-block 'Hasql.Pipeline' accumulator. Every @writeXxx@ appends
-- instead of making a network round-trip, and the orchestrator
-- flushes the whole pipeline in one 'Sess.pipeline' call at end of
-- block.
--
-- One 'IORef' holds the lot, not a record of per-table queues:
-- Pipeline is 'Applicative', so @*>@ preserves submission order
-- across tables and foreign keys land in dependency order.
module DbSync.Phase.Following.WriteBuffer
  ( WriteBuffer
  , newWriteBuffer
  , append
  , drain
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Hasql.Pipeline as Pipeline

-- | Ordered, append-only accumulator of pipeline statements. One
-- buffer serves exactly one block: 'processForward' creates it, each
-- extractor's writer appends to it, and the per-block flush drains it
-- once. A buffer reused across blocks mixes their statements.
newtype WriteBuffer = WriteBuffer (IORef (Pipeline.Pipeline ()))

newWriteBuffer :: IO WriteBuffer
newWriteBuffer = WriteBuffer <$> newIORef (pure ())

-- | The statement runs after every statement appended before it.
append :: WriteBuffer -> Pipeline.Pipeline () -> IO ()
append (WriteBuffer ref) action =
  atomicModifyIORef' ref $ \prev -> (prev *> action, ())

-- | Take and clear the pipeline.
drain :: WriteBuffer -> IO (Pipeline.Pipeline ())
drain (WriteBuffer ref) =
  atomicModifyIORef' ref $ \prev -> (pure (), prev)
