{-# LANGUAGE OverloadedStrings #-}

-- | Per-block 'Hasql.Pipeline' accumulator.
--
-- Replaces the per-row @Conn.use Sess.statement@ call site in the
-- Follow writer: every @writeXxx@ appends a @Pipeline.statement@
-- action to a single 'IORef' instead of issuing a network
-- round-trip. The orchestrator flushes the whole pipeline in one
-- 'Sess.pipeline' call at end of block.
--
-- The buffer is intentionally a single @IORef (Pipeline ())@ rather
-- than a record of per-table queues. Pipeline is 'Applicative', so
-- concatenation via @*>@ preserves submission order across tables
-- — which is exactly what we need so foreign keys land in
-- dependency order (block → tx → tx_out, etc.).
module DbSync.Phase.Following.WriteBuffer
  ( WriteBuffer
  , newWriteBuffer
  , append
  , drain
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Hasql.Pipeline as Pipeline

-- | An ordered, append-only accumulator of pipeline statements.
--
-- A 'WriteBuffer' lives for exactly one block: it is created at
-- the top of 'processForward', appended to by every extractor's
-- writer call, and drained once by the per-block flush. Reusing a
-- buffer across blocks would mix their statements; tests pin this
-- invariant.
newtype WriteBuffer = WriteBuffer (IORef (Pipeline.Pipeline ()))

newWriteBuffer :: IO WriteBuffer
newWriteBuffer = WriteBuffer <$> newIORef (pure ())

-- | Add a pipeline statement to the buffer. Order is preserved:
-- the statement runs after every statement appended before it.
append :: WriteBuffer -> Pipeline.Pipeline () -> IO ()
append (WriteBuffer ref) action =
  atomicModifyIORef' ref $ \prev -> (prev *> action, ())

-- | Take and clear the pipeline. Called by the orchestrator at end
-- of block to flush all writes in one network round-trip.
drain :: WriteBuffer -> IO (Pipeline.Pipeline ())
drain (WriteBuffer ref) =
  atomicModifyIORef' ref $ \prev -> (pure (), prev)
