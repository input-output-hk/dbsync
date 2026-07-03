{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Db.Loader
Description : Multi-threaded loader-stream writer with per-table TBQueue fan-out.

The 'LoaderStream' streams encoded rows to PostgreSQL via per-table
writer threads. Rows are accumulated into ~64KB chunks on the
producer side ('lsWriteRow' runs on the single consumer thread), so
the bounded per-table 'TBQueue's and the writer threads see one
element per chunk rather than per row — two STM transactions and
one transport call per ~64KB instead of per row. Each writer thread
drains its queue and pushes chunks down its dedicated loader
connection. Today that connection runs PostgreSQL's @COPY FROM
STDIN@ protocol via @libpq@ (which accepts arbitrarily-chunked,
non-row-aligned data); the public API of this module deliberately
hides that detail.

Epoch-aligned commits use a sentinel\/barrier pattern:

  1. Parser flushes each table's partial chunk, then writes
     'Nothing' to all queues
  2. Each writer drains remaining chunks, ends its stream, signals ready
  3. Parser waits for all writers, then @COMMIT@ on all connections
  4. Parser calls 'lsReopen' to start new streams for the next epoch

Errors from worker threads propagate to the parent via @async@ + @link@.
All errors are 'AppDatabaseError' with source location tracking.
-}
module DbSync.Db.Loader
  ( -- * Types
    LoaderStream (..)
  , HasLoaderStream (..)

    -- * Construction
  , mkLoaderStream
  , closeLoaderStream
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (TBQueue, lengthTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import DbSync.Db.Loader.Connection
  ( LoaderConnection (..)
  , beginStream
  , beginTransaction
  , closeLoaderConnection
  , commitTransaction
  , endStream
  , openLoaderConnection
  , writeStreamRow
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Error (throwInternal)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Multi-threaded loader-stream writer.
--
-- Each table has a dedicated 'TBQueue' and writer thread. The parser
-- thread dispatches encoded rows via 'lsWriteRow'. Commits are
-- coordinated via the sentinel\/barrier pattern in 'lsCommit'.
data LoaderStream = LoaderStream
  { lsWriteRow     :: !(Text -> ByteString -> IO ())
      -- ^ Append an encoded row to the named table's chunk buffer;
      -- crossing the ~64KB threshold hands the chunk to the writer.
  , lsCommit       :: !(IO ())
      -- ^ Epoch boundary: flush partial chunks, drain all queues,
      -- end streams, COMMIT
  , lsReopen       :: !(IO ())
      -- ^ Reopen streams for the next epoch (BEGIN + new loader stream)
  , lsClose        :: !(IO ())
      -- ^ Close all connections and stop writer threads
  , lsQueueDepths  :: !(IO [(Text, Natural, Natural)])
      -- ^ Snapshot of every per-table queue depth as
      -- @(tableName, currentDepth, capacity)@, denominated in
      -- ~64KB chunks. Used by diagnostic subsystems (the consumer
      -- pulse) to surface downstream backpressure.
  }

-- | Access the multi-threaded loader-stream writer from env. Implemented
-- by 'IngestEnv' only.
class HasLoaderStream env where
  getLoaderStream :: env -> LoaderStream

-- | Internal state for a single table's loader channel.
data LoaderChannel = LoaderChannel
  { chConnection :: !LoaderConnection
  , chQueue      :: !(TBQueue (Maybe ByteString))
      -- ^ 'Just chunk' = one or more concatenated encoded rows;
      -- 'Nothing' = sentinel (epoch boundary)
  , chBuffer     :: !(IORef ChunkBuffer)
      -- ^ Rows accumulated since the last chunk flush. Only the
      -- consumer thread touches it (the same thread that calls
      -- 'lsWriteRow' \/ 'lsCommit' \/ 'lsReopen'), so a plain
      -- 'IORef' suffices.
  , chWorker     :: !(IORef (Async ()))
      -- ^ Mutable so 'lsReopen' can swap in the next epoch's worker.
  , chReady      :: !(MVar ())
      -- ^ Writer signals here after draining on sentinel
  }

-- | Rows accumulated towards the next chunk, newest first.
data ChunkBuffer = ChunkBuffer
  { cbRows  :: ![ByteString]
  , cbBytes :: !Int
  }

emptyChunkBuffer :: ChunkBuffer
emptyChunkBuffer = ChunkBuffer [] 0

-- | Materialise a buffer into one wire chunk, restoring row order.
concatChunk :: ChunkBuffer -> ByteString
concatChunk = BS.concat . reverse . cbRows

-- | Chunk flush threshold. A row that overflows the threshold
-- flushes immediately (a single oversized row — e.g. a large
-- @tx_cbor@ — becomes a chunk of its own; the transport accepts any
-- size). libpq's internal COPY buffer flushes around 8KB, so 64KB
-- chunks also amortise its send-side work.
chunkBytes :: Int
chunkBytes = 64 * 1024

-- | Chunks a table's queue may hold before the producer blocks:
-- 64 × ~64KB ≈ 4MB per table regardless of row size, enough to keep
-- COPY fed across PostgreSQL latency spikes.
chunkQueueBound :: Natural
chunkQueueBound = 64

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Create a multi-threaded 'LoaderStream'.
--
-- Opens one loader connection per table, creates bounded 'TBQueue's,
-- and spawns writer threads. Each writer thread is linked to the
-- calling thread so exceptions propagate immediately.
mkLoaderStream :: HasCallStack => ByteString -> [TableDef] -> IO LoaderStream
mkLoaderStream connStr tableDefs = do
  channels <- forM tableDefs $ \td -> do
    bc     <- openLoaderConnection connStr td
    queue  <- newTBQueueIO chunkQueueBound
    buffer <- newIORef emptyChunkBuffer
    ready  <- newEmptyMVar
    worker <- async $ streamWorkerLoop bc queue ready
    link worker  -- propagate worker exceptions to parent
    workerRef <- newIORef worker
    pure (tdName td, LoaderChannel bc queue buffer workerRef ready)

  let channelMap = Map.fromList channels
      tableNames = map fst channels

  pure LoaderStream
    { lsWriteRow = \tableName rowBytes ->
        case Map.lookup tableName channelMap of
          Nothing ->
            throwInternal $
              "LoaderStream: unknown table '" <> tableName <> "'"
          Just ch -> do
            buf <- readIORef (chBuffer ch)
            let !bytes = cbBytes buf + BS.length rowBytes
                !buf'  = ChunkBuffer (rowBytes : cbRows buf) bytes
            if bytes >= chunkBytes
              then do
                writeIORef (chBuffer ch) emptyChunkBuffer
                atomically $ writeTBQueue (chQueue ch) (Just (concatChunk buf'))
              else writeIORef (chBuffer ch) buf'

    , lsCommit = do
        -- After a prior lsCommit and before the next lsReopen the
        -- writer threads have exited (post putMVar) and their
        -- replacements have not been spawned yet. A sentinel sent
        -- in that window has no consumer, so takeMVar would block
        -- forever — and there is nothing to commit either, since
        -- the prior lsCommit already drained and committed. Skip.
        allLive <- fmap and . forM (Map.elems channelMap) $ \ch -> do
          w <- readIORef (chWorker ch)
          isNothing <$> poll w
        when allLive $ do
          -- 1. Flush each table's partial chunk, then send the sentinel
          forM_ channelMap $ \ch -> do
            flushChannel ch
            atomically $ writeTBQueue (chQueue ch) Nothing
          -- 2. Wait for all writers to signal ready (drained + endStream)
          forM_ channelMap $ \ch ->
            takeMVar (chReady ch)
          -- 3. COMMIT on all connections
          forM_ channelMap $ \ch ->
            commitTransaction (chConnection ch)

    , lsReopen = do
        -- Begin new transaction + loader stream on each connection.
        -- Buffers are empty here (lsCommit flushed them); the reset
        -- is defensive.
        forM_ channelMap $ \ch -> do
          writeIORef (chBuffer ch) emptyChunkBuffer
          beginTransaction (chConnection ch)
          beginStream (chConnection ch)
        -- Restart worker threads (old ones exited after sentinel)
        forM_ channelMap $ \ch -> do
          worker' <- async $
            streamWorkerLoop (chConnection ch) (chQueue ch) (chReady ch)
          link worker'
          writeIORef (chWorker ch) worker'

    , lsClose = do
        -- Cancel all workers and close connections
        forM_ channelMap $ \ch -> do
          readIORef (chWorker ch) >>= cancel
          closeLoaderConnection (chConnection ch)

    , lsQueueDepths =
        for tableNames $ \name -> do
          cur <- case Map.lookup name channelMap of
            Just ch -> atomically (lengthTBQueue (chQueue ch))
            Nothing -> pure 0
          pure (name, cur, chunkQueueBound)
    }

-- | Hand the channel's partial chunk to its writer, if non-empty.
flushChannel :: LoaderChannel -> IO ()
flushChannel ch = do
  buf <- readIORef (chBuffer ch)
  unless (null (cbRows buf)) $ do
    writeIORef (chBuffer ch) emptyChunkBuffer
    atomically $ writeTBQueue (chQueue ch) (Just (concatChunk buf))

-- | Close the 'LoaderStream', cancelling all threads and releasing connections.
closeLoaderStream :: LoaderStream -> IO ()
closeLoaderStream = lsClose

-- ---------------------------------------------------------------------------
-- * Worker thread
-- ---------------------------------------------------------------------------

-- | Per-table writer thread loop.
--
-- Drains the 'TBQueue' and writes each chunk via 'writeStreamRow'.
-- On receiving 'Nothing' (sentinel), calls 'endStream' to close the
-- current stream and signals readiness on the 'MVar'.
streamWorkerLoop :: LoaderConnection -> TBQueue (Maybe ByteString) -> MVar () -> IO ()
streamWorkerLoop bc queue ready = go
  where
    go = do
      mChunk <- atomically $ readTBQueue queue
      case mChunk of
        Nothing -> do
          -- Sentinel received: end stream and signal ready
          endStream bc
          putMVar ready ()
        Just chunk -> do
          writeStreamRow bc chunk
          go
