{-# LANGUAGE OverloadedStrings #-}

-- | Multi-threaded loader-stream writer with per-table 'TBQueue'
-- fan-out.
--
-- Each table owns a bounded queue, a writer thread and a loader
-- connection. 'lsWriteRow' batches rows into ~64KB chunks, so the
-- queues and the transport see one element per chunk, not per row.
-- Worker exceptions reach the parent through @async@ + @link@.
module DbSync.Db.Loader
  ( -- * Types
    LoaderStream (..)
  , HasLoaderStream (..)

    -- * Construction
  , mkLoaderStream
  , closeLoaderStream
  ) where

import Cardano.Prelude

import Control.Concurrent.Async (mapConcurrently_)
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

-- | Handle the consumer thread drives. Every operation here runs on
-- that one thread; the writer threads stay behind the queues.
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

-- | Implemented by 'IngestEnv' only.
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
      -- consumer thread touches it, so a plain 'IORef' suffices.
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

-- | Open one loader connection, queue and writer thread per table.
mkLoaderStream :: ByteString -> [TableDef] -> IO LoaderStream
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
        -- Between an lsCommit and the next lsReopen the writer
        -- threads have exited and their replacements do not exist
        -- yet. A sentinel sent in that window has no consumer, so
        -- takeMVar would block forever. Nothing is pending either,
        -- so skip.
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
          -- 3. COMMIT on all connections; safe concurrently because
          -- each channel owns its connection and its writer exited.
          mapConcurrently_ (commitTransaction . chConnection)
            (Map.elems channelMap)

    , lsReopen = do
        -- Begin new transaction + loader stream on each connection.
        -- Buffers are empty here (lsCommit flushed them); the reset
        -- is defensive.
        flip mapConcurrently_ (Map.elems channelMap) $ \ch -> do
          writeIORef (chBuffer ch) emptyChunkBuffer
          beginTransaction (chConnection ch)
          beginStream (chConnection ch)
        -- Restart worker threads (old ones exited after sentinel);
        -- link must run on the consumer thread for exceptions to
        -- propagate to it.
        forM_ channelMap $ \ch -> do
          worker' <- async $
            streamWorkerLoop (chConnection ch) (chQueue ch) (chReady ch)
          link worker'
          writeIORef (chWorker ch) worker'

    , lsClose = do
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

closeLoaderStream :: LoaderStream -> IO ()
closeLoaderStream = lsClose

-- ---------------------------------------------------------------------------
-- * Worker thread
-- ---------------------------------------------------------------------------

-- | Per-table writer thread loop. The sentinel ends the stream and
-- signals the barrier; the thread then exits until 'lsReopen'.
streamWorkerLoop :: LoaderConnection -> TBQueue (Maybe ByteString) -> MVar () -> IO ()
streamWorkerLoop bc queue ready = go
  where
    go = do
      mChunk <- atomically $ readTBQueue queue
      case mChunk of
        Nothing -> do
          endStream bc
          putMVar ready ()
        Just chunk -> do
          writeStreamRow bc chunk
          go
