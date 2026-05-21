{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Db.Loader
Description : Multi-threaded loader-stream writer with per-table TBQueue fan-out.

The 'LoaderStream' streams encoded rows to PostgreSQL via per-table
writer threads. The parser thread writes to bounded 'TBQueue's (one
per table); each writer thread drains its queue and pushes rows down
its dedicated loader connection. Today that connection runs PostgreSQL's
@COPY FROM STDIN@ protocol via @libpq@; the public API of this module
deliberately hides that detail.

Epoch-aligned commits use a sentinel\/barrier pattern:

  1. Parser writes 'Nothing' to all queues
  2. Each writer drains remaining rows, ends its stream, signals ready
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

import Control.Concurrent.STM (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

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
  { lsWriteRow :: !(Text -> ByteString -> IO ())
      -- ^ Dispatch an encoded row to the named table's queue
  , lsCommit   :: !(IO ())
      -- ^ Epoch boundary: drain all queues, end streams, COMMIT
  , lsReopen   :: !(IO ())
      -- ^ Reopen streams for the next epoch (BEGIN + new loader stream)
  , lsClose    :: !(IO ())
      -- ^ Close all connections and stop writer threads
  }

-- | Access the multi-threaded loader-stream writer from env. Implemented
-- by 'IngestEnv' only.
class HasLoaderStream env where
  getLoaderStream :: env -> LoaderStream

-- | Internal state for a single table's loader channel.
data LoaderChannel = LoaderChannel
  { chConnection :: !LoaderConnection
  , chQueue      :: !(TBQueue (Maybe ByteString))
      -- ^ 'Just row' = data row; 'Nothing' = sentinel (epoch boundary)
  , chWorker     :: !(IORef (Async ()))
      -- ^ Mutable so 'lsReopen' can swap in the next epoch's worker.
  , chReady      :: !(MVar ())
      -- ^ Writer signals here after draining on sentinel
  }

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
    bc    <- openLoaderConnection connStr td
    let queueBound = tableQueueBound (tdName td)
    queue <- newTBQueueIO queueBound
    ready <- newEmptyMVar
    worker <- async $ streamWorkerLoop bc queue ready
    link worker  -- propagate worker exceptions to parent
    workerRef <- newIORef worker
    pure (tdName td, LoaderChannel bc queue workerRef ready)

  let channelMap = Map.fromList channels

  pure LoaderStream
    { lsWriteRow = \tableName rowBytes ->
        case Map.lookup tableName channelMap of
          Nothing ->
            throwInternal $
              "LoaderStream: unknown table '" <> tableName <> "'"
          Just ch ->
            atomically $ writeTBQueue (chQueue ch) (Just rowBytes)

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
          -- 1. Send sentinel to all queues
          forM_ channelMap $ \ch ->
            atomically $ writeTBQueue (chQueue ch) Nothing
          -- 2. Wait for all writers to signal ready (drained + endStream)
          forM_ channelMap $ \ch ->
            takeMVar (chReady ch)
          -- 3. COMMIT on all connections
          forM_ channelMap $ \ch ->
            commitTransaction (chConnection ch)

    , lsReopen = do
        -- Begin new transaction + loader stream on each connection
        forM_ channelMap $ \ch -> do
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
    }

-- | Close the 'LoaderStream', cancelling all threads and releasing connections.
closeLoaderStream :: LoaderStream -> IO ()
closeLoaderStream = lsClose

-- ---------------------------------------------------------------------------
-- * Worker thread
-- ---------------------------------------------------------------------------

-- | Per-table queue bounds. Large-data tables (@tx_cbor@, @tx_metadata@)
-- get smaller bounds to limit pinned memory accumulation from serialised
-- CBOR ByteStrings. Small-row tables keep high bounds for throughput.
--
-- Alonzo+ transactions average 10-50KB of CBOR each:
--
--   * @tx_cbor@:     ~50KB/row × 200 = ~10MB max
--   * @tx_metadata@: ~1KB/row  × 500 = ~500KB max
--   * other tables:  ~200B/row × 10K = ~2MB max
tableQueueBound :: Text -> Natural
tableQueueBound "tx_cbor"     = 200
tableQueueBound "tx_metadata" = 500
tableQueueBound _             = 10000

-- | Per-table writer thread loop.
--
-- Drains the 'TBQueue' and writes each row via 'writeStreamRow'. On
-- receiving 'Nothing' (sentinel), calls 'endStream' to close the
-- current stream and signals readiness on the 'MVar'.
streamWorkerLoop :: LoaderConnection -> TBQueue (Maybe ByteString) -> MVar () -> IO ()
streamWorkerLoop bc queue ready = go
  where
    go = do
      mRow <- atomically $ readTBQueue queue
      case mRow of
        Nothing -> do
          -- Sentinel received: end stream and signal ready
          endStream bc
          putMVar ready ()
        Just rowBytes -> do
          writeStreamRow bc rowBytes
          go
