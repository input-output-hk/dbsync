{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbSync.Copy.Writer
Description : Multi-threaded COPY writer with per-table TBQueue fan-out.

The 'CopyWriter' streams COPY-encoded rows to PostgreSQL via per-table
writer threads. The parser thread writes to bounded 'TBQueue's (one per
table); each writer thread drains its queue and calls @putCopyData@ on
its dedicated @libpq@ connection.

Epoch-aligned commits use a sentinel\/barrier pattern:

  1. Parser writes 'Nothing' to all queues
  2. Each writer drains remaining rows, calls @endCopy@, signals ready
  3. Parser waits for all writers, then @COMMIT@ on all connections
  4. Parser calls 'cwReopen' to start new COPY streams for the next epoch

Errors from worker threads propagate to the parent via @async@ + @link@.
All errors are 'AppDatabaseError' with source location tracking.
-}
module DbSync.Copy.Writer
  ( -- * Types
    CopyWriter (..)

    -- * Construction
  , mkCopyWriter
  , closeCopyWriter
  ) where

import Cardano.Prelude

import Control.Concurrent.Async (Async, async, cancel, link)
import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, readTBQueue, writeTBQueue)

import qualified Data.Map.Strict as Map

import DbSync.Copy.Connection
  ( CopyConnection (..)
  , beginCopy
  , beginTransaction
  , closeCopyConnection
  , commitTransaction
  , endCopy
  , openCopyConnection
  , writeCopyData
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Error (AppError (..), throwAppError)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Multi-threaded COPY writer.
--
-- Each table has a dedicated 'TBQueue' and writer thread. The parser
-- thread dispatches COPY-encoded rows via 'cwWriteRow'. Commits are
-- coordinated via the sentinel\/barrier pattern in 'cwCommit'.
data CopyWriter = CopyWriter
  { cwWriteRow :: !(Text -> ByteString -> IO ())
      -- ^ Dispatch a COPY-encoded row to the named table's queue
  , cwCommit   :: !(IO ())
      -- ^ Epoch boundary: drain all queues, endCopy, COMMIT
  , cwReopen   :: !(IO ())
      -- ^ Reopen COPY streams for the next epoch (BEGIN + COPY FROM STDIN)
  , cwClose    :: !(IO ())
      -- ^ Close all connections and stop writer threads
  }

-- | Internal state for a single table's COPY channel.
data CopyChannel = CopyChannel
  { chConnection :: !CopyConnection
  , chQueue      :: !(TBQueue (Maybe ByteString))
      -- ^ 'Just row' = data row; 'Nothing' = sentinel (epoch boundary)
  , chWorker     :: !(Async ())
  , chReady      :: !(MVar ())
      -- ^ Writer signals here after draining on sentinel
  }

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Create a multi-threaded 'CopyWriter'.
--
-- Opens one @libpq@ connection per table, creates bounded 'TBQueue's,
-- and spawns writer threads. Each writer thread is linked to the calling
-- thread so exceptions propagate immediately.
mkCopyWriter :: HasCallStack => ByteString -> [TableDef] -> IO CopyWriter
mkCopyWriter connStr tableDefs = do
  channels <- forM tableDefs $ \td -> do
    cc    <- openCopyConnection connStr td
    let queueBound = tableQueueBound (tdName td)
    queue <- newTBQueueIO queueBound
    ready <- newEmptyMVar
    worker <- async $ copyWorkerLoop cc queue ready
    link worker  -- propagate worker exceptions to parent
    pure (tdName td, CopyChannel cc queue worker ready)

  let channelMap = Map.fromList channels

  pure CopyWriter
    { cwWriteRow = \tableName rowBytes ->
        case Map.lookup tableName channelMap of
          Nothing ->
            throwAppError AppInternalError $
              "CopyWriter: unknown table '" <> tableName <> "'"
          Just ch ->
            atomically $ writeTBQueue (chQueue ch) (Just rowBytes)

    , cwCommit = do
        -- 1. Send sentinel to all queues
        forM_ channelMap $ \ch ->
          atomically $ writeTBQueue (chQueue ch) Nothing
        -- 2. Wait for all writers to signal ready (drained + endCopy)
        forM_ channelMap $ \ch ->
          takeMVar (chReady ch)
        -- 3. COMMIT on all connections
        forM_ channelMap $ \ch ->
          commitTransaction (chConnection ch)

    , cwReopen = do
        -- Begin new transaction + COPY stream on each connection
        forM_ channelMap $ \ch -> do
          beginTransaction (chConnection ch)
          beginCopy (chConnection ch)
        -- Restart worker threads (old ones exited after sentinel)
        forM_ channelMap $ \ch -> do
          worker' <- async $
            copyWorkerLoop (chConnection ch) (chQueue ch) (chReady ch)
          link worker'

    , cwClose = do
        -- Cancel all workers and close connections
        forM_ channelMap $ \ch -> do
          cancel (chWorker ch)
          closeCopyConnection (chConnection ch)
    }

-- | Close the 'CopyWriter', cancelling all threads and releasing connections.
closeCopyWriter :: CopyWriter -> IO ()
closeCopyWriter = cwClose

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
-- Drains the 'TBQueue' and writes each row via 'writeCopyData'. On
-- receiving 'Nothing' (sentinel), calls 'endCopy' to close the
-- COPY stream and signals readiness on the 'MVar'.
copyWorkerLoop :: CopyConnection -> TBQueue (Maybe ByteString) -> MVar () -> IO ()
copyWorkerLoop cc queue ready = go
  where
    go = do
      mRow <- atomically $ readTBQueue queue
      case mRow of
        Nothing -> do
          -- Sentinel received: end COPY stream and signal ready
          endCopy cc
          putMVar ready ()
        Just rowBytes -> do
          writeCopyData cc rowBytes
          go
