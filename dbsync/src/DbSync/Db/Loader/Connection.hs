{-# LANGUAGE OverloadedStrings #-}

-- | Low-level loader-stream connection management.
--
-- Each table gets its own dedicated connection running a loader-stream
-- session. The current implementation drives PostgreSQL's @COPY FROM
-- STDIN@ over @postgresql-libpq@; the @beginStream@\/@writeStreamRow@
-- \/@endStream@ vocabulary insulates callers from that detail so a
-- future variant (binary COPY, batched prepared INSERTs, a different
-- DB) can be swapped in without rippling through the call sites.
--
-- Errors are thrown as 'AppDatabaseError' with source location tracking.
module DbSync.Db.Loader.Connection
  ( -- * Types
    LoaderConnection (..)

    -- * Connection lifecycle
  , openLoaderConnection
  , closeLoaderConnection

    -- * Stream operations
  , beginStream
  , writeStreamRow
  , endStream

    -- * Transaction control
  , beginTransaction
  , commitTransaction
  ) where

import Cardano.Prelude hiding (handle)

import qualified Data.ByteString.Char8 as BS8

import qualified Database.PostgreSQL.LibPQ as PQ

import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Loader (copyFromStdinSql, copyableColumnList)
import DbSync.Db.Statement.Transaction (beginSqlBs, commitSqlBs)
import DbSync.Error (throwDb)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | A connection dedicated to loader streaming for a single table.
data LoaderConnection = LoaderConnection
  { bcConnection :: !PQ.Connection
  , bcTableName  :: !Text
  , bcColumnList :: !ByteString
      -- ^ Pre-built column list: @"id", "hash", "epoch_no", ...@
  }

-- ---------------------------------------------------------------------------
-- * Connection lifecycle
-- ---------------------------------------------------------------------------

-- | Open a new @libpq@ connection and prepare it for streaming.
--
-- Connects to PostgreSQL, begins a transaction, and starts a
-- @COPY tablename (columns) FROM STDIN@ stream.
openLoaderConnection :: HasCallStack => ByteString -> TableDef -> IO LoaderConnection
openLoaderConnection connStr tableDef = do
  conn <- PQ.connectdb connStr
  connStatus <- PQ.status conn
  when (connStatus /= PQ.ConnectionOk) $ do
    errMsg <- PQ.errorMessage conn
    throwDb $
      "Failed to connect for loader stream on table "
      <> tdName tableDef <> ": "
      <> maybe "(no error message)" (toS . BS8.unpack) errMsg

  let colList = copyableColumnList tableDef
      bc = LoaderConnection
        { bcConnection = conn
        , bcTableName  = tdName tableDef
        , bcColumnList = colList
        }

  beginTransaction bc
  beginStream bc
  pure bc

-- | Close the @libpq@ connection and release resources.
closeLoaderConnection :: LoaderConnection -> IO ()
closeLoaderConnection bc = PQ.finish (bcConnection bc)

-- ---------------------------------------------------------------------------
-- * Stream operations
-- ---------------------------------------------------------------------------

-- | Start a loader-stream session on this connection.
--
-- The connection must be in a transaction (after 'beginTransaction')
-- and NOT already streaming. Today this issues a @COPY FROM STDIN@
-- statement.
beginStream :: HasCallStack => LoaderConnection -> IO ()
beginStream bc = do
  let sql = copyFromStdinSql (bcTableName bc) (bcColumnList bc)
  result <- PQ.exec (bcConnection bc) sql
  checkResult bc "beginStream" result

-- | Write a single encoded row to the stream.
--
-- The row must be tab-separated and newline-terminated (produced by
-- the encoder helpers in @DbSync.Db.Loader.Encoder@).
writeStreamRow :: HasCallStack => LoaderConnection -> ByteString -> IO ()
writeStreamRow bc rowBytes = do
  copyResult <- PQ.putCopyData (bcConnection bc) rowBytes
  case copyResult of
    PQ.CopyInOk -> pure ()
    PQ.CopyInError -> do
      errMsg <- PQ.errorMessage (bcConnection bc)
      throwDb $
        "putCopyData failed for table " <> bcTableName bc
        <> ": " <> maybe "(no error)" (toS . BS8.unpack) errMsg
    PQ.CopyInWouldBlock ->
      -- For synchronous connections this shouldn't happen, but handle it
      writeStreamRow bc rowBytes

-- | End the current loader stream.
--
-- Must be called before 'commitTransaction'. After this, the
-- connection is back in normal SQL mode.
endStream :: HasCallStack => LoaderConnection -> IO ()
endStream bc = do
  copyResult <- PQ.putCopyEnd (bcConnection bc) mempty
  case copyResult of
    PQ.CopyInOk -> do
      -- Must consume the result from putCopyEnd
      _result <- PQ.getResult (bcConnection bc)
      pure ()
    PQ.CopyInError -> do
      errMsg <- PQ.errorMessage (bcConnection bc)
      throwDb $
        "putCopyEnd failed for table " <> bcTableName bc
        <> ": " <> maybe "(no error)" (toS . BS8.unpack) errMsg
    PQ.CopyInWouldBlock ->
      endStream bc

-- ---------------------------------------------------------------------------
-- * Transaction control
-- ---------------------------------------------------------------------------

-- | Begin a transaction on this connection.
beginTransaction :: HasCallStack => LoaderConnection -> IO ()
beginTransaction bc = do
  result <- PQ.exec (bcConnection bc) beginSqlBs
  checkResult bc "BEGIN" result

-- | Commit the current transaction on this connection.
commitTransaction :: HasCallStack => LoaderConnection -> IO ()
commitTransaction bc = do
  result <- PQ.exec (bcConnection bc) commitSqlBs
  checkResult bc "COMMIT" result

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Check that a @libpq@ result is not an error.
checkResult :: HasCallStack => LoaderConnection -> Text -> Maybe PQ.Result -> IO ()
checkResult bc operation mResult = case mResult of
  Nothing -> do
    errMsg <- PQ.errorMessage (bcConnection bc)
    throwDb $
      operation <> " failed for table " <> bcTableName bc
      <> ": " <> maybe "(no result)" (toS . BS8.unpack) errMsg
  Just result -> do
    resultStatus <- PQ.resultStatus result
    case resultStatus of
      PQ.CommandOk -> pure ()
      PQ.CopyIn    -> pure ()  -- expected after COPY FROM STDIN
      PQ.TuplesOk  -> pure ()
      _other -> do
        errMsg <- PQ.resultErrorMessage result
        throwDb $
          operation <> " failed for table " <> bcTableName bc
          <> " (status: " <> show resultStatus <> "): "
          <> maybe "(no error)" (toS . BS8.unpack) errMsg
