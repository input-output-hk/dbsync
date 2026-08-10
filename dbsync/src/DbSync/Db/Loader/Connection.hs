{-# LANGUAGE OverloadedStrings #-}

-- | Low-level loader-stream connection management.
--
-- Each table gets a dedicated connection running PostgreSQL's
-- @COPY FROM STDIN@ over @postgresql-libpq@. The
-- @beginStream@\/@writeStreamRow@\/@endStream@ vocabulary hides that
-- transport. Failures throw 'AppDatabaseError'.
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

-- | Connect, begin a transaction, and start the COPY stream. The
-- connection is ready for 'writeStreamRow' on return.
openLoaderConnection :: ByteString -> TableDef -> IO LoaderConnection
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

closeLoaderConnection :: LoaderConnection -> IO ()
closeLoaderConnection bc = PQ.finish (bcConnection bc)

-- ---------------------------------------------------------------------------
-- * Stream operations
-- ---------------------------------------------------------------------------

-- | The connection must already be in a transaction and must not be
-- streaming.
beginStream :: LoaderConnection -> IO ()
beginStream bc = do
  let sql = copyFromStdinSql (bcTableName bc) (bcColumnList bc)
  result <- PQ.exec (bcConnection bc) sql
  checkResult bc "beginStream" result

-- | Write a chunk of one or more encoded rows to the stream.
--
-- The rows use COPY text format: tab-separated, newline-terminated,
-- as produced by @DbSync.Db.Loader.Encoder@. The protocol does not
-- require chunks to be row-aligned, so any concatenation of complete
-- rows is valid.
writeStreamRow :: LoaderConnection -> ByteString -> IO ()
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
      -- Unreachable on a synchronous connection, but retry anyway.
      writeStreamRow bc rowBytes

-- | End the current stream, which must happen before
-- 'commitTransaction'. The connection returns to normal SQL mode.
--
-- A /server-side/ COPY failure surfaces here. @libpq@ buffers rows
-- locally, so the server only reports a rejected row in the final
-- result after the stream ends. An unchecked result would let the
-- COMMIT run inside an aborted transaction, which PostgreSQL executes
-- as ROLLBACK while still reporting success, and every row of the
-- stream would vanish.
endStream :: LoaderConnection -> IO ()
endStream bc = do
  copyResult <- PQ.putCopyEnd (bcConnection bc) mempty
  case copyResult of
    PQ.CopyInOk -> drainResults
    PQ.CopyInError -> do
      errMsg <- PQ.errorMessage (bcConnection bc)
      throwDb $
        "putCopyEnd failed for table " <> bcTableName bc
        <> ": " <> maybe "(no error)" (toS . BS8.unpack) errMsg
    PQ.CopyInWouldBlock ->
      endStream bc
  where
    -- Consume every pending result. The COPY command's final status
    -- must be CommandOk. Anything else means the server rejected the
    -- stream and the transaction is already aborted.
    drainResults :: IO ()
    drainResults = do
      mResult <- PQ.getResult (bcConnection bc)
      for_ mResult $ \result -> do
        status <- PQ.resultStatus result
        case status of
          PQ.CommandOk -> drainResults
          _ -> do
            errMsg <- PQ.resultErrorMessage result
            throwDb $
              "COPY stream for table " <> bcTableName bc
              <> " failed server-side (status: " <> show status <> "): "
              <> maybe "(no error)" (toS . BS8.unpack) errMsg

-- ---------------------------------------------------------------------------
-- * Transaction control
-- ---------------------------------------------------------------------------

beginTransaction :: LoaderConnection -> IO ()
beginTransaction bc = do
  result <- PQ.exec (bcConnection bc) beginSqlBs
  checkResult bc "BEGIN" result

commitTransaction :: LoaderConnection -> IO ()
commitTransaction bc = do
  result <- PQ.exec (bcConnection bc) commitSqlBs
  checkResult bc "COMMIT" result

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

checkResult :: LoaderConnection -> Text -> Maybe PQ.Result -> IO ()
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
