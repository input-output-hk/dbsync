{-# LANGUAGE OverloadedStrings #-}

-- | Low-level COPY connection management via @postgresql-libpq@.
--
-- Each table gets its own @libpq@ connection running @COPY FROM STDIN@.
-- This module provides functions to open, write to, close, and reopen
-- COPY streams, as well as commit and begin transactions.
--
-- Errors are thrown as 'AppDatabaseError' with source location tracking.
module DbSync.Copy.Connection
  ( -- * Types
    CopyConnection (..)

    -- * Connection lifecycle
  , openCopyConnection
  , closeCopyConnection

    -- * COPY stream operations
  , beginCopy
  , writeCopyData
  , endCopy

    -- * Transaction control
  , beginTransaction
  , commitTransaction
  ) where

import Cardano.Prelude hiding (handle)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TE

import qualified Database.PostgreSQL.LibPQ as PQ

import DbSync.Db.Schema.Types (ColumnDef (..), TableDef (..))
import DbSync.Error (throwDb)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | A connection dedicated to COPY streaming for a single table.
data CopyConnection = CopyConnection
  { ccConnection :: !PQ.Connection
  , ccTableName  :: !Text
  , ccColumnList :: !ByteString
      -- ^ Pre-built column list: @"id", "hash", "epoch_no", ...@
  }

-- ---------------------------------------------------------------------------
-- * Connection lifecycle
-- ---------------------------------------------------------------------------

-- | Open a new @libpq@ connection and prepare it for COPY streaming.
--
-- Connects to PostgreSQL, begins a transaction, and starts a
-- @COPY tablename (columns) FROM STDIN@ stream.
openCopyConnection :: HasCallStack => ByteString -> TableDef -> IO CopyConnection
openCopyConnection connStr tableDef = do
  conn <- PQ.connectdb connStr
  connStatus <- PQ.status conn
  when (connStatus /= PQ.ConnectionOk) $ do
    errMsg <- PQ.errorMessage conn
    throwDb $
      "Failed to connect for COPY on table "
      <> tdName tableDef <> ": "
      <> maybe "(no error message)" (toS . BS8.unpack) errMsg

  let colList = buildColumnList tableDef
      cc = CopyConnection
        { ccConnection = conn
        , ccTableName  = tdName tableDef
        , ccColumnList = colList
        }

  beginTransaction cc
  beginCopy cc
  pure cc

-- | Close the @libpq@ connection and release resources.
closeCopyConnection :: CopyConnection -> IO ()
closeCopyConnection cc = PQ.finish (ccConnection cc)

-- ---------------------------------------------------------------------------
-- * COPY stream operations
-- ---------------------------------------------------------------------------

-- | Start a @COPY FROM STDIN@ stream on this connection.
--
-- The connection must be in a transaction (after 'beginTransaction')
-- and NOT already in COPY mode.
beginCopy :: HasCallStack => CopyConnection -> IO ()
beginCopy cc = do
  let sql = "COPY \"" <> TE.encodeUtf8 (ccTableName cc)
            <> "\" (" <> ccColumnList cc <> ") FROM STDIN"
  result <- PQ.exec (ccConnection cc) sql
  checkResult cc "beginCopy" result

-- | Write a single COPY-encoded row to the stream.
--
-- The row must be tab-separated and newline-terminated (produced by
-- the COPY encoder functions in @DbSync.Db.Schema.Core@).
writeCopyData :: HasCallStack => CopyConnection -> ByteString -> IO ()
writeCopyData cc rowBytes = do
  copyResult <- PQ.putCopyData (ccConnection cc) rowBytes
  case copyResult of
    PQ.CopyInOk -> pure ()
    PQ.CopyInError -> do
      errMsg <- PQ.errorMessage (ccConnection cc)
      throwDb $
        "putCopyData failed for table " <> ccTableName cc
        <> ": " <> maybe "(no error)" (toS . BS8.unpack) errMsg
    PQ.CopyInWouldBlock ->
      -- For synchronous connections this shouldn't happen, but handle it
      writeCopyData cc rowBytes

-- | End the current COPY stream.
--
-- Must be called before 'commitTransaction'. After this, the connection
-- is back in normal SQL mode.
endCopy :: HasCallStack => CopyConnection -> IO ()
endCopy cc = do
  copyResult <- PQ.putCopyEnd (ccConnection cc) mempty
  case copyResult of
    PQ.CopyInOk -> do
      -- Must consume the result from putCopyEnd
      _result <- PQ.getResult (ccConnection cc)
      pure ()
    PQ.CopyInError -> do
      errMsg <- PQ.errorMessage (ccConnection cc)
      throwDb $
        "putCopyEnd failed for table " <> ccTableName cc
        <> ": " <> maybe "(no error)" (toS . BS8.unpack) errMsg
    PQ.CopyInWouldBlock ->
      endCopy cc

-- ---------------------------------------------------------------------------
-- * Transaction control
-- ---------------------------------------------------------------------------

-- | Begin a transaction on this connection.
beginTransaction :: HasCallStack => CopyConnection -> IO ()
beginTransaction cc = do
  result <- PQ.exec (ccConnection cc) "BEGIN"
  checkResult cc "BEGIN" result

-- | Commit the current transaction on this connection.
commitTransaction :: HasCallStack => CopyConnection -> IO ()
commitTransaction cc = do
  result <- PQ.exec (ccConnection cc) "COMMIT"
  checkResult cc "COMMIT" result

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Build the column list for a COPY statement from a 'TableDef'.
-- E.g. @"id", "hash", "epoch_no", "slot_no", ...@
buildColumnList :: TableDef -> ByteString
buildColumnList td =
  BS.intercalate ", " $
    map (TE.encodeUtf8 . (\c -> "\"" <> cdName c <> "\"")) (tdColumns td)

-- | Check that a @libpq@ result is not an error.
checkResult :: HasCallStack => CopyConnection -> Text -> Maybe PQ.Result -> IO ()
checkResult cc operation mResult = case mResult of
  Nothing -> do
    errMsg <- PQ.errorMessage (ccConnection cc)
    throwDb $
      operation <> " failed for table " <> ccTableName cc
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
          operation <> " failed for table " <> ccTableName cc
          <> " (status: " <> show resultStatus <> "): "
          <> maybe "(no error)" (toS . BS8.unpack) errMsg
