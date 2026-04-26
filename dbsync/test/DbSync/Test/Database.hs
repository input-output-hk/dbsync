{-# LANGUAGE OverloadedStrings #-}

-- | Database test helpers.
--
-- Provides functions to create and drop the test database, and a bracket
-- pattern for test suites that need a clean database. Connects via
-- @template1@ (the PostgreSQL maintenance database) to issue CREATE/DROP.
--
-- The test database name defaults to @dbsync_test@ but can be overridden
-- via the @DBSYNC_TEST_DB@ environment variable.
--
-- For CI: ensure PostgreSQL is running and the current user has CREATEDB
-- privileges.
module DbSync.Test.Database
  ( -- * Database lifecycle
    createTestDatabase
  , dropTestDatabase
  , withTestDatabase

    -- * Configuration
  , testDbName
  , testConnStr
  , testConnBs

    -- * Utilities
  , queryTestDb
  , execTestDb
  , truncateAllTables
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import System.IO.Error (userError)
import System.Process (readProcessWithExitCode)

-- ---------------------------------------------------------------------------
-- * Configuration
-- ---------------------------------------------------------------------------

-- | The test database name. Could be made configurable via env var in future.
testDbName :: Text
testDbName = "dbsync_test"

-- | The connection string for the test database (for @psql@ and @libpq@).
testConnStr :: Text
testConnStr = "dbname=" <> testDbName

-- | ByteString version of 'testConnStr' (for @libpq@).
testConnBs :: ByteString
testConnBs = TE.encodeUtf8 testConnStr

-- | The maintenance database used for CREATE/DROP DATABASE commands.
-- @template1@ is guaranteed to exist in all PostgreSQL installations.
maintenanceDb :: Text
maintenanceDb = "dbname=template1"

-- ---------------------------------------------------------------------------
-- * Database lifecycle
-- ---------------------------------------------------------------------------

-- | Create the test database. Drops it first if it already exists.
--
-- Connects to @template1@ to issue the DDL. Safe to call multiple times.
createTestDatabase :: IO ()
createTestDatabase = do
  dropTestDatabase
  execMaintenance $
    "CREATE DATABASE \"" <> testDbName <> "\";"

-- | Drop the test database if it exists.
--
-- Terminates any active connections first, then drops.
-- Connects to @template1@ to issue the DDL.
dropTestDatabase :: IO ()
dropTestDatabase = do
  -- Terminate existing connections (ignore errors if DB doesn't exist)
  execMaintenanceSilent $
    "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '"
    <> testDbName <> "' AND pid <> pg_backend_pid();"
  execMaintenanceSilent $
    "DROP DATABASE IF EXISTS \"" <> testDbName <> "\";"

-- | Bracket pattern: create a fresh test database, run the action, then drop it.
--
-- Use with HSpec's @around_@:
--
-- @
-- spec :: Spec
-- spec = around_ withTestDatabase $ do
--   it "does something with the DB" $ ...
-- @
withTestDatabase :: IO () -> IO ()
withTestDatabase action = do
  createTestDatabase
  action `finally` dropTestDatabase

-- ---------------------------------------------------------------------------
-- * Utilities
-- ---------------------------------------------------------------------------

-- | Run a SQL query against the test database and return the output.
-- Uses @psql -t -A -F \"|\"@ for clean, parseable output.
queryTestDb :: Text -> IO Text
queryTestDb sql = do
  (exitCode, out, err) <- readProcessWithExitCode
    "psql"
    [T.unpack testConnStr, "-t", "-A", "-F", "|", "-c", T.unpack sql]
    ""
  case exitCode of
    ExitSuccess -> pure (T.pack out)
    ExitFailure _ ->
      throwIO $ userError $
        "queryTestDb failed: " <> err <> "\nSQL: " <> T.unpack sql

-- | Execute a SQL statement against the test database (no output expected).
execTestDb :: Text -> IO ()
execTestDb sql = do
  (exitCode, _out, err) <- readProcessWithExitCode
    "psql"
    [T.unpack testConnStr, "-q", "-c", T.unpack sql]
    ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      throwIO $ userError $
        "execTestDb failed: " <> err <> "\nSQL: " <> T.unpack sql

-- | Truncate all tables in the test database.
-- Useful between tests when you don't want to drop/recreate the schema.
truncateAllTables :: [Text] -> IO ()
truncateAllTables tableNames =
  execTestDb $ "TRUNCATE TABLE " <> T.intercalate ", " tableNames <> " CASCADE;"

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Execute SQL against the maintenance database (@template1@).
execMaintenance :: Text -> IO ()
execMaintenance sql = do
  (exitCode, _out, err) <- readProcessWithExitCode
    "psql"
    [T.unpack maintenanceDb, "-q", "-c", T.unpack sql]
    ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      throwIO $ userError $
        "execMaintenance failed: " <> err <> "\nSQL: " <> T.unpack sql

-- | Execute SQL against the maintenance database, ignoring errors.
-- Used for cleanup operations where the target may not exist.
execMaintenanceSilent :: Text -> IO ()
execMaintenanceSilent sql = do
  void $ readProcessWithExitCode
    "psql"
    [T.unpack maintenanceDb, "-q", "-c", T.unpack sql]
    ""
