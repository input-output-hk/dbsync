{-# LANGUAGE OverloadedStrings #-}

-- | Database test helpers.
--
-- The suite runs against an externally provisioned @dbsync_test@
-- database; these helpers connect to it, run SQL, and reset schema
-- state between specs. For CI: ensure PostgreSQL is running and
-- @dbsync_test@ exists.
module DbSync.Test.Database
  ( -- * Configuration
    testDbName
  , testConnStr
  , testConnBs
  , testHasqlSettings

    -- * Utilities
  , queryTestDb
  , execTestDb
  , truncateAllTables

    -- * Spec setup helpers
  , setupFollowTipSchema
  , teardownSchema
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hasql.Connection.Settings as Settings

import System.IO.Error (userError)
import System.Process (readProcessWithExitCode)

import DbSync.Db.Schema.Init (dropSchema, initSchema, prepareSchemaForFollowTip)
import DbSync.Db.Schema.Types (TableDef)

-- ---------------------------------------------------------------------------
-- * Configuration
-- ---------------------------------------------------------------------------

-- | The externally provisioned test database name.
testDbName :: Text
testDbName = "dbsync_test"

-- | The connection string for the test database (for @psql@ and @libpq@).
testConnStr :: Text
testConnStr = "dbname=" <> testDbName

-- | ByteString version of 'testConnStr' (for @libpq@).
testConnBs :: ByteString
testConnBs = TE.encodeUtf8 testConnStr

-- | Hasql connection settings for the test database. Relies on
-- libpq defaults for host\/port\/user, matching the 'testConnStr'
-- "dbname=…" format.
testHasqlSettings :: Settings.Settings
testHasqlSettings = Settings.dbname testDbName

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

-- | Truncate all tables in the test database. Resets owned
-- sequences so 'INSERT ... RETURNING id' starts at 1 each time.
truncateAllTables :: [Text] -> IO ()
truncateAllTables tableNames =
  execTestDb $
    "TRUNCATE TABLE " <> T.intercalate ", " tableNames
      <> " RESTART IDENTITY CASCADE;"

-- ---------------------------------------------------------------------------
-- * Spec setup helpers
-- ---------------------------------------------------------------------------

-- | Drop + init + flip-to-LOGGED + attach sequences. Used in
-- @beforeAll_@ for any Spec that exercises the FollowingChainTip
-- INSERT path.
setupFollowTipSchema :: [TableDef] -> IO ()
setupFollowTipSchema tables = do
  dropSchema tables testConnStr
  initSchema tables testConnStr
  prepareSchemaForFollowTip tables testConnStr

-- | Drop everything for use in @afterAll_@.
teardownSchema :: [TableDef] -> IO ()
teardownSchema tables = dropSchema tables testConnStr
