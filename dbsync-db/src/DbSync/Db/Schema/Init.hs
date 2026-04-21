{-# LANGUAGE OverloadedStrings #-}

-- | Schema initialisation and version tracking.
--
-- Creates database tables from 'TableDef' definitions using @psql@,
-- records extractor versions in a @schema_version@ table, and provides
-- version checking on startup.
--
-- During 'IngestChainHistory', this module is called once at startup
-- to create the UNLOGGED tables that COPY streams will write into.
module DbSync.Db.Schema.Init
  ( -- * Schema lifecycle
    initSchema
  , dropSchema

    -- * Version checking
  , checkSchemaVersions
  , SchemaVersionRow (..)

    -- * psql helpers (exported for tests)
  , execPsql
  , queryPsql
  ) where

import Cardano.Prelude

import Data.List (lookup)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import System.Exit (ExitCode (..))
import System.IO.Error (userError)
import System.Process (readProcessWithExitCode)

import DbSync.Db.Schema.Generate (generateCreateTable)
import DbSync.Db.Schema.Types (TableDef (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | A row from the @schema_version@ table.
data SchemaVersionRow = SchemaVersionRow
  { svrExtractorName :: !Text
  , svrVersion       :: !Int
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Schema lifecycle
-- ---------------------------------------------------------------------------

-- | Initialise the database schema.
--
-- 1. Creates the @schema_version@ table (if not exists)
-- 2. Drops any existing tables owned by the given 'TableDef's
-- 3. Creates all tables from the 'TableDef's via 'generateCreateTable'
-- 4. Records extractor versions in @schema_version@
--
-- This is idempotent — calling it twice on the same database is safe
-- because it drops and recreates.
initSchema :: [TableDef] -> [(Text, Int)] -> Text -> IO ()
initSchema tableDefs extractorVersions connStr = do
  -- Always drop first for idempotency
  dropSchema tableDefs extractorVersions connStr

  -- Create the version tracking table
  execPsql connStr createVersionTableSQL

  -- Create all data tables
  let ddlStatements = map generateCreateTable tableDefs
      allDDL = T.unlines ddlStatements
  execPsql connStr allDDL

  -- Record extractor versions
  forM_ extractorVersions $ \(name, ver) ->
    execPsql connStr $ insertVersionSQL name ver

-- | Drop all tables owned by the given 'TableDef's and their
-- @schema_version@ entries.
--
-- Safe to call on an empty database (uses @IF EXISTS@).
dropSchema :: [TableDef] -> [(Text, Int)] -> Text -> IO ()
dropSchema tableDefs extractorVersions connStr = do
  -- Drop data tables
  forM_ tableDefs $ \td ->
    execPsql connStr $ "DROP TABLE IF EXISTS " <> quote (tdName td) <> " CASCADE;"

  -- Clean up version entries (table may not exist yet)
  forM_ extractorVersions $ \(name, _) ->
    execPsql connStr $
      "DELETE FROM schema_version WHERE extractor_name = "
      <> quoteLiteral name <> ";"

-- ---------------------------------------------------------------------------
-- * Version checking
-- ---------------------------------------------------------------------------

-- | Check that all expected extractor versions match what is in the database.
--
-- Returns @Right ()@ if every entry in the expected list has a matching
-- row in @schema_version@. Returns @Left msg@ on any mismatch:
--
--   * Code version ahead of DB → needs migration or re-sync
--   * Extractor missing from DB → was never initialised
--   * DB version ahead of code → downgrade not supported
--
-- Extra extractors in the DB that are not in the expected list are
-- ignored (allows removing extractors from the profile without error).
checkSchemaVersions :: [(Text, Int)] -> Text -> IO (Either Text ())
checkSchemaVersions expectedVersions connStr = do
  -- Check if schema_version table exists
  tableExists <- queryPsql connStr
    "SELECT count(*) FROM pg_tables WHERE schemaname = 'public' AND tablename = 'schema_version';"

  if T.strip tableExists /= "1"
    then
      if null expectedVersions
        then pure (Right ())
        else pure (Left "schema_version table does not exist but extractors are expected")
    else do
      -- Query all versions from DB
      dbVersionsRaw <- queryPsql connStr
        "SELECT extractor_name, version FROM schema_version ORDER BY extractor_name;"

      let dbVersions = parseVersionRows dbVersionsRaw

      -- Check each expected version
      let mismatches = mapMaybe (checkOne dbVersions) expectedVersions
      if null mismatches
        then pure (Right ())
        else pure (Left $ T.unlines mismatches)

  where
    checkOne
      :: [(Text, Int)]  -- DB versions
      -> (Text, Int)    -- Expected (name, version)
      -> Maybe Text     -- Error message if mismatch
    checkOne dbVersions (name, expectedVer) =
      case Data.List.lookup name dbVersions of
        Nothing ->
          Just $ "Extractor '" <> name <> "' v" <> show expectedVer
            <> " not found in database"
        Just dbVer
          | dbVer == expectedVer -> Nothing
          | dbVer < expectedVer ->
              Just $ "Extractor '" <> name <> "' version mismatch: DB has v"
                <> show dbVer <> ", code expects v" <> show expectedVer
                <> ". Re-sync required."
          | otherwise ->
              Just $ "Extractor '" <> name <> "' version mismatch: DB has v"
                <> show dbVer <> " but code only supports v" <> show expectedVer
                <> ". Downgrade not supported."

    parseVersionRows :: Text -> [(Text, Int)]
    parseVersionRows raw =
      let ls = filter (not . T.null) $ T.lines (T.strip raw)
      in mapMaybe parseLine ls

    parseLine :: Text -> Maybe (Text, Int)
    parseLine line =
      case T.splitOn "|" line of
        [name, verStr] ->
          case readMaybe (T.unpack (T.strip verStr)) of
            Just v  -> Just (T.strip name, v)
            Nothing -> Nothing
        _ -> Nothing

-- ---------------------------------------------------------------------------
-- * SQL templates
-- ---------------------------------------------------------------------------

-- | DDL for the @schema_version@ table.
createVersionTableSQL :: Text
createVersionTableSQL = T.unlines
  [ "CREATE TABLE IF NOT EXISTS \"schema_version\" ("
  , "  \"extractor_name\" TEXT NOT NULL,"
  , "  \"version\" INTEGER NOT NULL,"
  , "  \"created_at\" TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now()"
  , ");"
  ]

-- | INSERT a version row.
insertVersionSQL :: Text -> Int -> Text
insertVersionSQL name ver =
  "INSERT INTO schema_version (extractor_name, version) VALUES ("
  <> quoteLiteral name <> ", " <> show ver <> ");"

-- ---------------------------------------------------------------------------
-- * psql helpers
-- ---------------------------------------------------------------------------

-- | Execute a SQL statement via @psql@. Throws on failure.
execPsql :: Text -> Text -> IO ()
execPsql connStr sql = do
  (exitCode, _stdout, stderr) <- readProcessWithExitCode
    "psql"
    [T.unpack connStr, "-q", "-c", T.unpack sql]
    ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      -- Silently ignore "table does not exist" errors from DROP IF EXISTS
      -- and "relation does not exist" from DELETE on missing table
      if "does not exist" `isInfixOf` stderr
         || "ERROR" `notInfixOf` stderr
        then pure ()
        else throwIO $ userError $
          "psql failed: " <> stderr <> "\nSQL: " <> T.unpack sql
  where
    notInfixOf needle haystack = not (needle `isInfixOf` haystack)

-- | Run a query via @psql@ and return the output as 'Text'.
--
-- Uses @-t@ (tuples only, no header/footer), @-A@ (unaligned),
-- and @-F \"|\"@ (pipe field separator) for clean, parseable output.
queryPsql :: Text -> Text -> IO Text
queryPsql connStr sql = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "psql"
    [T.unpack connStr, "-t", "-A", "-F", "|", "-c", T.unpack sql]
    ""
  case exitCode of
    ExitSuccess -> pure (T.pack stdout)
    ExitFailure _ ->
      throwIO $ userError $
        "psql query failed: " <> stderr <> "\nSQL: " <> T.unpack sql

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Quote a SQL identifier with double quotes.
quote :: Text -> Text
quote name = "\"" <> name <> "\""

-- | Quote a SQL string literal with single quotes (escaping internal quotes).
quoteLiteral :: Text -> Text
quoteLiteral val = "'" <> T.replace "'" "''" val <> "'"
