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

    -- * Schema-state analysis (pure)
  , SchemaState (..)
  , SchemaMismatch (..)
  , SchemaAction (..)
  , analyzeSchemaState
  , decideSchemaAction
  , renderSchemaMismatch

    -- * psql helpers (exported for tests)
  , execPsql
  , queryPsql
  ) where

import Cardano.Prelude

import Data.List (lookup)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import System.IO.Error (userError)
import System.Process (readProcessWithExitCode)

import DbSync.Db.Schema.Generate (generateCreateTable)
import DbSync.Db.Schema.SyncState (syncStateTableDef, syncStateTableName)
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

-- | Observed state of the database schema, relative to the extractor versions
-- the running code expects.
--
-- Distinguishes the three boot-time scenarios:
--
--   * 'SchemaFresh' — no @schema_version@ table; this is a brand-new database
--     and the boot flow should run 'initSchema'.
--   * 'SchemaMatches' — every expected extractor is present at the expected
--     version; the boot flow should skip 'initSchema' and resume.
--   * 'SchemaMismatched' — at least one extractor disagrees; the boot flow
--     should abort and surface the discrepancies to the operator (unless
--     @--force-resync@ overrides).
data SchemaState
  = SchemaFresh
  | SchemaMatches
  | SchemaMismatched !(NonEmpty SchemaMismatch)
  deriving stock (Eq, Show)

-- | A single point of disagreement between expected (code) and observed (DB)
-- extractor versions.
data SchemaMismatch
  = -- | Code expects this extractor but the DB has no row for it.
    --   Fields: @(extractorName, expectedVersion)@.
    MissingExtractor !Text !Int
  | -- | DB is at a lower version than the code: re-sync is needed.
    --   Fields: @(extractorName, dbVersion, expectedVersion)@.
    VersionAhead !Text !Int !Int
  | -- | DB is at a higher version than the code: downgrade is not supported.
    --   Fields: @(extractorName, dbVersion, expectedVersion)@.
    VersionBehind !Text !Int !Int
  deriving stock (Eq, Show)

-- | The action the boot flow should take, given the observed schema state and
-- whether the operator passed @--force-resync@.
data SchemaAction
  = -- | Schema already matches; do not touch DDL.
    ActionSkipInit
  | -- | DB is empty; run 'initSchema' to create everything.
    ActionRunInit
  | -- | Operator forced a clean slate; drop everything (including
    -- @schema_version@) and re-run 'initSchema'.
    ActionForceReinit
  | -- | Schema mismatch and no force flag; the operator must intervene.
    ActionAbort !(NonEmpty SchemaMismatch)
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Schema lifecycle
-- ---------------------------------------------------------------------------

-- | Initialise the database schema.
--
-- 1. Creates the @schema_version@ table (if not exists)
-- 2. Drops any existing tables owned by the given 'TableDef's
--    (plus @dbsync_sync_state@).
-- 3. Creates the @dbsync_sync_state@ singleton metadata table
--    (LOGGED, constrained; see 'DbSync.Db.Schema.SyncState').
-- 4. Creates all data tables from the 'TableDef's via 'generateCreateTable'.
-- 5. Records extractor versions in @schema_version@.
--
-- This is idempotent — calling it twice on the same database is safe
-- because it drops and recreates. 'DbSync.Checkpoint.SyncState.seedSyncState'
-- is __not__ called here; seeding is the caller's responsibility so that
-- the @ledger_enabled@ flag comes from runtime configuration.
initSchema :: [TableDef] -> [(Text, Int)] -> Text -> IO ()
initSchema tableDefs extractorVersions connStr = do
  -- Always drop first for idempotency
  dropSchema tableDefs extractorVersions connStr

  -- Create the version tracking table
  execPsql connStr createVersionTableSQL

  -- Create the singleton sync-state table (LOGGED, constrained, with
  -- defaults). Its DDL is generated from its 'TableDef' exactly like
  -- the extractor tables, so it picks up the same column-ordering
  -- golden as the rest of the schema.
  execPsql connStr (generateCreateTable syncStateTableDef)

  -- Create all data tables
  let ddlStatements = map generateCreateTable tableDefs
      allDDL = T.unlines ddlStatements
  execPsql connStr allDDL

  -- Record extractor versions
  forM_ extractorVersions $ \(name, ver) ->
    execPsql connStr $ insertVersionSQL name ver

-- | Drop all tables owned by the given 'TableDef's, the
-- @dbsync_sync_state@ singleton, and their @schema_version@ entries.
--
-- Safe to call on an empty database (uses @IF EXISTS@).
dropSchema :: [TableDef] -> [(Text, Int)] -> Text -> IO ()
dropSchema tableDefs extractorVersions connStr = do
  -- Drop data tables
  forM_ tableDefs $ \td ->
    execPsql connStr $ "DROP TABLE IF EXISTS " <> quote (tdName td) <> " CASCADE;"

  -- Drop the sync-state table too so tests can start fresh
  execPsql connStr $
    "DROP TABLE IF EXISTS " <> quote syncStateTableName <> " CASCADE;"

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
-- * Schema-state analysis (pure)
-- ---------------------------------------------------------------------------

-- | Pure analysis of schema state given the extractors the code expects and
-- the rows observed in the database.
--
-- @Nothing@ for the second argument means the @schema_version@ table itself
-- does not exist (a fresh DB). @Just rows@ means the table is present and
-- @rows@ are the @(name, version)@ pairs read from it.
--
-- Extra extractors in @rows@ that are not in the expected list are
-- silently ignored — operators are allowed to remove an extractor from
-- their profile without re-syncing the rest.
analyzeSchemaState
  :: [(Text, Int)]            -- ^ Expected: @(extractorName, expectedVersion)@
  -> Maybe [(Text, Int)]      -- ^ Observed DB rows; 'Nothing' = table missing
  -> SchemaState
analyzeSchemaState _ Nothing = SchemaFresh
analyzeSchemaState expected (Just dbRows) =
  case mapMaybe (compareOne dbRows) expected of
    []       -> SchemaMatches
    (m : ms) -> SchemaMismatched (m NE.:| ms)
  where
    compareOne :: [(Text, Int)] -> (Text, Int) -> Maybe SchemaMismatch
    compareOne dbVersions (name, expectedVer) =
      case lookup name dbVersions of
        Nothing -> Just (MissingExtractor name expectedVer)
        Just dbVer
          | dbVer == expectedVer -> Nothing
          | dbVer <  expectedVer -> Just (VersionAhead  name dbVer expectedVer)
          | otherwise            -> Just (VersionBehind name dbVer expectedVer)

-- | Decide what the boot flow should do, given the observed schema state and
-- the operator-supplied @--force-resync@ flag.
--
-- 'True' for @--force-resync@ short-circuits everything: the operator has
-- explicitly asked for a clean slate.
decideSchemaAction :: Bool -> SchemaState -> SchemaAction
decideSchemaAction True  _                       = ActionForceReinit
decideSchemaAction False SchemaMatches           = ActionSkipInit
decideSchemaAction False SchemaFresh             = ActionRunInit
decideSchemaAction False (SchemaMismatched errs) = ActionAbort errs

-- | Render a single 'SchemaMismatch' as a human-readable line suitable for
-- logging. Stable wording so operators can grep for it.
renderSchemaMismatch :: SchemaMismatch -> Text
renderSchemaMismatch = \case
  MissingExtractor name ver ->
    "Extractor '" <> name <> "' v" <> show ver
      <> " is expected but not present in the database."
  VersionAhead name dbVer codeVer ->
    "Extractor '" <> name <> "': database has v" <> show dbVer
      <> ", code expects v" <> show codeVer
      <> ". Re-sync required."
  VersionBehind name dbVer codeVer ->
    "Extractor '" <> name <> "': database has v" <> show dbVer
      <> " but code only supports v" <> show codeVer
      <> ". Downgrade not supported."

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
  (exitCode, _out, err) <- readProcessWithExitCode
    "psql"
    [T.unpack connStr, "-q", "-c", T.unpack sql]
    ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      -- Silently ignore "table does not exist" errors from DROP IF EXISTS
      -- and "relation does not exist" from DELETE on missing table
      if "does not exist" `isInfixOf` err
         || "ERROR" `notInfixOf` err
        then pure ()
        else throwIO $ userError $
          "psql failed: " <> err <> "\nSQL: " <> T.unpack sql
  where
    notInfixOf needle haystack = not (needle `isInfixOf` haystack)

-- | Run a query via @psql@ and return the output as 'Text'.
--
-- Uses @-t@ (tuples only, no header/footer), @-A@ (unaligned),
-- and @-F \"|\"@ (pipe field separator) for clean, parseable output.
queryPsql :: Text -> Text -> IO Text
queryPsql connStr sql = do
  (exitCode, out, err) <- readProcessWithExitCode
    "psql"
    [T.unpack connStr, "-t", "-A", "-F", "|", "-c", T.unpack sql]
    ""
  case exitCode of
    ExitSuccess -> pure (T.pack out)
    ExitFailure _ ->
      throwIO $ userError $
        "psql query failed: " <> err <> "\nSQL: " <> T.unpack sql

-- ---------------------------------------------------------------------------
-- * Internal helpers
-- ---------------------------------------------------------------------------

-- | Quote a SQL identifier with double quotes.
quote :: Text -> Text
quote name = "\"" <> name <> "\""

-- | Quote a SQL string literal with single quotes (escaping internal quotes).
quoteLiteral :: Text -> Text
quoteLiteral val = "'" <> T.replace "'" "''" val <> "'"
