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
  , prepareSchemaForFollowTip

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

    -- * Schema-flip DDL builders (pure)
  , prepareSchemaForFollowTipSql
  , perTableSchemaForFollowTipSql
  , setLoggedSql
  , createIdSequenceSql
  , attachIdDefaultSql
  , analyzeSql
  , vacuumSql

    -- * psql helpers (exported for tests)
  , execPsql
  , queryPsql

    -- * Server probes
  , showWalLevel
  ) where

import Cardano.Prelude

import Data.List (lookup)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import System.IO.Error (userError)
import System.Process (readProcessWithExitCode)

import DbSync.Db.Schema.EpochParamPending
  ( epochParamPendingTableDef
  , epochParamPendingTableName
  )
import DbSync.Db.Schema.Generate (generateCreateTable)
import DbSync.Db.Schema.SyncState (syncStateTableDef, syncStateTableName)
import DbSync.Db.Schema.Types (TableDef (..), TableMode (..))
import DbSync.Db.Sql (quoteIdent, quoteLiteral)

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
--     @--resync-from-genesis@ overrides).
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
-- whether the operator passed @--resync-from-genesis@.
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

-- | Initialise the database schema on a __fresh__ database.
--
-- 1. Creates the @schema_version@ table.
-- 2. Creates the @dbsync_sync_state@ singleton metadata table
--    (LOGGED, constrained; see 'DbSync.Db.Schema.SyncState').
-- 3. Creates the @epoch_param_pending@ system table used by the
--    ledger worker → PreparingForChainTip deposit-flush handshake
--    (LOGGED so it survives a crash between flush and sync-state
--    advance; truncated at end of 'PreparingForChainTip').
-- 4. Creates all data tables from the 'TableDef's via 'generateCreateTable'.
-- 5. Records extractor versions in @schema_version@.
--
-- __Not idempotent__: this function expects the database to be empty (no
-- @schema_version@ table). Callers that want to re-run on a populated DB
-- must call 'dropSchema' first — the boot flow only does so when the
-- operator explicitly passes @--resync-from-genesis@.
--
-- 'DbSync.Checkpoint.SyncState.seedSyncState' is __not__ called here;
-- seeding is the caller's responsibility so that the @ledger_enabled@ flag
-- comes from runtime configuration.
initSchema :: [TableDef] -> [(Text, Int)] -> Text -> IO ()
initSchema tableDefs extractorVersions connStr = do
  -- Create the version tracking table
  execPsql connStr createVersionTableSQL

  -- Create the singleton sync-state table (LOGGED, constrained, with
  -- defaults). Its DDL is generated from its 'TableDef' exactly like
  -- the extractor tables, so it picks up the same column-ordering
  -- golden as the rest of the schema.
  execPsql connStr (generateCreateTable syncStateTableDef)

  -- System table for the ledger worker's per-epoch deposit-param
  -- snapshot. Always created; stays empty when the ledger feature
  -- is disabled.
  execPsql connStr (generateCreateTable epochParamPendingTableDef)

  -- Create all data tables
  let ddlStatements = map generateCreateTable tableDefs
      allDDL = T.unlines ddlStatements
  execPsql connStr allDDL

  -- Record extractor versions
  forM_ extractorVersions $ \(name, ver) ->
    execPsql connStr $ insertVersionSQL name ver

-- | Drop everything owned by this dbsync schema: the data tables, the
-- @dbsync_sync_state@ singleton, and the @schema_version@ table itself.
--
-- This is the \"force re-sync\" / test-hygiene drop. The boot flow only
-- calls it when the operator opts in via @--resync-from-genesis@; matched-version
-- restarts must not invoke it (that would defeat the resume logic).
--
-- The @extractorVersions@ argument is currently unused but kept for symmetry
-- with 'initSchema' and to make future per-extractor cleanup additive.
--
-- Safe to call on an empty database (every statement uses @IF EXISTS@).
dropSchema :: [TableDef] -> [(Text, Int)] -> Text -> IO ()
dropSchema tableDefs _extractorVersions connStr = do
  -- Drop data tables
  forM_ tableDefs $ \td ->
    execPsql connStr $ "DROP TABLE IF EXISTS " <> quoteIdent (tdName td) <> " CASCADE;"

  -- Drop the sync-state table too so tests / resync-from-genesis start fresh
  execPsql connStr $
    "DROP TABLE IF EXISTS " <> quoteIdent syncStateTableName <> " CASCADE;"

  -- Drop the system temp table used by the ledger-worker deposit flush.
  execPsql connStr $
    "DROP TABLE IF EXISTS " <> quoteIdent epochParamPendingTableName <> " CASCADE;"

  -- Drop the schema_version table itself (not just its rows). Dropping the
  -- table is the only way to recover from a stale shape (e.g. left over from
  -- an upstream cardano-db-sync install with different columns); any caller
  -- that wants to preserve schema_version must not call dropSchema.
  execPsql connStr "DROP TABLE IF EXISTS \"schema_version\" CASCADE;"

-- | Flip UNLOGGED extractor tables to LOGGED and attach an
-- @<table>_id_seq@. Idempotent. Precondition for hasql INSERTs.
prepareSchemaForFollowTip :: [TableDef] -> Text -> IO ()
prepareSchemaForFollowTip tables connStr =
  for_ (prepareSchemaForFollowTipSql tables) (execPsql connStr)

-- | The DDL statements that 'prepareSchemaForFollowTip' would run,
-- as a flat list — for callers that want to send them via hasql
-- rather than @psql@. Includes only the UNLOGGED tables; tables
-- already LOGGED contribute nothing.
prepareSchemaForFollowTipSql :: [TableDef] -> [Text]
prepareSchemaForFollowTipSql tables =
  concatMap perTableSchemaForFollowTipSql
    (filter ((== TableUnlogged) . tdMode) tables)

-- | The three flip statements (@SET LOGGED@, @CREATE SEQUENCE@,
-- @ALTER … SET DEFAULT@) for a single table, ready to be run as a
-- per-table unit by a parallel worker. The caller is responsible
-- for filtering on 'tdMode'; this function does no filtering.
perTableSchemaForFollowTipSql :: TableDef -> [Text]
perTableSchemaForFollowTipSql td =
  let name = tdName td
  in [ setLoggedSql name
     , createIdSequenceSql name
     , attachIdDefaultSql name
     ]

-- | @ALTER TABLE … SET LOGGED@ DDL for a single table.
setLoggedSql :: Text -> Text
setLoggedSql tableName =
  "ALTER TABLE " <> quoteIdent tableName <> " SET LOGGED"

-- | Create the @<table>_id_seq@ sequence and attach ownership to
-- the @id@ column. Idempotent (@IF NOT EXISTS@).
createIdSequenceSql :: Text -> Text
createIdSequenceSql tableName =
  T.concat
    [ "CREATE SEQUENCE IF NOT EXISTS "
    , quoteIdent (tableName <> "_id_seq")
    , " OWNED BY "
    , quoteIdent tableName, ".\"id\""
    ]

-- | Wire the @id@ column's @DEFAULT@ to @nextval(<table>_id_seq)@.
attachIdDefaultSql :: Text -> Text
attachIdDefaultSql tableName =
  T.concat
    [ "ALTER TABLE ", quoteIdent tableName
    , " ALTER COLUMN \"id\" SET DEFAULT nextval('"
    , tableName <> "_id_seq"
    , "'::regclass)"
    ]

-- | @ANALYZE@ on a single table. Used after the bulk pass to refresh
-- planner statistics that the new indexes and updated columns
-- invalidated.
analyzeSql :: Text -> Text
analyzeSql tableName =
  "ANALYZE " <> quoteIdent tableName

-- | @VACUUM@ on a single table. Used between resolve and the LOGGED
-- flip to reclaim dead tuples left by the resolve UPDATEs, so the
-- subsequent heap rewrite doesn't drag them along.
vacuumSql :: Text -> Text
vacuumSql tableName =
  "VACUUM " <> quoteIdent tableName

-- ---------------------------------------------------------------------------
-- * Version checking
-- ---------------------------------------------------------------------------

-- | Inspect the database and classify the schema state against the
-- versions expected by the running code.
--
-- Thin IO wrapper over 'analyzeSchemaState': queries @pg_tables@ to detect
-- whether the @schema_version@ table exists, reads its rows if so, and
-- delegates the comparison to the pure analyser.
checkSchemaVersions :: [(Text, Int)] -> Text -> IO SchemaState
checkSchemaVersions expectedVersions connStr = do
  tableExists <- queryPsql connStr
    "SELECT count(*) FROM pg_tables WHERE schemaname = 'public' AND tablename = 'schema_version';"
  if T.strip tableExists /= "1"
    then pure (analyzeSchemaState expectedVersions Nothing)
    else do
      dbVersionsRaw <- queryPsql connStr
        "SELECT extractor_name, version FROM schema_version ORDER BY extractor_name;"
      pure (analyzeSchemaState expectedVersions (Just (parseVersionRows dbVersionsRaw)))

  where
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
-- the operator-supplied @--resync-from-genesis@ flag.
--
-- 'True' for @--resync-from-genesis@ short-circuits everything: the operator has
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

-- | Probe @wal_level@ via @SHOW@. Returns the value as 'Text', e.g.
-- @"minimal"@, @"replica"@, or @"logical"@. Used at boot to warn
-- when the server isn't on @wal_level = minimal@ — at minimal,
-- @ALTER TABLE … SET LOGGED@ skips WAL for tables larger than
-- @wal_skip_threshold@.
showWalLevel :: Text -> IO Text
showWalLevel connStr =
  T.strip <$> queryPsql connStr "SHOW wal_level;"

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
