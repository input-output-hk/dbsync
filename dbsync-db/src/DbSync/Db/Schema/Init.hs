{-# LANGUAGE OverloadedStrings #-}

-- | Schema initialisation and extractor presence checks.
--
-- Creates database tables from 'TableDef' definitions using @psql@ and
-- provides the boot-time presence check that compares the enabled
-- extractors against the set recorded on the @dbsync_sync_state@ row.
--
-- During 'IngestChainHistory', this module is called once at startup
-- to create the UNLOGGED tables that COPY streams will write into.
module DbSync.Db.Schema.Init
  ( -- * Schema lifecycle
    initSchema
  , initSchemaStatements
  , dropSchema
  , prepareSchemaForFollowTip

    -- * Extractor presence
  , checkExtractorPresence

    -- * Schema-state analysis (pure)
  , SchemaState (..)
  , SchemaMismatch (..)
  , SchemaAction (..)
  , analyzeExtractorState
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

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import System.IO.Error (userError)
import System.Process (readProcessWithExitCode)

import DbSync.Db.Schema.EpochParamPending
  ( epochParamPendingTableDef
  , epochParamPendingTableName
  )
import DbSync.Db.Schema.EpochView
  ( createEpochViewsSql
  , dropEpochViewsSql
  , epochFinalizedTableName
  )
import DbSync.Db.Schema.Generate (generateCreateTable)
import DbSync.Db.Schema.SyncState (syncStateTableDef, syncStateTableName)
import DbSync.Db.Schema.Types (TableDef (..), TableMode (..))
import DbSync.Db.Sql (quoteIdent, quoteLiteral)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Observed state of the database schema, relative to the extractors the
-- running profile enables.
--
-- Distinguishes the boot-time scenarios:
--
--   * 'SchemaFresh' — no @dbsync_sync_state@ table; this is a brand-new
--     database and the boot flow should run 'initSchema'.
--   * 'SchemaUnseeded' — the table exists but carries no @id = 1@ row; a
--     crash landed between schema creation and the seed write. The boot
--     flow skips 'initSchema' and 'decideBoot' aborts with a resync hint.
--   * 'SchemaMatches' — every enabled extractor is recorded; the boot flow
--     should skip 'initSchema' and resume.
--   * 'SchemaMismatched' — at least one enabled extractor is missing from
--     the database; the boot flow should abort and surface the
--     discrepancies to the operator (unless @--resync-from-genesis@
--     overrides).
data SchemaState
  = SchemaFresh
  | SchemaUnseeded
  | SchemaMatches
  | SchemaMismatched !(NonEmpty SchemaMismatch)
  deriving stock (Eq, Show)

-- | An extractor the profile enables but the database was not built with.
data SchemaMismatch
  = MissingExtractor !Text
  deriving stock (Eq, Show)

-- | The action the boot flow should take, given the observed schema state and
-- whether the operator passed @--resync-from-genesis@.
data SchemaAction
  = -- | Schema already matches; do not touch DDL.
    ActionSkipInit
  | -- | DB is empty; run 'initSchema' to create everything.
    ActionRunInit
  | -- | Operator forced a clean slate; drop everything (including
    -- @dbsync_sync_state@) and re-run 'initSchema'.
    ActionForceReinit
  | -- | Schema mismatch and no force flag; the operator must intervene.
    ActionAbort !(NonEmpty SchemaMismatch)
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Schema lifecycle
-- ---------------------------------------------------------------------------

-- | Initialise the database schema on a __fresh__ database by executing
-- 'initSchemaStatements' in order.
--
-- __Not idempotent__: this function expects the database to be empty.
-- Callers that want to re-run on a populated DB must call 'dropSchema'
-- first — the boot flow only does so when the operator explicitly passes
-- @--resync-from-genesis@.
--
-- 'DbSync.SyncState.Row.seedSyncState' is __not__ called here; seeding is
-- the caller's responsibility so that the @ledger_enabled@ flag and the
-- enabled-extractor set come from runtime configuration.
initSchema
  :: [TableDef]
  -> Text     -- ^ Connection string.
  -> IO ()
initSchema tableDefs connStr =
  for_ (initSchemaStatements tableDefs) (execPsql connStr)

-- | The ordered DDL 'initSchema' runs on a fresh database:
--
-- 1. the @dbsync_sync_state@ singleton metadata table;
-- 2. the @epoch_param_pending@ system table (always created; stays empty
--    when the ledger feature is disabled);
-- 3. all data tables from the 'TableDef's, as a single batch;
-- 4. the @epoch_current@ and @epoch@ views, when the @epoch@ extractor is
--    enabled (i.e. @epoch_finalized@ is among the data tables).
--
-- The migration baseline file is generated from this same list, so it
-- cannot drift from what 'initSchema' creates.
initSchemaStatements :: [TableDef] -> [Text]
initSchemaStatements tableDefs =
  [ generateCreateTable syncStateTableDef
  , generateCreateTable epochParamPendingTableDef
  , T.unlines (map generateCreateTable tableDefs)
  ]
    <> [createEpochViewsSql | any ((== epochFinalizedTableName) . tdName) tableDefs]

-- | Drop everything owned by this dbsync schema: the data tables, the
-- @dbsync_sync_state@ singleton, and the @epoch_param_pending@ system
-- table.
--
-- This is the \"force re-sync\" / test-hygiene drop. The boot flow only
-- calls it when the operator opts in via @--resync-from-genesis@; matched
-- restarts must not invoke it (that would defeat the resume logic).
--
-- Safe to call on an empty database (every statement uses @IF EXISTS@).
dropSchema :: [TableDef] -> Text -> IO ()
dropSchema tableDefs connStr = do
  -- Drop epoch views first; they reference @epoch_finalized@.
  when (any ((== epochFinalizedTableName) . tdName) tableDefs) $
    execPsql connStr dropEpochViewsSql

  -- Drop data tables
  forM_ tableDefs $ \td ->
    execPsql connStr $ "DROP TABLE IF EXISTS " <> quoteIdent (tdName td) <> " CASCADE;"

  -- Drop the sync-state table too so tests / resync-from-genesis start fresh
  execPsql connStr $
    "DROP TABLE IF EXISTS " <> quoteIdent syncStateTableName <> " CASCADE;"

  -- Drop the system temp table used by the ledger-worker deposit flush.
  execPsql connStr $
    "DROP TABLE IF EXISTS " <> quoteIdent epochParamPendingTableName <> " CASCADE;"

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

-- | Per-table flip statements: @SET LOGGED@, and (for non-identity
-- @id@ columns) @CREATE SEQUENCE@ + @ALTER … SET DEFAULT@. Tables
-- whose @id@ is declared @GENERATED BY DEFAULT AS IDENTITY@ already
-- have a PG-managed backing sequence; the sequence-attach DDL would
-- fail on them ("column id is an identity column"). The caller is
-- responsible for filtering on 'tdMode'.
perTableSchemaForFollowTipSql :: TableDef -> [Text]
perTableSchemaForFollowTipSql td =
  let name = tdName td
  in setLoggedSql name
       : if "id" `elem` tdIdentityColumns td
           then []
           else [createIdSequenceSql name, attachIdDefaultSql name]

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
-- * Extractor presence
-- ---------------------------------------------------------------------------

-- | Inspect the database and classify the schema state against the
-- extractors the running profile enables.
--
-- Three-way probe over the @dbsync_sync_state@ singleton:
--
--   * table absent → 'SchemaFresh' (brand-new DB; run 'initSchema').
--   * table present but unseeded (no @id = 1@ row) → 'SchemaUnseeded':
--     a crash landed between schema creation and the seed write.
--   * table present and seeded → compare the recorded @extractors@
--     against @expectedNames@ via 'analyzeExtractorState'.
checkExtractorPresence :: [Text] -> Text -> IO SchemaState
checkExtractorPresence expectedNames connStr = do
  tableExists <- queryPsql connStr $
    "SELECT count(*) FROM pg_tables WHERE schemaname = 'public' AND tablename = "
      <> quoteLiteral syncStateTableName <> ";"
  if T.strip tableExists /= "1"
    then pure SchemaFresh
    else do
      rowExists <- queryPsql connStr $
        "SELECT count(*) FROM " <> quoteIdent syncStateTableName <> " WHERE id = 1;"
      if T.strip rowExists /= "1"
        then pure SchemaUnseeded
        else do
          namesRaw <- queryPsql connStr $
            "SELECT unnest(extractors) FROM " <> quoteIdent syncStateTableName
              <> " WHERE id = 1;"
          let names = filter (not . T.null) (map T.strip (T.lines (T.strip namesRaw)))
          pure (analyzeExtractorState expectedNames (Just names))

-- ---------------------------------------------------------------------------
-- * Schema-state analysis (pure)
-- ---------------------------------------------------------------------------

-- | Pure analysis of schema state given the extractors the code expects and
-- the names observed in the database.
--
-- @Nothing@ for the second argument means no enabled-extractor set was
-- recorded (a fresh DB). @Just names@ means a seeded @dbsync_sync_state@
-- row was found and @names@ are its recorded extractors.
--
-- Extra extractors in @names@ that are not in the expected list are
-- silently ignored — operators are allowed to remove an extractor from
-- their profile without re-syncing the rest.
analyzeExtractorState
  :: [Text]         -- ^ Extractor names the profile enables
  -> Maybe [Text]   -- ^ Recorded extractor set; 'Nothing' = none recorded
  -> SchemaState
analyzeExtractorState _ Nothing = SchemaFresh
analyzeExtractorState expected (Just present) =
  case filter (`notElem` present) expected of
    []       -> SchemaMatches
    (m : ms) -> SchemaMismatched (MissingExtractor m NE.:| map MissingExtractor ms)

-- | Decide what the boot flow should do, given the observed schema state and
-- the operator-supplied @--resync-from-genesis@ flag.
--
-- 'True' for @--resync-from-genesis@ short-circuits everything: the operator has
-- explicitly asked for a clean slate.
decideSchemaAction :: Bool -> SchemaState -> SchemaAction
decideSchemaAction True  _                       = ActionForceReinit
decideSchemaAction False SchemaMatches           = ActionSkipInit
decideSchemaAction False SchemaUnseeded          = ActionSkipInit
decideSchemaAction False SchemaFresh             = ActionRunInit
decideSchemaAction False (SchemaMismatched errs) = ActionAbort errs

-- | Render a single 'SchemaMismatch' as a human-readable line suitable for
-- logging. Stable wording so operators can grep for it.
renderSchemaMismatch :: SchemaMismatch -> Text
renderSchemaMismatch = \case
  MissingExtractor name ->
    "Extractor '" <> name
      <> "' is enabled in the profile but missing from the database."

-- ---------------------------------------------------------------------------
-- * psql helpers
-- ---------------------------------------------------------------------------

-- | Execute a SQL statement via @psql@ with @ON_ERROR_STOP=1@.
-- Throws on any non-zero exit.
execPsql :: Text -> Text -> IO ()
execPsql connStr sql = do
  (exitCode, _out, err) <- readProcessWithExitCode
    "psql"
    [T.unpack connStr, "-q", "-v", "ON_ERROR_STOP=1", "-c", T.unpack sql]
    ""
  case exitCode of
    ExitSuccess     -> pure ()
    ExitFailure _ ->
      throwIO $ userError $
        "psql failed: " <> err <> "\nSQL: " <> T.unpack sql

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
