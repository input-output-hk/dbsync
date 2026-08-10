{-# LANGUAGE OverloadedStrings #-}

-- | Schema initialisation and extractor presence checks.
--
-- Boot calls this module once to create the UNLOGGED tables from their
-- 'TableDef's through @psql@, and to compare the enabled extractors with
-- the set recorded on the @dbsync_sync_state@ row.
module DbSync.Db.Schema.Init
  (     -- * Schema lifecycle
    initSchema
  , initSchemaStatements
  , dropSchema
  , truncateDataTables
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
-- config enables.
data SchemaState
  = SchemaFresh
    -- ^ No @dbsync_sync_state@ table: a brand-new database.
  | SchemaUnseeded
    -- ^ The table exists but carries no @id = 1@ row. A crash landed
    -- between schema creation and the seed write.
  | SchemaMatches
  | SchemaMismatched !(NonEmpty SchemaMismatch)
  deriving stock (Eq, Show)

data SchemaMismatch
  = MissingExtractor !Text
    -- ^ Enabled in the config, absent from the database.
  | UnexpectedExtractor !Text
    -- ^ Recorded in the database, absent from the config.
  deriving stock (Eq, Show)

-- | The action the boot flow should take, given the observed schema state and
-- whether the operator passed @--resync-from-genesis@.
data SchemaAction
  = -- | Schema already matches; do not touch DDL.
    ActionSkipInit
  | ActionRunInit
  | -- | Drop everything, @dbsync_sync_state@ included, then re-run
    -- 'initSchema'.
    ActionForceReinit
  | -- | Schema mismatch and no force flag; the operator must intervene.
    ActionAbort !(NonEmpty SchemaMismatch)
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Schema lifecycle
-- ---------------------------------------------------------------------------

-- | Run 'initSchemaStatements' in order.
--
-- __Not idempotent__: the database must be empty. To re-run on a populated
-- database, call 'dropSchema' first.
--
-- The caller must also call 'DbSync.SyncState.Row.seedSyncState', because
-- the @ledger_enabled@ flag and the extractor set come from runtime config.
initSchema
  :: [TableDef]
  -> Text     -- ^ Connection string.
  -> IO ()
initSchema tableDefs connStr =
  for_ (initSchemaStatements tableDefs) (execPsql connStr)

-- | The ordered DDL 'initSchema' runs on a fresh database:
--
-- 1. the @dbsync_sync_state@ singleton metadata table;
-- 2. the @epoch_param_pending@ system table, which stays empty when the
--    ledger feature is off;
-- 3. all data tables from the 'TableDef's, as a single batch;
-- 4. the @epoch_current@ and @epoch@ views, when @epoch_finalized@ is
--    among the data tables.
--
-- The migration baseline file comes from this same list, so it cannot
-- drift from what 'initSchema' creates.
initSchemaStatements :: [TableDef] -> [Text]
initSchemaStatements tableDefs =
  [ generateCreateTable syncStateTableDef
  , generateCreateTable epochParamPendingTableDef
  , T.unlines (map generateCreateTable tableDefs)
  ]
    <> [createEpochViewsSql | any ((== epochFinalizedTableName) . tdName) tableDefs]

-- | Drop the data tables, the @dbsync_sync_state@ singleton, and the
-- @epoch_param_pending@ system table.
--
-- Only @--resync-from-genesis@ and test hygiene call this. A matched
-- restart must not, because the drop defeats the resume logic. Every
-- statement uses @IF EXISTS@, so an empty database is safe.
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

-- | Empty every data table and reset its identity sequences, but keep the
-- schema and the @dbsync_sync_state@ singleton.
--
-- This is the fresh-boot purge for a database that crashed mid-Ingest
-- before the first epoch-boundary commit. @last_committed_slot@ is still
-- NULL, so boot classifies the database as fresh, but orphan rows from
-- the aborted leg survive and collide with the genesis re-COPY.
-- @RESTART IDENTITY@ rewinds the sequences, so the re-COPY starts at 1.
truncateDataTables :: [TableDef] -> Text -> IO ()
truncateDataTables tableDefs connStr =
  unless (null names) $
    execPsql connStr $
      "TRUNCATE TABLE " <> T.intercalate ", " (map quoteIdent names)
        <> " RESTART IDENTITY CASCADE;"
  where
    names = map tdName tableDefs <> [epochParamPendingTableName]

-- | Flip UNLOGGED extractor tables to LOGGED and attach an
-- @<table>_id_seq@. Idempotent. Precondition for hasql INSERTs.
prepareSchemaForFollowTip :: [TableDef] -> Text -> IO ()
prepareSchemaForFollowTip tables connStr =
  for_ (prepareSchemaForFollowTipSql tables) (execPsql connStr)

-- | The DDL that 'prepareSchemaForFollowTip' runs, as a flat list, for
-- callers that send it through hasql instead of @psql@. A LOGGED table
-- contributes nothing.
prepareSchemaForFollowTipSql :: [TableDef] -> [Text]
prepareSchemaForFollowTipSql tables =
  concatMap perTableSchemaForFollowTipSql
    (filter ((== TableUnlogged) . tdMode) tables)

-- | Per-table flip statements: @SET LOGGED@, plus @CREATE SEQUENCE@ and
-- @ALTER … SET DEFAULT@ for a non-identity @id@ column. An identity @id@
-- already owns a PG-managed sequence, and the attach DDL fails on it with
-- "column id is an identity column". The caller filters on 'tdMode'.
perTableSchemaForFollowTipSql :: TableDef -> [Text]
perTableSchemaForFollowTipSql td =
  let name = tdName td
  in setLoggedSql name
       : if "id" `elem` tdIdentityColumns td
           then []
           else [createIdSequenceSql name, attachIdDefaultSql name]

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

-- | Runs after the bulk pass, to refresh the planner statistics that the
-- new indexes and the updated columns invalidated.
analyzeSql :: Text -> Text
analyzeSql tableName =
  "ANALYZE " <> quoteIdent tableName

-- ---------------------------------------------------------------------------
-- * Extractor presence
-- ---------------------------------------------------------------------------

-- | Probe the @dbsync_sync_state@ singleton and classify the schema state
-- against the extractors the config enables.
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

-- | Compare the extractors the config enables with the names the database
-- records.
--
-- The comparison runs in both directions, missing names first. Only an
-- enabled extractor gets tables, so a recorded name the config dropped
-- means the cleanup and rollback passes skip tables that do exist and
-- leave rows behind.
analyzeExtractorState
  :: [Text]         -- ^ Extractor names the config enables
  -> Maybe [Text]   -- ^ Recorded extractor set; 'Nothing' = fresh database
  -> SchemaState
analyzeExtractorState _ Nothing = SchemaFresh
analyzeExtractorState expected (Just present) =
  case missing <> unexpected of
    []       -> SchemaMatches
    (m : ms) -> SchemaMismatched (m NE.:| ms)
  where
    missing    = map MissingExtractor    (filter (`notElem` present) expected)
    unexpected = map UnexpectedExtractor (filter (`notElem` expected) present)

-- | Decide what the boot flow does, given the schema state and the
-- @--resync-from-genesis@ flag. 'True' short-circuits every other case.
decideSchemaAction :: Bool -> SchemaState -> SchemaAction
decideSchemaAction True  _                       = ActionForceReinit
decideSchemaAction False SchemaMatches           = ActionSkipInit
decideSchemaAction False SchemaUnseeded          = ActionSkipInit
decideSchemaAction False SchemaFresh             = ActionRunInit
decideSchemaAction False (SchemaMismatched errs) = ActionAbort errs

-- | Renders one log line. The wording stays stable, so operators can grep
-- for it.
renderSchemaMismatch :: SchemaMismatch -> Text
renderSchemaMismatch = \case
  MissingExtractor name ->
    "Extractor '" <> name
      <> "' is enabled in the config but missing from the database."
  UnexpectedExtractor name ->
    "Extractor '" <> name
      <> "' is recorded in the database but not enabled in the config."

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

-- | Returns @"minimal"@, @"replica"@, or @"logical"@. Boot warns when the
-- server is not on @wal_level = minimal@: at minimal,
-- @ALTER TABLE … SET LOGGED@ skips the WAL for tables larger than
-- @wal_skip_threshold@.
showWalLevel :: Text -> IO Text
showWalLevel connStr =
  T.strip <$> queryPsql connStr "SHOW wal_level;"

-- | Run a query through @psql@. The @-t@, @-A@ and @-F \"|\"@ flags give
-- unaligned, pipe-separated output with no header or footer.
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
