{-# LANGUAGE OverloadedStrings #-}

-- | Test-side query helpers for post-Prep / post-Follow PG assertions.
--
-- Built on top of 'DbSync.Test.Database.queryTestDb' (psql shell-out)
-- rather than hasql so the helpers compose with any test that has
-- already booted the dbsync schema via 'runApp', without needing to
-- thread a separate hasql 'Conn.Connection' around.
--
-- All helpers are stateless wrappers — they open the libpq
-- connection per call. Fine for the once-per-test usage pattern;
-- not appropriate for tight loops. Use 'DbSync.Test.Hasql' there
-- instead.
module DbSync.Test.PgAssertions
  ( -- * Row counts
    countRows
  , countNulls

    -- * Schema-flip introspection
  , countNonLoggedTables
  , listMissingIndexes

    -- * Sequence introspection
  , sequenceAdvanced

    -- * Sync-state ↔ block consistency
  , readSyncStateLast
  , readBlockMax

    -- * Settle-state polling
  , waitForSchemaSettled
  , waitForTableQueryable

    -- * Schema-driven SQL fragments
  , tableColumn

    -- * Generic decoders
  , readNullableInt
  ) where

import Cardano.Prelude

import qualified Data.Text as T

import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (ColumnDef (..), TableDef (..))
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.Helpers (waitFor)

-- ---------------------------------------------------------------------------
-- * Row counts
-- ---------------------------------------------------------------------------

-- | @SELECT count(*) FROM table@ as an 'Int'. Returns @0@ if the
-- value comes back unparseable (e.g. the table doesn't exist); the
-- caller's downstream assertion then fails loudly on the wrong-value
-- comparison rather than swallowing a parse error here.
countRows :: Text -> IO Int
countRows table = do
  t <- T.strip <$> queryTestDb ("SELECT count(*) FROM " <> table <> ";")
  pure $ fromMaybe 0 (readMaybe (T.unpack t))

-- | NULL count for a single column. Used by FK-resolution
-- assertions where Prep's backfill UPDATEs are expected to leave
-- zero NULLs on the affected columns.
countNulls :: Text -> Text -> IO Int
countNulls table col = do
  t <- T.strip <$> queryTestDb
    ("SELECT count(*) FROM " <> table <> " WHERE " <> col <> " IS NULL;")
  pure $ fromMaybe 0 (readMaybe (T.unpack t))

-- ---------------------------------------------------------------------------
-- * Schema-flip introspection
-- ---------------------------------------------------------------------------

-- | How many tables from the supplied list are still UNLOGGED
-- (@pg_class.relpersistence <> \'p\'@). After 'PreparingForVolatileTail'
-- this should be zero. Table names come from compile-time
-- 'TableDef' values and so are SQL-safe by construction, but they
-- are still single-quoted in the @IN (...)@ list as a defence in
-- depth.
countNonLoggedTables :: [Text] -> IO Int
countNonLoggedTables names = do
  let inList = T.intercalate "," (map quoteLit names)
  t <- T.strip <$> queryTestDb
    ( "SELECT count(*) FROM pg_class WHERE relkind = 'r' AND relname IN ("
        <> inList <> ") AND relpersistence <> 'p';"
    )
  pure $ fromMaybe 0 (readMaybe (T.unpack t))

-- | Of the supplied index names, those missing from
-- @pg_indexes.public@. The empty list means every expected index
-- exists. Helps tests assert presence of a known-good set without
-- false negatives on optional indexes that might or might not exist.
listMissingIndexes :: [Text] -> IO [Text]
listMissingIndexes expected = do
  let inList = T.intercalate "," (map quoteLit expected)
  raw <- queryTestDb
    ( "SELECT indexname FROM pg_indexes WHERE schemaname = 'public' AND indexname IN ("
        <> inList <> ");"
    )
  let present = filter (not . T.null) (map T.strip (T.lines raw))
  pure $ filter (`notElem` present) expected

-- ---------------------------------------------------------------------------
-- * Sequence introspection
-- ---------------------------------------------------------------------------

-- | 'True' when the named sequence's @last_value@ matches Prep's
-- contract: @MAX(id) + 1@ on a populated table, @1@ on an empty one.
-- Returns 'False' on parse failure so the calling assertion surfaces
-- the discrepancy rather than the parser.
sequenceAdvanced :: Text -> Text -> IO Bool
sequenceAdvanced table seqName = do
  seqRaw <- T.strip <$> queryTestDb
    ("SELECT last_value FROM " <> seqName <> ";")
  maxRaw <- T.strip <$> queryTestDb
    ("SELECT COALESCE(MAX(id), 0) FROM " <> table <> ";")
  let seqVal = readMaybe (T.unpack seqRaw) :: Maybe Int
      maxVal = readMaybe (T.unpack maxRaw) :: Maybe Int
  case (seqVal, maxVal) of
    (Just s, Just 0) -> pure (s == 1)
    (Just s, Just m) -> pure (s == m + 1)
    _                -> pure False

-- ---------------------------------------------------------------------------
-- * Settle-state polling
-- ---------------------------------------------------------------------------

-- | Block until every name in @tables@ is LOGGED in 'pg_class' AND
-- every name in @indexes@ is present in 'pg_indexes'. Panics on
-- timeout.
--
-- Prep commits with @synchronous_commit = off@ and fans the LOGGED
-- flip + index build across a pool of backends. By the time
-- 'markSyncComplete' writes @sync_complete=true@ the parallel work
-- has finished, but the catalog updates may still be propagating to
-- fresh (psql-spawned) connections for a few hundred ms. Tests that
-- read @pg_class@ or @pg_indexes@ immediately after observing
-- @sync_complete=true@ should call this first so the subsequent
-- strict-equality asserts have a settled DB to read.
waitForSchemaSettled
  :: [Text]   -- ^ tables expected to be LOGGED
  -> [Text]   -- ^ index names expected to exist
  -> Int      -- ^ timeout in seconds
  -> IO ()
waitForSchemaSettled tables indexes =
  waitFor "post-Prep schema state to settle" settled
  where
    settled = do
      nonLogged <- countNonLoggedTables tables
      missing   <- listMissingIndexes indexes
      pure (nonLogged == 0 && null missing)

-- | Block until a @SELECT 1 FROM @\<table\>@ LIMIT 1@ succeeds on a
-- fresh psql connection. Guards strict-equality reads against the
-- @aaResyncFromGenesis=True@ boot's dropSchema → initSchema window
-- and against the post-Prep catalog-propagation lag.
waitForTableQueryable :: Text -> Int -> IO ()
waitForTableQueryable table =
  waitFor ("table " <> table <> " queryable") queryable
  where
    queryable = do
      result <- try $ queryTestDb ("SELECT 1 FROM " <> table <> " LIMIT 1;")
      pure $ case (result :: Either SomeException Text) of
        Right _ -> True
        Left  _ -> False

-- ---------------------------------------------------------------------------
-- * Sync-state ↔ block consistency
-- ---------------------------------------------------------------------------

-- | @(last_committed_slot, last_committed_block_no)@ from the
-- dbsync sync-state row, both 'Nothing' before any block has been
-- committed.
readSyncStateLast :: IO (Maybe Int, Maybe Int)
readSyncStateLast = do
  slot  <- readNullableInt $
    "SELECT COALESCE(" <> tableColumn syncStateTableDef "last_committed_slot"
      <> "::text, '') FROM " <> tdName syncStateTableDef <> " LIMIT 1;"
  block <- readNullableInt $
    "SELECT COALESCE(" <> tableColumn syncStateTableDef "last_committed_block_no"
      <> "::text, '') FROM " <> tdName syncStateTableDef <> " LIMIT 1;"
  pure (slot, block)

-- | @(MAX(slot_no), MAX(block_no))@ from the @block@ table. 'Nothing'
-- on each component when the table is empty.
readBlockMax :: IO (Maybe Int, Maybe Int)
readBlockMax = do
  slot  <- readNullableInt $
    "SELECT COALESCE(MAX(" <> tableColumn blockTableDef "slot_no"
      <> ")::text, '') FROM " <> tdName blockTableDef <> ";"
  block <- readNullableInt $
    "SELECT COALESCE(MAX(" <> tableColumn blockTableDef "block_no"
      <> ")::text, '') FROM " <> tdName blockTableDef <> ";"
  pure (slot, block)

-- ---------------------------------------------------------------------------
-- * Schema-driven SQL fragments
-- ---------------------------------------------------------------------------

-- | Look up a column name on a 'TableDef'. Returns the @cdName@
-- value (which equals @name@ on success) so it composes directly
-- into SQL fragments via @\<>@. Panics with the list of declared
-- columns when @name@ isn't on the table — surfaces schema drift
-- the moment a test runs, without waiting for the PG round-trip
-- that would otherwise report @column \"foo\" does not exist@.
tableColumn :: HasCallStack => TableDef -> Text -> Text
tableColumn td name =
  case find ((== name) . cdName) (tdColumns td) of
    Just c  -> cdName c
    Nothing -> panic $
      "tableColumn: \"" <> name <> "\" not in " <> tdName td
        <> "; have: " <> T.intercalate ", " (map cdName (tdColumns td))

-- ---------------------------------------------------------------------------
-- * Generic decoders
-- ---------------------------------------------------------------------------

-- | Run @sql@ via 'queryTestDb' and parse the first cell as a
-- nullable 'Int'. Empty / unparseable cells return 'Nothing'.
readNullableInt :: Text -> IO (Maybe Int)
readNullableInt sql = do
  t <- T.strip <$> queryTestDb sql
  if T.null t then pure Nothing else pure (readMaybe (T.unpack t))

-- ---------------------------------------------------------------------------
-- * Internal
-- ---------------------------------------------------------------------------

-- | Single-quote a value for inclusion in a SQL literal, doubling
-- any embedded apostrophes per the SQL spec.
quoteLit :: Text -> Text
quoteLit t = "'" <> T.replace "'" "''" t <> "'"
