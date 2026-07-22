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
  , readInt
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

-- | @SELECT count(*) FROM table@ as an 'Int'. Panics on unparseable
-- output so a zero-expectation assertion can never pass vacuously.
countRows :: HasCallStack => Text -> IO Int
countRows table = do
  t <- T.strip <$> queryTestDb ("SELECT count(*) FROM " <> table <> ";")
  pure $ readIntCell ("countRows " <> table) t

-- | NULL count for a single column. Used by FK-resolution
-- assertions where Prep's backfill UPDATEs are expected to leave
-- zero NULLs on the affected columns. Panics on unparseable output.
countNulls :: HasCallStack => Text -> Text -> IO Int
countNulls table col = do
  t <- T.strip <$> queryTestDb
    ("SELECT count(*) FROM " <> table <> " WHERE " <> col <> " IS NULL;")
  pure $ readIntCell ("countNulls " <> table <> "." <> col) t

-- ---------------------------------------------------------------------------
-- * Schema-flip introspection
-- ---------------------------------------------------------------------------

-- | How many tables from the supplied list are still UNLOGGED
-- (@pg_class.relpersistence <> \'p\'@). After 'PreparingForVolatileTail'
-- this should be zero. Table names come from compile-time
-- 'TableDef' values and so are SQL-safe by construction, but they
-- are still single-quoted in the @IN (...)@ list as a defence in
-- depth.
countNonLoggedTables :: HasCallStack => [Text] -> IO Int
countNonLoggedTables names = do
  let inList = T.intercalate "," (map quoteLit names)
  t <- T.strip <$> queryTestDb
    ( "SELECT count(*) FROM pg_class WHERE relkind = 'r' AND relname IN ("
        <> inList <> ") AND relpersistence <> 'p';"
    )
  pure $ readIntCell "countNonLoggedTables" t

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

-- | 'True' when the table's @id@ sequence matches Prep's contract:
-- the next allocation equals @MAX(id) + 1@ on a populated table, or
-- @1@ on an empty one. The sequence name is resolved via
-- @pg_get_serial_sequence@ so explicit and @IDENTITY@-backed
-- sequences are handled uniformly. The probing @nextval@ is reverted
-- via @setval(..., is_called=false)@, so repeated calls observe the
-- same sequence state. Panics on unparseable output.
sequenceAdvanced :: HasCallStack => Text -> IO Bool
sequenceAdvanced table = do
  nextRaw <- T.strip <$> queryTestDb
    ("SELECT nextval(pg_get_serial_sequence('" <> table <> "', 'id'));")
  let n = readIntCell ("sequenceAdvanced " <> table <> " nextval") nextRaw
  -- undo the advance: the next consumer sees n again
  _ <- queryTestDb
    ( "SELECT setval(pg_get_serial_sequence('" <> table <> "', 'id'), "
        <> show n <> ", false);"
    )
  maxRaw <- T.strip <$> queryTestDb
    ("SELECT COALESCE(MAX(id), 0) FROM " <> table <> ";")
  let m = readIntCell ("sequenceAdvanced " <> table <> " max id") maxRaw
  pure $ if m == 0 then n == 1 else n == m + 1

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

-- | Run @sql@ via 'queryTestDb' and parse the first cell as 'Int',
-- panicking on unparseable output.
readInt :: HasCallStack => Text -> IO Int
readInt sql = do
  t <- T.strip <$> queryTestDb sql
  pure $ readIntCell ("readInt " <> sql) t

-- | Run @sql@ via 'queryTestDb' and parse the first cell as a
-- nullable 'Int'. An empty cell is NULL ('Nothing'); non-empty
-- unparseable output panics rather than masquerading as NULL.
readNullableInt :: HasCallStack => Text -> IO (Maybe Int)
readNullableInt sql = do
  t <- T.strip <$> queryTestDb sql
  if T.null t
    then pure Nothing
    else pure (Just (readIntCell "readNullableInt" t))

-- ---------------------------------------------------------------------------
-- * Internal
-- ---------------------------------------------------------------------------

-- | Single-quote a value for inclusion in a SQL literal, doubling
-- any embedded apostrophes per the SQL spec.
quoteLit :: Text -> Text
quoteLit t = "'" <> T.replace "'" "''" t <> "'"

-- | Parse a psql cell as 'Int', panicking on failure so no
-- assertion can pass against garbage output.
readIntCell :: HasCallStack => Text -> Text -> Int
readIntCell what t =
  case readMaybe (T.unpack t) of
    Just n  -> n
    Nothing -> panic $ what <> ": unparseable psql output: " <> show t
