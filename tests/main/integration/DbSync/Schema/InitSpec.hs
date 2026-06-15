{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for schema initialisation.
--
-- Tests that 'initSchema' creates tables from 'TableDef's via @psql@,
-- and that 'checkExtractorPresence' compares the enabled extractors
-- against the set recorded in the @dbsync_sync_state.extractors@ column.
--
-- Requires a running PostgreSQL instance with a @dbsync_test@ database.
module DbSync.Schema.InitSpec (spec) where

import Cardano.Prelude

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Test.Hspec
  ( Spec
  , afterAll_
  , anyIOException
  , beforeAll_
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  , shouldThrow
  )

import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init
  ( SchemaAction (..)
  , SchemaMismatch (..)
  , SchemaState (..)
  , analyzeExtractorState
  , checkExtractorPresence
  , decideSchemaAction
  , dropSchema
  , initSchema
  , queryPsql
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Schema.Version (Fingerprint (..))
import DbSync.SyncState.Row
  ( closeControlConnection
  , openControlConnection
  , seedSyncState
  )
import DbSync.AppM (runAppM)
import DbSync.Test.Database (testHasqlSettings)

-- | Connection string for the test database.
testConnStr :: Text
testConnStr = "dbname=dbsync_test"

-- | The three core TableDefs.
coreTables :: [TableDef]
coreTables = [blockTableDef, txTableDef, slotLeaderTableDef]

-- | Comma-separated single-quoted SQL list of 'coreTables' names,
-- suitable for embedding in @WHERE tablename IN (...)@ /
-- @WHERE relname IN (...)@ clauses.
coreTablesInList :: Text
coreTablesInList =
  T.intercalate ", " (map (\td -> "'" <> tdName td <> "'") coreTables)

-- | The extractor names the core tables belong to.
coreNames :: [Text]
coreNames = ["core"]

-- | Seed the @dbsync_sync_state@ singleton row with the given enabled
-- extractor set, so 'checkExtractorPresence' has a row to read back.
seedExtractors :: [Text] -> IO ()
seedExtractors names =
  bracket (openControlConnection testHasqlSettings) closeControlConnection $ \ctrl ->
    runAppM ctrl (seedSyncState 1 (Fingerprint "test-fp") False names)

spec :: Spec
spec = describe "DbSync.Db.Schema.Init" $ do

  -- ---------------------------------------------------------------------------
  -- Pure tests (no PostgreSQL required)
  -- ---------------------------------------------------------------------------

  describe "decideSchemaAction (pure)" $ do
    it "resync-from-genesis overrides everything: matches" $
      decideSchemaAction True SchemaMatches `shouldBe` ActionForceReinit

    it "resync-from-genesis overrides everything: fresh" $
      decideSchemaAction True SchemaFresh `shouldBe` ActionForceReinit

    it "resync-from-genesis overrides everything: mismatched" $
      let errs = MissingExtractor "core" NE.:| []
      in decideSchemaAction True (SchemaMismatched errs) `shouldBe` ActionForceReinit

    it "no force, schema matches → skip init" $
      decideSchemaAction False SchemaMatches `shouldBe` ActionSkipInit

    it "no force, fresh DB → run init" $
      decideSchemaAction False SchemaFresh `shouldBe` ActionRunInit

    it "no force, mismatched → abort with the same errors" $
      let errs = MissingExtractor "utxo" NE.:| [MissingExtractor "pool"]
      in decideSchemaAction False (SchemaMismatched errs) `shouldBe` ActionAbort errs

  describe "analyzeExtractorState (pure)" $ do
    it "no recorded extractors → SchemaFresh (no expected extractors)" $
      analyzeExtractorState [] Nothing `shouldBe` SchemaFresh

    it "no recorded extractors → SchemaFresh (with expected extractors)" $
      analyzeExtractorState ["core", "utxo"] Nothing `shouldBe` SchemaFresh

    it "all expected extractors present → SchemaMatches" $
      analyzeExtractorState
        ["core", "utxo"]
        (Just ["core", "utxo"])
        `shouldBe` SchemaMatches

    it "extra extractors in DB are silently ignored" $
      analyzeExtractorState
        ["core"]
        (Just ["core", "removed_feature"])
        `shouldBe` SchemaMatches

    it "expected extractor missing from DB → MissingExtractor" $
      analyzeExtractorState
        ["core", "utxo"]
        (Just ["core"])
        `shouldBe` SchemaMismatched (MissingExtractor "utxo" NE.:| [])

    it "multiple missing extractors reported in expected order" $
      analyzeExtractorState
        ["core", "utxo", "metadata"]
        (Just ["core"])
        `shouldBe` SchemaMismatched
          (MissingExtractor "utxo" NE.:| [MissingExtractor "metadata"])

    it "empty expected extractors with present table → SchemaMatches" $
      analyzeExtractorState [] (Just ["core"]) `shouldBe` SchemaMatches

  -- Each top-level group cleans up after itself
  describe "initSchema + dropSchema" $
    beforeAll_ (dropSchema coreTables testConnStr) $
    afterAll_  (dropSchema coreTables testConnStr) $ do

      it "creates tables that exist in pg_tables" $ do
        initSchema coreTables testConnStr
        result <- queryPsql testConnStr $
          "SELECT tablename FROM pg_tables"
            <> " WHERE schemaname = 'public' AND tablename IN ("
            <> coreTablesInList <> ") ORDER BY tablename;"
        let tables = T.lines (T.strip result)
        tables `shouldBe` List.sort (map tdName coreTables)

      it "creates tables as UNLOGGED" $ do
        -- pg_class.relpersistence: 'u' = UNLOGGED, 'p' = permanent (LOGGED)
        result <- queryPsql testConnStr $
          "SELECT relname, relpersistence FROM pg_class"
            <> " WHERE relname IN (" <> coreTablesInList
            <> ") ORDER BY relname;"
        let rows = T.lines (T.strip result)
        length rows `shouldBe` length coreTables
        -- All should be UNLOGGED
        rows `shouldSatisfy` all (T.isInfixOf "|u")

      it "creates block table with correct column count" $ do
        result <- queryPsql testConnStr $
          "SELECT count(*) FROM information_schema.columns"
            <> " WHERE table_name = '" <> tdName blockTableDef
            <> "' AND table_schema = 'public';"
        T.strip result `shouldBe` T.pack (show (length (tdColumns blockTableDef)))

      it "creates tx table with correct column count" $ do
        result <- queryPsql testConnStr $
          "SELECT count(*) FROM information_schema.columns"
            <> " WHERE table_name = '" <> tdName txTableDef
            <> "' AND table_schema = 'public';"
        T.strip result `shouldBe` T.pack (show (length (tdColumns txTableDef)))

      it "creates the id column as bigint NOT NULL" $ do
        result <- queryPsql testConnStr $
          "SELECT column_name, data_type, is_nullable FROM information_schema.columns "
          <> "WHERE table_name = '" <> tdName blockTableDef <> "' AND column_name = 'id';"
        T.strip result `shouldBe` "id|bigint|NO"

      it "creates nullable columns correctly" $ do
        result <- queryPsql testConnStr $
          "SELECT column_name, is_nullable FROM information_schema.columns "
          <> "WHERE table_name = '" <> tdName blockTableDef
          <> "' AND column_name = 'epoch_no';"
        T.strip result `shouldBe` "epoch_no|YES"

      it "dropSchema removes all tables" $ do
        dropSchema coreTables testConnStr
        result <- queryPsql testConnStr $
          "SELECT count(*) FROM pg_tables WHERE schemaname = 'public' AND tablename IN ("
            <> coreTablesInList <> ");"
        T.strip result `shouldBe` "0"
        -- Re-create for the afterAll_ cleanup to be idempotent
        initSchema coreTables testConnStr

  describe "extractors column" $
    beforeAll_ (dropSchema coreTables testConnStr >> initSchema coreTables testConnStr >> seedExtractors coreNames) $
    afterAll_  (dropSchema coreTables testConnStr) $ do

      it "records the enabled extractor set on the sync-state row" $ do
        result <- queryPsql testConnStr
          "SELECT unnest(extractors) FROM dbsync_sync_state ORDER BY 1;"
        let names = T.lines (T.strip result)
        names `shouldBe` List.sort coreNames

  describe "checkExtractorPresence" $
    beforeAll_ (dropSchema coreTables testConnStr >> initSchema coreTables testConnStr >> seedExtractors coreNames) $
    afterAll_  (dropSchema coreTables testConnStr) $ do

      it "returns SchemaMatches when the enabled extractors are present" $ do
        result <- checkExtractorPresence coreNames testConnStr
        result `shouldBe` SchemaMatches

      it "returns SchemaMismatched MissingExtractor when an extractor is absent" $ do
        result <- checkExtractorPresence ["core", "utxo"] testConnStr
        result `shouldBe` SchemaMismatched (MissingExtractor "utxo" NE.:| [])

      it "returns SchemaMatches when the DB has extra extractors not enabled" $ do
        -- DB recorded "core"; the running profile enables nothing — fine.
        result <- checkExtractorPresence [] testConnStr
        result `shouldBe` SchemaMatches

  describe "initSchema requires a fresh DB" $
    beforeAll_ (dropSchema coreTables testConnStr) $
    afterAll_  (dropSchema coreTables testConnStr) $ do

      it "creates the expected tables on a clean DB" $ do
        initSchema coreTables testConnStr
        result <- queryPsql testConnStr $
          "SELECT count(*) FROM pg_tables"
            <> " WHERE schemaname = 'public' AND tablename IN ("
            <> coreTablesInList <> ");"
        T.strip result `shouldBe` T.pack (show (length coreTables))

      it "fails if called on a populated DB (no longer drops + recreates)" $ do
        -- After the previous test the schema is in place; calling initSchema
        -- again must throw because CREATE TABLE on existing tables fails.
        initSchema coreTables testConnStr
          `shouldThrow` anyIOException
