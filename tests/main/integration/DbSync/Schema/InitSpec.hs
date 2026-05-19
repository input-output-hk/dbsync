{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for schema initialisation.
--
-- Tests that 'initSchema' creates tables from 'TableDef's via @psql@,
-- records extractor versions in @schema_version@, and that
-- 'checkSchemaVersions' detects mismatches.
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
  , analyzeSchemaState
  , checkSchemaVersions
  , decideSchemaAction
  , dropSchema
  , initSchema
  , queryPsql
  )
import DbSync.Db.Schema.Types (TableDef (..))

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

-- | Extractor version entries for the core tables.
coreVersions :: [(Text, Int)]
coreVersions = [("core", 1)]

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
      let errs = MissingExtractor "core" 1 NE.:| []
      in decideSchemaAction True (SchemaMismatched errs) `shouldBe` ActionForceReinit

    it "no force, schema matches → skip init" $
      decideSchemaAction False SchemaMatches `shouldBe` ActionSkipInit

    it "no force, fresh DB → run init" $
      decideSchemaAction False SchemaFresh `shouldBe` ActionRunInit

    it "no force, mismatched → abort with the same errors" $
      let errs = VersionAhead "core" 1 2 NE.:| [MissingExtractor "utxo" 1]
      in decideSchemaAction False (SchemaMismatched errs) `shouldBe` ActionAbort errs

  describe "analyzeSchemaState (pure)" $ do
    it "schema_version table missing → SchemaFresh (no expected extractors)" $
      analyzeSchemaState [] Nothing `shouldBe` SchemaFresh

    it "schema_version table missing → SchemaFresh (with expected extractors)" $
      analyzeSchemaState [("core", 1), ("utxo", 1)] Nothing `shouldBe` SchemaFresh

    it "all expected extractors present at expected versions → SchemaMatches" $
      analyzeSchemaState
        [("core", 1), ("utxo", 1)]
        (Just [("core", 1), ("utxo", 1)])
        `shouldBe` SchemaMatches

    it "extra extractors in DB are silently ignored" $
      analyzeSchemaState
        [("core", 1)]
        (Just [("core", 1), ("removed_feature", 1)])
        `shouldBe` SchemaMatches

    it "expected extractor missing from DB → MissingExtractor" $
      analyzeSchemaState
        [("core", 1), ("utxo", 1)]
        (Just [("core", 1)])
        `shouldBe` SchemaMismatched (MissingExtractor "utxo" 1 NE.:| [])

    it "DB version older than code → VersionAhead" $
      analyzeSchemaState
        [("core", 2)]
        (Just [("core", 1)])
        `shouldBe` SchemaMismatched (VersionAhead "core" 1 2 NE.:| [])

    it "DB version newer than code → VersionBehind" $
      analyzeSchemaState
        [("core", 1)]
        (Just [("core", 2)])
        `shouldBe` SchemaMismatched (VersionBehind "core" 2 1 NE.:| [])

    it "multiple mismatches reported in expected order" $
      analyzeSchemaState
        [("core", 1), ("utxo", 2), ("metadata", 1)]
        (Just [("core", 1), ("utxo", 1)])
        `shouldBe` SchemaMismatched
          (VersionAhead "utxo" 1 2 NE.:| [MissingExtractor "metadata" 1])

    it "empty expected extractors with present table → SchemaMatches" $
      analyzeSchemaState [] (Just [("core", 1)]) `shouldBe` SchemaMatches

  -- Each top-level group cleans up after itself
  describe "initSchema + dropSchema" $
    beforeAll_ (dropSchema coreTables coreVersions testConnStr) $
    afterAll_  (dropSchema coreTables coreVersions testConnStr) $ do

      it "creates tables that exist in pg_class" $ do
        initSchema coreTables coreVersions testConnStr
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
        dropSchema coreTables coreVersions testConnStr
        result <- queryPsql testConnStr $
          "SELECT count(*) FROM pg_tables WHERE schemaname = 'public' AND tablename IN ("
            <> coreTablesInList <> ");"
        T.strip result `shouldBe` "0"
        -- Re-create for the afterAll_ cleanup to be idempotent
        initSchema coreTables coreVersions testConnStr

  describe "schema_version tracking" $
    beforeAll_ (dropSchema coreTables coreVersions testConnStr >> initSchema coreTables coreVersions testConnStr) $
    afterAll_  (dropSchema coreTables coreVersions testConnStr) $ do

      it "creates a schema_version table" $ do
        result <- queryPsql testConnStr $
          "SELECT count(*) FROM pg_tables"
            <> " WHERE schemaname = 'public' AND tablename = 'schema_version';"
        T.strip result `shouldBe` "1"

      it "records extractor name and version" $ do
        result <- queryPsql testConnStr
          "SELECT extractor_name, version FROM schema_version ORDER BY extractor_name;"
        T.strip result `shouldBe` "core|1"

      it "records a created_at timestamp" $ do
        result <- queryPsql testConnStr
          "SELECT count(*) FROM schema_version WHERE created_at IS NOT NULL;"
        T.strip result `shouldBe` "1"

  describe "checkSchemaVersions" $
    beforeAll_ (dropSchema coreTables coreVersions testConnStr >> initSchema coreTables coreVersions testConnStr) $
    afterAll_  (dropSchema coreTables coreVersions testConnStr) $ do

      it "returns SchemaMatches when versions match" $ do
        result <- checkSchemaVersions coreVersions testConnStr
        result `shouldBe` SchemaMatches

      it "returns SchemaMismatched VersionAhead when code is ahead of DB" $ do
        let aheadVersions = [("core", 2)]
        result <- checkSchemaVersions aheadVersions testConnStr
        result `shouldBe` SchemaMismatched (VersionAhead "core" 1 2 NE.:| [])

      it "returns SchemaMismatched MissingExtractor when extractor is absent" $ do
        let extraVersions = [("core", 1), ("utxo", 1)]
        result <- checkSchemaVersions extraVersions testConnStr
        result `shouldBe` SchemaMismatched (MissingExtractor "utxo" 1 NE.:| [])

      it "returns SchemaMatches when DB has extra extractors not in code" $ do
        -- DB has "core" v1, code only checks [] — that's fine
        result <- checkSchemaVersions [] testConnStr
        result `shouldBe` SchemaMatches

  describe "initSchema requires a fresh DB" $
    beforeAll_ (dropSchema coreTables coreVersions testConnStr) $
    afterAll_  (dropSchema coreTables coreVersions testConnStr) $ do

      it "creates the expected tables on a clean DB" $ do
        initSchema coreTables coreVersions testConnStr
        result <- queryPsql testConnStr $
          "SELECT count(*) FROM pg_tables"
            <> " WHERE schemaname = 'public' AND tablename IN ("
            <> coreTablesInList <> ");"
        T.strip result `shouldBe` T.pack (show (length coreTables))

      it "fails if called on a populated DB (no longer drops + recreates)" $ do
        -- After the previous test the schema is in place; calling initSchema
        -- again must throw because CREATE TABLE on existing tables fails.
        initSchema coreTables coreVersions testConnStr
          `shouldThrow` anyIOException
