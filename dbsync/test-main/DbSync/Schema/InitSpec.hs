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

import qualified Data.Text as T

import Test.Hspec (Spec, afterAll_, beforeAll_, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init
  ( checkSchemaVersions
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

-- | Extractor version entries for the core tables.
coreVersions :: [(Text, Int)]
coreVersions = [("core", 1)]

spec :: Spec
spec = describe "DbSync.Db.Schema.Init" $ do

  -- Each top-level group cleans up after itself
  describe "initSchema + dropSchema" $
    beforeAll_ (dropSchema coreTables coreVersions testConnStr) $
    afterAll_  (dropSchema coreTables coreVersions testConnStr) $ do

      it "creates tables that exist in pg_class" $ do
        initSchema coreTables coreVersions testConnStr
        result <- queryPsql testConnStr
          "SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename IN ('block', 'tx', 'slot_leader') ORDER BY tablename;"
        let tables = T.lines (T.strip result)
        tables `shouldBe` ["block", "slot_leader", "tx"]

      it "creates tables as UNLOGGED" $ do
        -- pg_class.relpersistence: 'u' = UNLOGGED, 'p' = permanent (LOGGED)
        result <- queryPsql testConnStr
          "SELECT relname, relpersistence FROM pg_class WHERE relname IN ('block', 'tx', 'slot_leader') ORDER BY relname;"
        let rows = T.lines (T.strip result)
        -- psql output: "block|u", "slot_leader|u", "tx|u"
        length rows `shouldBe` 3
        -- All should be UNLOGGED
        rows `shouldSatisfy` all (T.isInfixOf "|u")

      it "creates block table with correct column count" $ do
        result <- queryPsql testConnStr
          "SELECT count(*) FROM information_schema.columns WHERE table_name = 'block' AND table_schema = 'public';"
        T.strip result `shouldBe` "16"

      it "creates tx table with correct column count" $ do
        result <- queryPsql testConnStr
          "SELECT count(*) FROM information_schema.columns WHERE table_name = 'tx' AND table_schema = 'public';"
        T.strip result `shouldBe` "13"

      it "creates the id column as bigint NOT NULL" $ do
        result <- queryPsql testConnStr $
          "SELECT column_name, data_type, is_nullable FROM information_schema.columns "
          <> "WHERE table_name = 'block' AND column_name = 'id';"
        T.strip result `shouldBe` "id|bigint|NO"

      it "creates nullable columns correctly" $ do
        result <- queryPsql testConnStr $
          "SELECT column_name, is_nullable FROM information_schema.columns "
          <> "WHERE table_name = 'block' AND column_name = 'epoch_no';"
        T.strip result `shouldBe` "epoch_no|YES"

      it "dropSchema removes all tables" $ do
        dropSchema coreTables coreVersions testConnStr
        result <- queryPsql testConnStr
          "SELECT count(*) FROM pg_tables WHERE schemaname = 'public' AND tablename IN ('block', 'tx', 'slot_leader');"
        T.strip result `shouldBe` "0"
        -- Re-create for the afterAll_ cleanup to be idempotent
        initSchema coreTables coreVersions testConnStr

  describe "schema_version tracking" $
    beforeAll_ (dropSchema coreTables coreVersions testConnStr >> initSchema coreTables coreVersions testConnStr) $
    afterAll_  (dropSchema coreTables coreVersions testConnStr) $ do

      it "creates a schema_version table" $ do
        result <- queryPsql testConnStr
          "SELECT count(*) FROM pg_tables WHERE schemaname = 'public' AND tablename = 'schema_version';"
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

      it "returns Right when versions match" $ do
        result <- checkSchemaVersions coreVersions testConnStr
        result `shouldBe` Right ()

      it "returns Left when code version is ahead of DB" $ do
        let aheadVersions = [("core", 2)]
        result <- checkSchemaVersions aheadVersions testConnStr
        result `shouldSatisfy` isLeft

      it "returns Left when extractor is missing from DB" $ do
        let extraVersions = [("core", 1), ("utxo", 1)]
        result <- checkSchemaVersions extraVersions testConnStr
        result `shouldSatisfy` isLeft

      it "returns Right when DB has extra extractors not in code" $ do
        -- DB has "core" v1, code only checks [] — that's fine
        result <- checkSchemaVersions [] testConnStr
        result `shouldBe` Right ()

  describe "initSchema is idempotent" $
    afterAll_ (dropSchema coreTables coreVersions testConnStr) $ do

      it "can be called twice without error (drops + recreates)" $ do
        initSchema coreTables coreVersions testConnStr
        initSchema coreTables coreVersions testConnStr
        result <- queryPsql testConnStr
          "SELECT count(*) FROM pg_tables WHERE schemaname = 'public' AND tablename IN ('block', 'tx', 'slot_leader');"
        T.strip result `shouldBe` "3"
