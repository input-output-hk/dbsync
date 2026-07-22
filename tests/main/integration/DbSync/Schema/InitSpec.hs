{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for schema initialisation.
--
-- Covers 'initSchema' creating tables from 'TableDef's via @psql@,
-- 'dropSchema' removing them, and 'checkExtractorPresence' comparing
-- the enabled extractors against @dbsync_sync_state.extractors@. The
-- pure decision logic ('decideSchemaAction', 'analyzeExtractorState')
-- lives in the unit-tier @DbSync.Schema.InitPureSpec@.
--
-- Requires a running PostgreSQL instance with a @dbsync_test@ database.
-- Each @it@ arranges its own schema via 'before_', so any single case
-- runs in isolation under @--match@ regardless of order.
module DbSync.Schema.InitSpec (spec) where

import Cardano.Prelude

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Test.Hspec
  ( Spec
  , afterAll_
  , anyIOException
  , before_
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  , shouldThrow
  )

import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Init
  ( SchemaMismatch (..)
  , SchemaState (..)
  , checkExtractorPresence
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

testConnStr :: Text
testConnStr = "dbname=dbsync_test"

coreTables :: [TableDef]
coreTables = [blockTableDef, txTableDef, slotLeaderTableDef]

-- | Comma-separated single-quoted SQL list of 'coreTables' names for
-- embedding in @WHERE tablename IN (...)@ clauses.
coreTablesInList :: Text
coreTablesInList =
  T.intercalate ", " (map (\td -> "'" <> tdName td <> "'") coreTables)

coreNames :: [Text]
coreNames = ["core"]

-- | Drop then create the core schema — the arrange step for cases that
-- inspect or mutate an existing schema.
freshSchema :: IO ()
freshSchema = dropSchema coreTables testConnStr >> initSchema coreTables testConnStr

-- | Seed the @dbsync_sync_state@ singleton with the given enabled
-- extractor set so 'checkExtractorPresence' has a row to read back.
seedExtractors :: [Text] -> IO ()
seedExtractors names =
  bracket (openControlConnection testHasqlSettings) closeControlConnection $ \ctrl ->
    runAppM ctrl (seedSyncState 1 (Fingerprint "test-fp") False names)

spec :: Spec
spec = describe "DbSync.Db.Schema.Init" $ do

  describe "initSchema" $
    before_ (dropSchema coreTables testConnStr) $
    afterAll_ (dropSchema coreTables testConnStr) $ do

      it "creates the core tables on a fresh DB" $ do
        initSchema coreTables testConnStr
        result <- queryPsql testConnStr $
          "SELECT tablename FROM pg_tables"
            <> " WHERE schemaname = 'public' AND tablename IN ("
            <> coreTablesInList <> ") ORDER BY tablename;"
        T.lines (T.strip result) `shouldBe` List.sort (map tdName coreTables)

      it "fails when called on a populated DB" $ do
        -- CREATE TABLE on existing tables must throw rather than
        -- silently dropping and recreating.
        initSchema coreTables testConnStr
        initSchema coreTables testConnStr `shouldThrow` anyIOException

  describe "schema shape" $
    before_ freshSchema $
    afterAll_ (dropSchema coreTables testConnStr) $ do

      it "creates tables as UNLOGGED" $ do
        -- pg_class.relpersistence: 'u' = UNLOGGED, 'p' = permanent.
        result <- queryPsql testConnStr $
          "SELECT relname, relpersistence FROM pg_class"
            <> " WHERE relname IN (" <> coreTablesInList
            <> ") ORDER BY relname;"
        let rows = T.lines (T.strip result)
        length rows `shouldBe` length coreTables
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

  describe "dropSchema" $
    before_ freshSchema $
    afterAll_ (dropSchema coreTables testConnStr) $

      it "removes all core tables" $ do
        dropSchema coreTables testConnStr
        result <- queryPsql testConnStr $
          "SELECT count(*) FROM pg_tables WHERE schemaname = 'public' AND tablename IN ("
            <> coreTablesInList <> ");"
        T.strip result `shouldBe` "0"

  describe "extractors column" $
    before_ (freshSchema >> seedExtractors coreNames) $
    afterAll_ (dropSchema coreTables testConnStr) $

      it "records the enabled extractor set on the sync-state row" $ do
        result <- queryPsql testConnStr
          "SELECT unnest(extractors) FROM dbsync_sync_state ORDER BY 1;"
        T.lines (T.strip result) `shouldBe` List.sort coreNames

  describe "checkExtractorPresence" $
    before_ (freshSchema >> seedExtractors coreNames) $
    afterAll_ (dropSchema coreTables testConnStr) $ do

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
