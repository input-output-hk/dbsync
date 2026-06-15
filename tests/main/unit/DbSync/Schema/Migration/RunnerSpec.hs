{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the schema-version gate decision and migration-file
-- selection. No PostgreSQL required.
module DbSync.Schema.Migration.RunnerSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Migration
  ( MigrationOutcome (..)
  , decideMigrations
  , selectMigrationSql
  )

spec :: Spec
spec = do
  describe "decideMigrations" $ do
    it "needs nothing when versions and fingerprints agree" $
      decideMigrations 3 3 "fp" "fp" `shouldBe` NoMigrationNeeded

    it "reports drift when the version matches but the fingerprint differs" $
      decideMigrations 3 3 "stored" "declared"
        `shouldBe` SchemaDriftUncovered "stored" "declared"

    it "lists the intervening versions when the database is behind" $
      decideMigrations 1 3 "x" "y" `shouldBe` MigrationsToApply (2 :| [3])

    it "rejects a database newer than the binary" $
      decideMigrations 4 2 "x" "y" `shouldBe` DbNewerThanBinary 4 2

  describe "selectMigrationSql" $ do
    it "concatenates the requested versions in order" $
      selectMigrationSql files (2 :| [3])
        `shouldBe` Right "create 2;\ncreate 3;"

    it "selects only the requested versions" $
      selectMigrationSql files (3 :| [])
        `shouldBe` Right "create 3;"

    it "fails naming versions whose files are missing" $
      selectMigrationSql files (2 :| [5])
        `shouldBe` Left "no migration file for schema version(s): 5"
  where
    files = [(2, "create 2;"), (3, "create 3;"), (4, "create 4;")]
