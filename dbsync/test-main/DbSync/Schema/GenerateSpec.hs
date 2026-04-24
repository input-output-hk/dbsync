{-# LANGUAGE OverloadedStrings #-}

-- | Tests for DDL generation from 'TableDef' definitions.
module DbSync.Schema.GenerateSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Generate (generateCreateTable)
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )

spec :: Spec
spec = do
  describe "generateCreateTable" $ do
    it "generates UNLOGGED CREATE TABLE for UNLOGGED mode" $ do
      let sql = generateCreateTable slotLeaderTableDef
      sql `shouldSatisfy` T.isInfixOf "CREATE UNLOGGED TABLE"

    it "generates regular CREATE TABLE for LOGGED mode" $ do
      let tableDef = slotLeaderTableDef { tdMode = TableLogged }
          sql = generateCreateTable tableDef
      sql `shouldSatisfy` T.isInfixOf "CREATE TABLE"
      sql `shouldSatisfy` (not . T.isInfixOf "UNLOGGED")

    it "includes table name" $ do
      let sql = generateCreateTable blockTableDef
      sql `shouldSatisfy` T.isInfixOf "\"block\""

    it "includes all column definitions" $ do
      let sql = generateCreateTable slotLeaderTableDef
      sql `shouldSatisfy` T.isInfixOf "\"id\""
      sql `shouldSatisfy` T.isInfixOf "\"hash\""
      sql `shouldSatisfy` T.isInfixOf "\"pool_hash_id\""
      sql `shouldSatisfy` T.isInfixOf "\"description\""

    it "maps PgBigInt to BIGINT" $ do
      let sql = generateCreateTable slotLeaderTableDef
      sql `shouldSatisfy` T.isInfixOf "BIGINT"

    it "maps PgBytea to BYTEA" $ do
      let sql = generateCreateTable slotLeaderTableDef
      sql `shouldSatisfy` T.isInfixOf "BYTEA"

    it "maps PgText to TEXT" $ do
      let sql = generateCreateTable slotLeaderTableDef
      sql `shouldSatisfy` T.isInfixOf "TEXT"

    it "adds NOT NULL for non-nullable columns" $ do
      let sql = generateCreateTable slotLeaderTableDef
      -- "id" is NOT NULL, "pool_hash_id" is nullable
      sql `shouldSatisfy` T.isInfixOf "NOT NULL"

    it "omits NOT NULL for nullable columns" $ do
      -- The slot_leader table has pool_hash_id as nullable
      let sql = generateCreateTable slotLeaderTableDef
          -- Split into lines and find pool_hash_id line
          poolLine = T.unlines $ filter (T.isInfixOf "pool_hash_id") (T.lines sql)
      -- pool_hash_id should NOT have NOT NULL
      poolLine `shouldSatisfy` (not . T.isInfixOf "NOT NULL")

    it "generates valid SQL for the block table" $ do
      let sql = generateCreateTable blockTableDef
      -- Should have 16 columns
      sql `shouldSatisfy` T.isInfixOf "\"hash\""
      sql `shouldSatisfy` T.isInfixOf "\"epoch_no\""
      sql `shouldSatisfy` T.isInfixOf "\"slot_no\""
      sql `shouldSatisfy` T.isInfixOf "\"time\""
      sql `shouldSatisfy` T.isInfixOf "TIMESTAMP"

    it "generates valid SQL for the tx table" $ do
      let sql = generateCreateTable txTableDef
      sql `shouldSatisfy` T.isInfixOf "\"tx\""
      sql `shouldSatisfy` T.isInfixOf "\"valid_contract\""
      sql `shouldSatisfy` T.isInfixOf "BOOLEAN"
      sql `shouldSatisfy` T.isInfixOf "NUMERIC"

    it "produces a minimal correct DDL for a simple table" $ do
      let simpleDef = TableDef
            { tdName = "test_table"
            , tdColumns =
                [ ColumnDef "id"   PgBigInt  False
                , ColumnDef "name" PgText    True
                ]
            , tdMode = TableUnlogged
            , tdPrimaryKey     = Nothing
            , tdChecks         = []
            , tdColumnDefaults = []
            }
          sql = generateCreateTable simpleDef
      sql `shouldBe` T.unlines
        [ "CREATE UNLOGGED TABLE \"test_table\" ("
        , "  \"id\" BIGINT NOT NULL,"
        , "  \"name\" TEXT"
        , ");"
        ]

    it "emits inline DEFAULT expressions on columns" $ do
      let defDef = TableDef
            { tdName = "with_defaults"
            , tdColumns =
                [ ColumnDef "id"       PgSmallInt False
                , ColumnDef "counter"  PgBigInt   False
                ]
            , tdMode = TableLogged
            , tdPrimaryKey     = Nothing
            , tdChecks         = []
            , tdColumnDefaults =
                [ ("id", "1")
                , ("counter", "42")
                ]
            }
          sql = generateCreateTable defDef
      sql `shouldBe` T.unlines
        [ "CREATE TABLE \"with_defaults\" ("
        , "  \"id\" SMALLINT NOT NULL DEFAULT 1,"
        , "  \"counter\" BIGINT NOT NULL DEFAULT 42"
        , ");"
        ]

    it "emits PRIMARY KEY constraint at table level" $ do
      let pkDef = TableDef
            { tdName = "with_pk"
            , tdColumns =
                [ ColumnDef "id" PgSmallInt False ]
            , tdMode = TableLogged
            , tdPrimaryKey     = Just ["id"]
            , tdChecks         = []
            , tdColumnDefaults = []
            }
          sql = generateCreateTable pkDef
      sql `shouldBe` T.unlines
        [ "CREATE TABLE \"with_pk\" ("
        , "  \"id\" SMALLINT NOT NULL,"
        , "  PRIMARY KEY (\"id\")"
        , ");"
        ]

    it "emits table-level CHECK constraints" $ do
      let checkDef = TableDef
            { tdName = "with_check"
            , tdColumns =
                [ ColumnDef "id" PgSmallInt False ]
            , tdMode = TableLogged
            , tdPrimaryKey     = Nothing
            , tdChecks         = [ "\"id\" = 1" ]
            , tdColumnDefaults = []
            }
          sql = generateCreateTable checkDef
      sql `shouldBe` T.unlines
        [ "CREATE TABLE \"with_check\" ("
        , "  \"id\" SMALLINT NOT NULL,"
        , "  CHECK (\"id\" = 1)"
        , ");"
        ]
