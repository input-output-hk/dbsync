{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'tableIndexStatements'.
module DbSync.Db.Statement.IndexesSpec (spec) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Statement.Indexes (tableIndexStatements)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Bare table with no PK and no unique constraints — the shape
-- most extractor data tables have today.
plainTable :: TableDef
plainTable = TableDef
  { tdName              = "plain"
  , tdColumns           = [ColumnDef "id" PgBigInt False]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  }

-- | Table with a primary key only.
pkTable :: TableDef
pkTable = plainTable
  { tdName       = "pk_only"
  , tdPrimaryKey = Just ["id"]
  }

-- | Table with one single-column unique constraint.
uniqueOneCol :: TableDef
uniqueOneCol = plainTable
  { tdName              = "uniq_one"
  , tdUniqueConstraints = [pure "hash"]
  }

-- | Table with one multi-column unique constraint.
uniqueMultiCol :: TableDef
uniqueMultiCol = plainTable
  { tdName              = "uniq_multi"
  , tdUniqueConstraints = ["addr_id" NE.:| ["pool_id", "epoch_no"]]
  }

-- | Table with both a PK and several unique constraints —
-- exercises the two emission paths and the per-constraint numbering.
pkAndUniques :: TableDef
pkAndUniques = plainTable
  { tdName              = "many"
  , tdPrimaryKey        = Just ["id"]
  , tdUniqueConstraints = [pure "name", "policy" NE.:| ["asset_name"]]
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DbSync.Db.Statement.Indexes" $ do

  describe "tableIndexStatements" $ do
    it "emits no statements for a table with no PK and no UNIQUE" $
      tableIndexStatements plainTable `shouldBe` []

    it "emits a single PK index for a PK-only table" $
      tableIndexStatements pkTable `shouldBe`
        [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"pk_only_pkey_idx\""
            <> " ON \"pk_only\" (\"id\")"
        ]

    it "emits one UNIQUE INDEX per single-column constraint" $
      tableIndexStatements uniqueOneCol `shouldBe`
        [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"uniq_one_unique_1_idx\""
            <> " ON \"uniq_one\" (\"hash\")"
        ]

    it "lists every column of a multi-column UNIQUE in order" $
      tableIndexStatements uniqueMultiCol `shouldBe`
        [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"uniq_multi_unique_1_idx\""
            <> " ON \"uniq_multi\" (\"addr_id\", \"pool_id\", \"epoch_no\")"
        ]

    it "emits PK first, then numbered UNIQUE indexes" $
      tableIndexStatements pkAndUniques `shouldBe`
        [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"many_pkey_idx\""
            <> " ON \"many\" (\"id\")"
        , "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"many_unique_1_idx\""
            <> " ON \"many\" (\"name\")"
        , "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"many_unique_2_idx\""
            <> " ON \"many\" (\"policy\", \"asset_name\")"
        ]
