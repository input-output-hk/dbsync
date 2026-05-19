{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'tableIndexStatements', 'preResolveIndexStatements',
-- and the helpers they share ('columnRef', 'uniqueConstraintIndexName').
module DbSync.Db.Statement.IndexesSpec (spec) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE

import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

import DbSync.Db.Schema.Core (txTableDef)
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Statement.Indexes
  ( Concurrency (..)
  , columnRef
  , preResolveIndexStatements
  , tableIndexStatements
  , uniqueConstraintIndexName
  )

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
  , tdForeignKeys       = []
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

  describe "tableIndexStatements Concurrent" $ do
    it "defaults Nothing PK to id" $
      tableIndexStatements Concurrent plainTable `shouldBe`
        [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"plain_pkey_idx\""
            <> " ON \"plain\" (\"id\")"
        ]

    it "emits a single PK index for a PK-only table" $
      tableIndexStatements Concurrent pkTable `shouldBe`
        [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"pk_only_pkey_idx\""
            <> " ON \"pk_only\" (\"id\")"
        ]

    it "emits the default PK plus one UNIQUE INDEX per single-column constraint" $
      tableIndexStatements Concurrent uniqueOneCol `shouldBe`
        [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"uniq_one_pkey_idx\""
            <> " ON \"uniq_one\" (\"id\")"
        , "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"uniq_one_unique_1_idx\""
            <> " ON \"uniq_one\" (\"hash\")"
        ]

    it "lists every column of a multi-column UNIQUE in order" $
      tableIndexStatements Concurrent uniqueMultiCol `shouldBe`
        [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"uniq_multi_pkey_idx\""
            <> " ON \"uniq_multi\" (\"id\")"
        , "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"uniq_multi_unique_1_idx\""
            <> " ON \"uniq_multi\" (\"addr_id\", \"pool_id\", \"epoch_no\")"
        ]

    it "emits PK first, then numbered UNIQUE indexes" $
      tableIndexStatements Concurrent pkAndUniques `shouldBe`
        [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"many_pkey_idx\""
            <> " ON \"many\" (\"id\")"
        , "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"many_unique_1_idx\""
            <> " ON \"many\" (\"name\")"
        , "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS \"many_unique_2_idx\""
            <> " ON \"many\" (\"policy\", \"asset_name\")"
        ]

  describe "tableIndexStatements NonConcurrent" $
    it "drops the CONCURRENTLY keyword across every emission path" $
      tableIndexStatements NonConcurrent pkAndUniques `shouldBe`
        [ "CREATE UNIQUE INDEX IF NOT EXISTS \"many_pkey_idx\""
            <> " ON \"many\" (\"id\")"
        , "CREATE UNIQUE INDEX IF NOT EXISTS \"many_unique_1_idx\""
            <> " ON \"many\" (\"name\")"
        , "CREATE UNIQUE INDEX IF NOT EXISTS \"many_unique_2_idx\""
            <> " ON \"many\" (\"policy\", \"asset_name\")"
        ]

  describe "uniqueConstraintIndexName" $ do
    it "matches the name pattern tableIndexStatements emits" $
      uniqueConstraintIndexName uniqueOneCol 1 `shouldBe` "uniq_one_unique_1_idx"

    it "uses 1-based indexing across multiple constraints" $ do
      uniqueConstraintIndexName pkAndUniques 1 `shouldBe` "many_unique_1_idx"
      uniqueConstraintIndexName pkAndUniques 2 `shouldBe` "many_unique_2_idx"

  describe "columnRef" $ do
    it "returns the column name when declared on the table" $
      columnRef pkAndUniques "id" `shouldBe` "id"

    it "panics at evaluation time on an unknown column" $
      -- 'panic' from cardano-prelude raises FatalError; any exception
      -- from the eval is enough to confirm the guard fires.
      evaluate (columnRef pkAndUniques "not_a_column" :: Text)
        `shouldThrow` anyException

  describe "preResolveIndexStatements" $ do
    it "covers tx.hash plus the four per-tx-id lookups the backfills probe" $
      preResolveIndexStatements `shouldBe`
        [ "CREATE UNIQUE INDEX IF NOT EXISTS \"tx_unique_1_idx\""
            <> " ON \"tx\" (\"hash\")"
        , "CREATE INDEX IF NOT EXISTS \"tx_out_tx_id_index_idx\""
            <> " ON \"tx_out\" (\"tx_id\", \"index\")"
        , "CREATE INDEX IF NOT EXISTS \"tx_in_tx_out_idx\""
            <> " ON \"tx_in\" (\"tx_out_id\", \"tx_out_index\")"
        , "CREATE INDEX IF NOT EXISTS \"collateral_tx_in_tx_in_id_idx\""
            <> " ON \"collateral_tx_in\" (\"tx_in_id\")"
        , "CREATE INDEX IF NOT EXISTS \"collateral_tx_out_tx_id_idx\""
            <> " ON \"collateral_tx_out\" (\"tx_id\")"
        , "CREATE INDEX IF NOT EXISTS \"tx_in_tx_in_id_idx\""
            <> " ON \"tx_in\" (\"tx_in_id\")"
        , "CREATE INDEX IF NOT EXISTS \"withdrawal_tx_id_idx\""
            <> " ON \"withdrawal\" (\"tx_id\")"
        ]

    it "names the tx-hash index to match the later concurrent rebuild" $
      -- Both passes route through 'uniqueConstraintIndexName txTableDef 1',
      -- so a matching name here is what makes the later
      -- 'tableIndexStatements' build a no-op via @IF NOT EXISTS@.
      uniqueConstraintIndexName txTableDef 1 `shouldBe` "tx_unique_1_idx"
