{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'parentRefConstraints'.
module DbSync.Db.Statement.ConstraintsSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList)

import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , ParentRef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Statement.Constraints
  ( ConstraintStatement (..)
  , parentRefConstraints
  )

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkTable :: Text -> [Text] -> [ParentRef] -> TableDef
mkTable name cols refs = TableDef
  { tdName              = name
  , tdColumns           = [ColumnDef c PgBigInt False | c <- "id" : cols]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = []
  , tdParentRefs        = refs
  }

parent :: TableDef
parent = mkTable "parent" [] []

child :: TableDef
child = mkTable "child" ["parent_id"] [ParentRef "parent_id" "parent" "id"]

-- | Two edges out of one table, one of them into a table a disabled
-- extractor would own.
orphanChild :: TableDef
orphanChild =
  mkTable "orphan" ["parent_id", "absent_id"]
    [ ParentRef "parent_id" "parent" "id"
    , ParentRef "absent_id" "absent" "id"
    ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "parentRefConstraints" $ do
  it "emits nothing for a table with no ownership edges" $
    parentRefConstraints [parent] `shouldBe` []

  it "names the constraint after the referencing table and column" $
    map csName (parentRefConstraints [parent, child])
      `shouldBe` ["child_parent_id_fkey"]

  it "renders ADD as NOT VALID so the scan is deferred to VALIDATE" $
    map csAddSql (parentRefConstraints [parent, child])
      `shouldBe`
        [ "ALTER TABLE \"child\" ADD CONSTRAINT \"child_parent_id_fkey\"\
          \ FOREIGN KEY (\"parent_id\") REFERENCES \"parent\" (\"id\")\
          \ NOT VALID;"
        ]

  it "renders VALIDATE against the referencing table" $
    map csValidateSql (parentRefConstraints [parent, child])
      `shouldBe`
        [ "ALTER TABLE \"child\" VALIDATE CONSTRAINT\
          \ \"child_parent_id_fkey\";"
        ]

  -- A profile that leaves an extractor off never created its tables, so
  -- an edge into one has to be dropped rather than fail the DDL.
  it "skips edges whose parent table is absent" $
    map csName (parentRefConstraints [parent, orphanChild])
      `shouldBe` ["orphan_parent_id_fkey"]

  it "emits one statement per edge, not per table" $
    map csTable (parentRefConstraints [parent, mkTable "absent" [] [], orphanChild])
      `shouldMatchList` ["orphan", "orphan"]
