{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for 'schemaDiff'. No PostgreSQL required.
module DbSync.Schema.Migration.DiffSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Migration.Diff (SchemaChange (..), schemaDiff)
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , ParentRef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )

spec :: Spec
spec = describe "schemaDiff" $ do
  it "reports a table only in new as AddTable" $
    schemaDiff [tableFoo] [tableFoo, tableBar]
      `shouldBe` [AddTable tableBar]

  it "reports a table only in old as DropTable" $
    schemaDiff [tableFoo, tableBar] [tableFoo]
      `shouldBe` [DropTable "bar"]

  it "reports a new nullable column as AddColumn" $
    schemaDiff [table "t" [idCol]] [table "t" [idCol, nameCol]]
      `shouldBe` [AddColumn "t" nameCol]

  it "reports a new NOT NULL column with a default as AddColumn" $ do
    let new = (table "t" [idCol, amountCol]) { tdColumnDefaults = [("amount", "0")] }
    schemaDiff [table "t" [idCol]] [new]
      `shouldBe` [AddColumn "t" amountCol]

  it "flags a new NOT NULL column without a default as ambiguous" $
    schemaDiff [table "t" [idCol]] [table "t" [idCol, amountCol]]
      `shouldBe`
        [ AmbiguousChange
            "t.amount: new NOT NULL column without default needs a backfill" ]

  it "reports a column only in old as DropColumn" $
    schemaDiff [table "t" [idCol, nameCol]] [table "t" [idCol]]
      `shouldBe` [DropColumn "t" "name"]

  it "flags a changed column type as ambiguous" $
    schemaDiff
      [table "t" [idCol, ColumnDef "val" PgBigInt False]]
      [table "t" [idCol, ColumnDef "val" PgText False]]
      `shouldBe`
        [ AmbiguousChange
            "t.val: type changed BIGINT -> TEXT — needs an explicit USING cast" ]

  it "flags a changed nullability as ambiguous" $
    schemaDiff
      [table "t" [idCol, ColumnDef "val" PgText False]]
      [table "t" [idCol, ColumnDef "val" PgText True]]
      `shouldBe`
        [ AmbiguousChange
            "t.val: nullability changed; emit ALTER COLUMN SET/DROP NOT NULL by hand" ]

  it "treats a simultaneous drop and add as a possible rename" $
    schemaDiff
      [table "t" [idCol, ColumnDef "old_name" PgText True]]
      [table "t" [idCol, ColumnDef "new_name" PgText True]]
      `shouldBe`
        [ AmbiguousChange
            "t: columns dropped (old_name) and added (new_name) — possible rename; resolve by hand" ]

  it "flags a newly generated column as ambiguous" $ do
    let computed = ColumnDef "computed" PgBigInt True
        new = (table "t" [idCol, computed]) { tdGeneratedColumns = [("computed", "id * 2")] }
    schemaDiff [table "t" [idCol, computed]] [new]
      `shouldBe`
        [ AmbiguousChange "t.computed: generated column added; review expression" ]

  it "reports a new table-level check as AddCheck" $ do
    let new = (table "t" [idCol]) { tdChecks = ["\"id\" > 0"] }
    schemaDiff [table "t" [idCol]] [new]
      `shouldBe` [AddCheck "t" "\"id\" > 0"]

  it "reports a new unique constraint as AddUniqueConstraint" $ do
    let new = (table "t" [idCol]) { tdUniqueConstraints = ["a" :| ["b"]] }
    schemaDiff [table "t" [idCol]] [new]
      `shouldBe` [AddUniqueConstraint "t" ("a" :| ["b"])]

  it "reports a new parent ref as AddForeignKey" $ do
    let cols = [idCol, ColumnDef "block_id" PgBigInt False]
        pr = ParentRef "block_id" "block" "id"
        new = (table "t" cols) { tdParentRefs = [pr] }
    schemaDiff [table "t" cols] [new]
      `shouldBe` [AddForeignKey "t" pr]

  it "ignores LOGGED/UNLOGGED mode differences" $
    schemaDiff [table "t" [idCol]] [(table "t" [idCol]) { tdMode = TableLogged }]
      `shouldBe` []

-- ---------------------------------------------------------------------------
-- * Fixtures
-- ---------------------------------------------------------------------------

table :: Text -> [ColumnDef] -> TableDef
table name cols = TableDef
  { tdName              = name
  , tdColumns           = cols
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = []
  , tdParentRefs        = []
  }

tableFoo, tableBar :: TableDef
tableFoo = table "foo" [idCol]
tableBar = table "bar" [idCol, nameCol]

idCol, nameCol, amountCol :: ColumnDef
idCol     = ColumnDef "id"     PgBigInt False
nameCol   = ColumnDef "name"   PgText   True
amountCol = ColumnDef "amount" PgBigInt False
