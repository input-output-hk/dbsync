{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for 'renderChange'. No PostgreSQL required.
module DbSync.Schema.Migration.RenderSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Migration.Diff (SchemaChange (..))
import DbSync.Db.Schema.Migration.Render (renderChange)
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , ParentRef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )

spec :: Spec
spec = describe "renderChange" $ do
  it "renders AddTable via the CREATE TABLE generator" $
    renderChange (AddTable simpleTable) `shouldBe` T.unlines
      [ "CREATE UNLOGGED TABLE \"t\" ("
      , "  \"id\" BIGINT NOT NULL"
      , ");"
      ]

  it "renders DropTable with CASCADE" $
    renderChange (DropTable "foo") `shouldBe` "DROP TABLE \"foo\" CASCADE;"

  it "renders AddColumn for a nullable column" $
    renderChange (AddColumn "t" (ColumnDef "name" PgText True))
      `shouldBe` "ALTER TABLE \"t\" ADD COLUMN \"name\" TEXT;"

  it "renders AddColumn with NOT NULL for a non-nullable column" $
    renderChange (AddColumn "t" (ColumnDef "amount" PgBigInt False))
      `shouldBe` "ALTER TABLE \"t\" ADD COLUMN \"amount\" BIGINT NOT NULL;"

  it "renders DropColumn" $
    renderChange (DropColumn "t" "old")
      `shouldBe` "ALTER TABLE \"t\" DROP COLUMN \"old\";"

  it "renders AddCheck" $
    renderChange (AddCheck "t" "\"id\" > 0")
      `shouldBe` "ALTER TABLE \"t\" ADD CHECK (\"id\" > 0);"

  it "renders AddUniqueConstraint over the quoted column list" $
    renderChange (AddUniqueConstraint "t" ("a" :| ["b"]))
      `shouldBe` "ALTER TABLE \"t\" ADD UNIQUE (\"a\", \"b\");"

  it "renders AddForeignKey" $
    renderChange (AddForeignKey "t" (ParentRef "block_id" "block" "id"))
      `shouldBe`
        "ALTER TABLE \"t\" ADD FOREIGN KEY (\"block_id\") REFERENCES \"block\" (\"id\");"

  it "renders AmbiguousChange as a manual TODO comment" $
    renderChange (AmbiguousChange "t.val: type changed BIGINT -> TEXT")
      `shouldBe` "-- TODO (manual): t.val: type changed BIGINT -> TEXT"

-- ---------------------------------------------------------------------------
-- * Fixtures
-- ---------------------------------------------------------------------------

simpleTable :: TableDef
simpleTable = TableDef
  { tdName              = "t"
  , tdColumns           = [ColumnDef "id" PgBigInt False]
  , tdMode              = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns  = []
  , tdIdentityColumns   = []
  , tdParentRefs        = []
  }
