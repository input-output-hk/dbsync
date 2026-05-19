{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'resetSequenceSql'.
module DbSync.Db.Statement.SequencesSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Core (blockTableDef, txTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (txOutTableDef)
import DbSync.Db.Statement.Sequences (resetSequenceSql)

spec :: Spec
spec = describe "DbSync.Db.Statement.Sequences" $ do

  describe "resetSequenceSql" $ do
    it "names the sequence as <table>_id_seq" $
      resetSequenceSql (tdName blockTableDef) `shouldBe`
        expectedSql blockTableDef

    it "quotes the table name as a SQL identifier" $
      resetSequenceSql (tdName txOutTableDef) `shouldBe`
        expectedSql txOutTableDef

    it "passes is_called=false so MAX(id) + 1 is the next value" $
      -- The third argument is what controls whether nextval returns
      -- the supplied number directly or one past it. The COALESCE
      -- + 1 form is correct only when paired with is_called=false.
      resetSequenceSql (tdName txTableDef) `shouldBe`
        expectedSql txTableDef
  where
    expectedSql td =
      "SELECT setval( '" <> tdName td <> "_id_seq', "
        <> "COALESCE((SELECT MAX(id) FROM \"" <> tdName td <> "\"), 0) + 1, false)"
