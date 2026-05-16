{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'resetSequenceSql'.
module DbSync.Db.Statement.SequencesSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Statement.Sequences (resetSequenceSql)

spec :: Spec
spec = describe "DbSync.Db.Statement.Sequences" $ do

  describe "resetSequenceSql" $ do
    it "names the sequence as <table>_id_seq" $
      resetSequenceSql "block" `shouldBe`
        "SELECT setval( 'block_id_seq', COALESCE((SELECT MAX(id) FROM \"block\"), 0) + 1, false)"

    it "quotes the table name as a SQL identifier" $
      resetSequenceSql "tx_out" `shouldBe`
        "SELECT setval( 'tx_out_id_seq', COALESCE((SELECT MAX(id) FROM \"tx_out\"), 0) + 1, false)"

    it "passes is_called=false so MAX(id) + 1 is the next value" $
      -- The third argument is what controls whether nextval returns
      -- the supplied number directly or one past it. The COALESCE
      -- + 1 form is correct only when paired with is_called=false.
      resetSequenceSql "tx" `shouldBe`
        "SELECT setval( 'tx_id_seq', COALESCE((SELECT MAX(id) FROM \"tx\"), 0) + 1, false)"
