{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @epoch@ extractor's static metadata. The runtime
-- behaviour (backfill / append / delete) is exercised at the phase
-- hooks; here we only check the registration shape.
module DbSync.Extractor.EpochSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import DbSync.Db.Schema.EpochView (epochFinalizedTableDef)
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Extractor.Epoch (epochExtractor)

spec :: Spec
spec = describe "epochExtractor metadata" $ do
  it "has name 'epoch'" $
    pdName epochExtractor `shouldBe` "epoch"

  it "has version 1" $
    pdVersion epochExtractor `shouldBe` 1

  it "depends on 'core' (the block table backs the views)" $
    pdDependencies epochExtractor `shouldBe` [("core", 1)]

  it "registers exactly one table" $
    length (pdTables epochExtractor) `shouldBe` 1

  it "registers the epoch_finalized table" $
    case pdTables epochExtractor of
      td : _ -> td `shouldBe` epochFinalizedTableDef
      []     -> expectationFailure "expected one table, got none"
