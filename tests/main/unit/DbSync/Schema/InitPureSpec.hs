{-# LANGUAGE OverloadedStrings #-}

-- | Pure schema-init decision logic: 'decideSchemaAction' and
-- 'analyzeExtractorState'. The PostgreSQL-backed behaviour of
-- 'initSchema'/'checkExtractorPresence' lives in the integration-tier
-- @DbSync.Schema.InitSpec@.
module DbSync.Schema.InitPureSpec (spec) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Init
  ( SchemaAction (..)
  , SchemaMismatch (..)
  , SchemaState (..)
  , analyzeExtractorState
  , decideSchemaAction
  )

spec :: Spec
spec = describe "DbSync.Db.Schema.Init (pure)" $ do

  describe "decideSchemaAction" $ do
    it "resync-from-genesis overrides everything: matches" $
      decideSchemaAction True SchemaMatches `shouldBe` ActionForceReinit

    it "resync-from-genesis overrides everything: fresh" $
      decideSchemaAction True SchemaFresh `shouldBe` ActionForceReinit

    it "resync-from-genesis overrides everything: mismatched" $
      let errs = MissingExtractor "core" NE.:| []
      in decideSchemaAction True (SchemaMismatched errs) `shouldBe` ActionForceReinit

    it "no force, schema matches → skip init" $
      decideSchemaAction False SchemaMatches `shouldBe` ActionSkipInit

    it "no force, fresh DB → run init" $
      decideSchemaAction False SchemaFresh `shouldBe` ActionRunInit

    it "no force, mismatched → abort with the same errors" $
      let errs = MissingExtractor "utxo" NE.:| [MissingExtractor "pool"]
      in decideSchemaAction False (SchemaMismatched errs) `shouldBe` ActionAbort errs

  describe "analyzeExtractorState" $ do
    it "no recorded extractors → SchemaFresh (no expected extractors)" $
      analyzeExtractorState [] Nothing `shouldBe` SchemaFresh

    it "no recorded extractors → SchemaFresh (with expected extractors)" $
      analyzeExtractorState ["core", "utxo"] Nothing `shouldBe` SchemaFresh

    it "all expected extractors present → SchemaMatches" $
      analyzeExtractorState
        ["core", "utxo"]
        (Just ["core", "utxo"])
        `shouldBe` SchemaMatches

    it "extra extractors in DB are silently ignored" $
      analyzeExtractorState
        ["core"]
        (Just ["core", "removed_feature"])
        `shouldBe` SchemaMatches

    it "expected extractor missing from DB → MissingExtractor" $
      analyzeExtractorState
        ["core", "utxo"]
        (Just ["core"])
        `shouldBe` SchemaMismatched (MissingExtractor "utxo" NE.:| [])

    it "multiple missing extractors reported in expected order" $
      analyzeExtractorState
        ["core", "utxo", "metadata"]
        (Just ["core"])
        `shouldBe` SchemaMismatched
          (MissingExtractor "utxo" NE.:| [MissingExtractor "metadata"])

    it "empty expected extractors with present table → SchemaMatches" $
      analyzeExtractorState [] (Just ["core"]) `shouldBe` SchemaMatches
