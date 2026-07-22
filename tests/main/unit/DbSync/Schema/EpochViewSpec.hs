{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the @epoch@ \/ @epoch_current@ view DDL and the
-- @epoch_finalized@ COPY encoder.
module DbSync.Schema.EpochViewSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.EpochView
  ( EpochFinalized (..)
  , createEpochViewsSql
  , dropEpochViewsSql
  , encodeEpochFinalizedCopy
  , epochCurrentViewName
  , epochFinalizedTableName
  , epochViewName
  )
import DbSync.Db.Schema.Ids (EpochId (..))
import DbSync.Db.Types (DbLovelace (..))

import qualified Data.ByteString.Char8 as BS8

spec :: Spec
spec = do
  viewSqlSpec
  copyEncoderSpec

-- ---------------------------------------------------------------------------
-- * View DDL
-- ---------------------------------------------------------------------------

viewSqlSpec :: Spec
viewSqlSpec = describe "view DDL" $ do
  describe "createEpochViewsSql" $ do
    it "creates the epoch_current view" $
      createEpochViewsSql `shouldSatisfy` T.isInfixOf
        ("CREATE VIEW " <> epochCurrentViewName <> " AS")

    it "creates the epoch view" $
      createEpochViewsSql `shouldSatisfy` T.isInfixOf
        ("CREATE VIEW " <> epochViewName <> " AS")

    it "joins block + tx" $
      createEpochViewsSql `shouldSatisfy` T.isInfixOf
        "FROM block b\n  LEFT JOIN tx ON tx.block_id = b.id"

    it "guards epoch_current against rows already in epoch_finalized" $
      createEpochViewsSql `shouldSatisfy` T.isInfixOf
        ("NOT EXISTS (SELECT 1 FROM \"" <> epochFinalizedTableName <> "\" ef")

    it "unions epoch_finalized then epoch_current in the epoch view" $ do
      let body = T.lines createEpochViewsSql
          ixFinal   = findLine "FROM epoch_finalized" body
          ixUnion   = findLine "UNION ALL" body
          ixCurrent = findLine "FROM epoch_current;" body
      ixFinal `shouldSatisfy` (< ixUnion)
      ixUnion `shouldSatisfy` (< ixCurrent)

  describe "dropEpochViewsSql" $ do
    it "drops both views" $ do
      dropEpochViewsSql `shouldSatisfy` T.isInfixOf
        ("DROP VIEW IF EXISTS " <> epochViewName)
      dropEpochViewsSql `shouldSatisfy` T.isInfixOf
        ("DROP VIEW IF EXISTS " <> epochCurrentViewName)

    it "drops epoch before epoch_current (epoch depends on it)" $ do
      let body = T.lines dropEpochViewsSql
          ixEpoch   = findLine ("DROP VIEW IF EXISTS " <> epochViewName) body
          ixCurrent = findLine ("DROP VIEW IF EXISTS " <> epochCurrentViewName) body
      ixEpoch `shouldSatisfy` (< ixCurrent)

-- ---------------------------------------------------------------------------
-- * COPY encoder (symmetry; not used at runtime)
-- ---------------------------------------------------------------------------

copyEncoderSpec :: Spec
copyEncoderSpec = describe "encodeEpochFinalizedCopy" $
  it "emits id, out_sum, fees in the documented order" $ do
    let row = encodeEpochFinalizedCopy (EpochId 11) sampleEpochFinalized
        fields = BS8.split '\t' (BS8.init row)
    take 3 fields `shouldBe` ["11", "1234500000", "9876"]

-- ---------------------------------------------------------------------------
-- * Fixtures
-- ---------------------------------------------------------------------------

sampleEpochFinalized :: EpochFinalized
sampleEpochFinalized = EpochFinalized
  { epochFinalizedOutSum    = 1_234_500_000
  , epochFinalizedFees      = DbLovelace 9876
  , epochFinalizedTxCount   = 42
  , epochFinalizedBlkCount  = 21600
  , epochFinalizedNo        = 7
  , epochFinalizedStartTime = sampleTime 0
  , epochFinalizedEndTime   = sampleTime 432000
  }

sampleTime :: Integer -> UTCTime
sampleTime secs = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime secs)

-- | Index of the first line containing @needle@, or 'maxBound' when
-- not found. The tests using this assert pairwise ordering, so a
-- missing line surfaces as a giant index and fails loudly.
findLine :: Text -> [Text] -> Int
findLine needle ls = case [i | (i, l) <- zip [0 ..] ls, T.isInfixOf needle l] of
  i : _ -> i
  []    -> maxBound
