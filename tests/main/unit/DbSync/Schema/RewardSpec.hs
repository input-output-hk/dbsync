{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the ledger-derived reward tables: 'generateCreateTable'
-- must emit a valid @BIGINT GENERATED ALWAYS AS (expr) STORED@ clause
-- for @reward.earned_epoch@ and @pot_reward.earned_epoch@ (no trailing
-- @NOT NULL@ or @DEFAULT@), and the COPY encoders must write the
-- documented field values.
module DbSync.Schema.RewardSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Generate (generateCreateTable)
import DbSync.Db.Schema.Ids
  ( PoolHashId (..)
  , StakeAddressId (..)
  )
import DbSync.Db.Schema.StakeDelegation
  ( EpochStakeProgress (..)
  , Reward (..)
  , encodeEpochStakeProgressCopy
  , encodeRewardCopy
  , epochStakeTableDef
  , potRewardTableDef
  , rewardTableDef
  )
import DbSync.Db.Types (DbLovelace (..), RewardSource (..))

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  generatedColumnDdlSpec
  copyEncoderSpec

-- ---------------------------------------------------------------------------
-- DDL emission for generated columns
-- ---------------------------------------------------------------------------

-- Locks in the @BIGINT GENERATED ALWAYS AS (expr) STORED@ shape.
-- The previous 'PgGenerated' in-band code path lost the underlying
-- SQL type and emitted NOT NULL after STORED — invalid DDL. The
-- assertions below would fail under that bug.
generatedColumnDdlSpec :: Spec
generatedColumnDdlSpec = describe "generateCreateTable for tables with generated columns" $ do

  describe "reward.earned_epoch" $ do
    let ddl = generateCreateTable rewardTableDef
        earnedLine = earnedEpochLine ddl

    it "emits the underlying BIGINT type before the GENERATED clause" $ do
      earnedLine `shouldSatisfy` T.isInfixOf "\"earned_epoch\" BIGINT"
      earnedLine `shouldSatisfy` T.isInfixOf "GENERATED ALWAYS AS ("
      earnedLine `shouldSatisfy` T.isInfixOf ") STORED"

    it "wraps the canonical CASE expression inside the GENERATED ALWAYS AS clause" $ do
      earnedLine `shouldSatisfy` T.isInfixOf "type='refund'"
      earnedLine `shouldSatisfy` T.isInfixOf "spendable_epoch-2"

    it "does not append NOT NULL after STORED" $
      earnedLine `shouldSatisfy` (not . T.isInfixOf "STORED NOT NULL")

    it "does not emit a DEFAULT clause on the generated column" $
      earnedLine `shouldSatisfy` (not . T.isInfixOf "DEFAULT")

  describe "pot_reward.earned_epoch" $ do
    let ddl = generateCreateTable potRewardTableDef
        earnedLine = earnedEpochLine ddl

    it "emits BIGINT GENERATED ALWAYS AS (...) STORED" $ do
      earnedLine `shouldSatisfy` T.isInfixOf "\"earned_epoch\" BIGINT GENERATED ALWAYS AS ("
      earnedLine `shouldSatisfy` T.isInfixOf ") STORED"

    it "uses the simpler pot_reward expression (no refund branch)" $ do
      earnedLine `shouldSatisfy` T.isInfixOf "spendable_epoch-1"
      earnedLine `shouldSatisfy` (not . T.isInfixOf "type='refund'")

    it "does not append NOT NULL or DEFAULT" $ do
      earnedLine `shouldSatisfy` (not . T.isInfixOf "STORED NOT NULL")
      earnedLine `shouldSatisfy` (not . T.isInfixOf "DEFAULT")

  describe "tables without generated columns" $
    it "epoch_stake DDL emits no GENERATED clause" $ do
      let ddl = generateCreateTable epochStakeTableDef
      ddl `shouldSatisfy` (not . T.isInfixOf "GENERATED ALWAYS")

-- ---------------------------------------------------------------------------
-- COPY encoding (generated columns must not appear in the row)
-- ---------------------------------------------------------------------------

-- The COPY column list (built by 'DbSync.Db.Loader.Connection.buildColumnList')
-- filters out 'tdGeneratedColumns'. The encoder must therefore emit
-- one fewer field than the table has columns; PostgreSQL fills in the
-- generated column from its expression.
copyEncoderSpec :: Spec
copyEncoderSpec = describe "COPY encoders for IDENTITY + generated-column tables" $ do

  it "encodeRewardCopy writes addr_id, type, amount, spendable_epoch, pool_id" $ do
    let row = encodeRewardCopy sampleReward
        fields = BS8.split '\t' (BS8.init row)
    fields !! 0 `shouldBe` "7"
    fields !! 1 `shouldBe` "leader"
    fields !! 2 `shouldBe` "5000000"
    fields !! 3 `shouldBe` "210"
    fields !! 4 `shouldBe` "99"

  it "encodeEpochStakeProgressCopy renders completed as 't'/'f'" $ do
    let trueRow  = encodeEpochStakeProgressCopy (EpochStakeProgress 200 True)
        falseRow = encodeEpochStakeProgressCopy (EpochStakeProgress 200 False)
    BS8.split '\t' (BS8.init trueRow)  !! 1 `shouldBe` "t"
    BS8.split '\t' (BS8.init falseRow) !! 1 `shouldBe` "f"

-- ---------------------------------------------------------------------------
-- Helpers and fixtures
-- ---------------------------------------------------------------------------

-- | Pull the line of @ddl@ that mentions @earned_epoch@. Generated
-- DDL is one CREATE TABLE per call, so this is the column line we
-- want to inspect.
earnedEpochLine :: Text -> Text
earnedEpochLine ddl =
  T.unlines (filter (T.isInfixOf "earned_epoch") (T.lines ddl))

sampleReward :: Reward
sampleReward = Reward
  { rewardAddrId         = StakeAddressId 7
  , rewardType           = RwdLeader
  , rewardAmount         = DbLovelace 5000000
  , rewardSpendableEpoch = 210
  , rewardPoolId         = PoolHashId 99
  , rewardEarnedEpoch    = 208     -- ignored by the encoder; PG computes it
  }
