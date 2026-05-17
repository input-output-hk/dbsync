{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the StakeDelegation-ledger half of
-- 'DbSync.Db.Schema.StakeDelegation' — the four ledger-derived
-- tables: @reward@, @reward_rest@, @epoch_stake@,
-- @epoch_stake_progress@.
--
-- The headline assertion is that 'generateCreateTable' emits a valid
-- @BIGINT GENERATED ALWAYS AS (expr) STORED@ DDL clause for
-- @reward.earned_epoch@ and @reward_rest.earned_epoch@: the
-- underlying SQL type comes through, the generation expression is
-- enclosed in parentheses and ends with @STORED@, and there is no
-- trailing @NOT NULL@ or @DEFAULT@ that would conflict with the
-- generated value.
--
-- Pure tests — no PostgreSQL required.
module DbSync.Schema.RewardSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Generate (generateCreateTable)
import DbSync.Db.Schema.Ids
  ( EpochStakeId (..)
  , EpochStakeProgressId (..)
  , PoolHashId (..)
  , RewardId (..)
  , RewardRestId (..)
  , StakeAddressId (..)
  )
import DbSync.Db.Schema.StakeDelegation
  ( EpochStake (..)
  , EpochStakeProgress (..)
  , Reward (..)
  , RewardRest (..)
  , encodeEpochStakeCopy
  , encodeEpochStakeProgressCopy
  , encodeRewardCopy
  , encodeRewardRestCopy
  , epochStakeProgressTableDef
  , epochStakeTableDef
  , rewardRestTableDef
  , rewardTableDef
  )
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Types (DbLovelace (..), RewardSource (..))

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  rewardTableDefSpec
  rewardRestTableDefSpec
  epochStakeTableDefSpec
  epochStakeProgressTableDefSpec
  generatedColumnDdlSpec
  copyEncoderSpec

-- ---------------------------------------------------------------------------
-- Table-shape specs
-- ---------------------------------------------------------------------------

rewardTableDefSpec :: Spec
rewardTableDefSpec = describe "rewardTableDef" $ do
  it "is named reward and is UNLOGGED during ingest" $ do
    tdName rewardTableDef `shouldBe` "reward"
    tdMode rewardTableDef `shouldBe` TableUnlogged

  it "has 7 columns in golden order" $
    map cdName (tdColumns rewardTableDef) `shouldBe`
      ["id", "addr_id", "type", "amount", "spendable_epoch", "pool_id", "earned_epoch"]

  it "marks every column NOT NULL (PG infers nullability for earned_epoch)" $
    all (not . cdNullable) (tdColumns rewardTableDef) `shouldBe` True

  it "stores type as TEXT (RewardSource enum) and amount as NUMERIC" $ do
    cdType (tdColumns rewardTableDef !! 2) `shouldBe` PgText
    cdType (tdColumns rewardTableDef !! 3) `shouldBe` PgNumeric

  it "declares earned_epoch as BIGINT (the underlying type, not 'GENERATED')" $
    cdType (tdColumns rewardTableDef !! 6) `shouldBe` PgBigInt

  it "lists earned_epoch in tdGeneratedColumns with the canonical CASE expression" $ do
    let entries = tdGeneratedColumns rewardTableDef
    map fst entries `shouldBe` ["earned_epoch"]
    let expr = snd (entries !! 0)
    expr `shouldSatisfy` T.isInfixOf "spendable_epoch"
    expr `shouldSatisfy` T.isInfixOf "type='refund'"

rewardRestTableDefSpec :: Spec
rewardRestTableDefSpec = describe "rewardRestTableDef" $ do
  it "is named reward_rest with 6 columns in golden order" $ do
    tdName rewardRestTableDef `shouldBe` "reward_rest"
    map cdName (tdColumns rewardRestTableDef) `shouldBe`
      ["id", "addr_id", "type", "amount", "spendable_epoch", "earned_epoch"]

  it "has no pool_id (separates it from reward)" $
    all (\c -> cdName c /= "pool_id") (tdColumns rewardRestTableDef) `shouldBe` True

  it "lists earned_epoch as the sole generated column" $
    map fst (tdGeneratedColumns rewardRestTableDef) `shouldBe` ["earned_epoch"]

epochStakeTableDefSpec :: Spec
epochStakeTableDefSpec = describe "epochStakeTableDef" $ do
  it "is named epoch_stake with 5 columns in golden order" $ do
    tdName epochStakeTableDef `shouldBe` "epoch_stake"
    map cdName (tdColumns epochStakeTableDef) `shouldBe`
      ["id", "addr_id", "pool_id", "amount", "epoch_no"]

  it "declares the (addr_id, pool_id, epoch_no) unique constraint" $
    tdUniqueConstraints epochStakeTableDef
      `shouldBe` ["addr_id" :| ["pool_id", "epoch_no"]]

  it "has no generated columns" $
    tdGeneratedColumns epochStakeTableDef `shouldBe` []

epochStakeProgressTableDefSpec :: Spec
epochStakeProgressTableDefSpec = describe "epochStakeProgressTableDef" $ do
  it "is named epoch_stake_progress with 3 columns" $ do
    tdName epochStakeProgressTableDef `shouldBe` "epoch_stake_progress"
    map cdName (tdColumns epochStakeProgressTableDef) `shouldBe`
      ["id", "epoch_no", "completed"]

  it "declares the (epoch_no) unique constraint" $
    tdUniqueConstraints epochStakeProgressTableDef `shouldBe` [pure "epoch_no"]

  it "stores completed as BOOLEAN NOT NULL" $ do
    let completed = tdColumns epochStakeProgressTableDef !! 2
    cdType completed `shouldBe` PgBoolean
    cdNullable completed `shouldBe` False

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

  describe "reward_rest.earned_epoch" $ do
    let ddl = generateCreateTable rewardRestTableDef
        earnedLine = earnedEpochLine ddl

    it "emits BIGINT GENERATED ALWAYS AS (...) STORED" $ do
      earnedLine `shouldSatisfy` T.isInfixOf "\"earned_epoch\" BIGINT GENERATED ALWAYS AS ("
      earnedLine `shouldSatisfy` T.isInfixOf ") STORED"

    it "uses the simpler reward_rest expression (no refund branch)" $ do
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
copyEncoderSpec = describe "COPY encoders for generated-column tables" $ do

  it "encodeRewardCopy emits 6 tab-separated fields (earned_epoch omitted)" $ do
    let row = encodeRewardCopy (RewardId 1) sampleReward
        tabs = BS.count (fromIntegral (fromEnum '\t')) row
    tabs `shouldBe` 5    -- 6 fields → 5 separators
    BS8.last row `shouldBe` '\n'

  it "encodeRewardCopy writes id, addr_id, type, amount, spendable_epoch, pool_id" $ do
    let row = encodeRewardCopy (RewardId 42) sampleReward
        fields = BS8.split '\t' (BS8.init row)
    fields !! 0 `shouldBe` "42"
    fields !! 1 `shouldBe` "7"
    fields !! 2 `shouldBe` "leader"
    fields !! 3 `shouldBe` "5000000"
    fields !! 4 `shouldBe` "210"
    fields !! 5 `shouldBe` "99"

  it "encodeRewardRestCopy emits 5 fields (earned_epoch omitted)" $ do
    let row = encodeRewardRestCopy (RewardRestId 1) sampleRewardRest
        tabs = BS.count (fromIntegral (fromEnum '\t')) row
    tabs `shouldBe` 4    -- 5 fields → 4 separators

  it "encodeEpochStakeCopy emits 5 fields (no generated columns)" $ do
    let row = encodeEpochStakeCopy (EpochStakeId 1) sampleEpochStake
        tabs = BS.count (fromIntegral (fromEnum '\t')) row
    tabs `shouldBe` 4

  it "encodeEpochStakeProgressCopy renders completed as 't'/'f'" $ do
    let trueRow  = encodeEpochStakeProgressCopy (EpochStakeProgressId 1)
                     (EpochStakeProgress 200 True)
        falseRow = encodeEpochStakeProgressCopy (EpochStakeProgressId 2)
                     (EpochStakeProgress 200 False)
    BS8.split '\t' (BS8.init trueRow)  !! 2 `shouldBe` "t"
    BS8.split '\t' (BS8.init falseRow) !! 2 `shouldBe` "f"

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

sampleRewardRest :: RewardRest
sampleRewardRest = RewardRest
  { rewardRestAddrId         = StakeAddressId 7
  , rewardRestType           = RwdReserves
  , rewardRestAmount         = DbLovelace 1000000
  , rewardRestSpendableEpoch = 210
  , rewardRestEarnedEpoch    = 209  -- ignored by the encoder
  }

sampleEpochStake :: EpochStake
sampleEpochStake = EpochStake
  { epochStakeAddrId  = StakeAddressId 7
  , epochStakePoolId  = PoolHashId 99
  , epochStakeAmount  = DbLovelace 12345678
  , epochStakeEpochNo = 210
  }
