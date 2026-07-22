{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the six EpochBoundary COPY encoders: field values
-- and positions, NULL encoding, and the DbInt65 sign-bit path.
module DbSync.Schema.EpochBoundarySpec (spec) where

import Cardano.Prelude

import Data.List ((!!))

import qualified Data.ByteString.Char8 as BS8

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.EpochBoundary
import DbSync.Db.Schema.Ids
import DbSync.Db.Types (DbLovelace (..), toDbInt65)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  epochParamSpec
  epochStateSpec
  costModelSpec
  potTransferSpec
  treasurySpec
  reserveSpec

-- ---------------------------------------------------------------------------
-- * epoch_param
-- ---------------------------------------------------------------------------

epochParamSpec :: Spec
epochParamSpec = describe "encodeEpochParamCopy" $
  it "writes epoch_no in field 0 and block_id at the documented index" $ do
    let row = encodeEpochParamCopy sampleEpochParam
        fields = BS8.split '\t' (BS8.init row)
    fields !! 0 `shouldBe` "42"
    -- block_id is column 29 (0-indexed, id excluded), per tdColumns order
    fields !! 29 `shouldBe` "555"

-- ---------------------------------------------------------------------------
-- * epoch_state
-- ---------------------------------------------------------------------------

epochStateSpec :: Spec
epochStateSpec = describe "encodeEpochStateCopy" $ do
  it "encodes NULL FK columns with backslash-N" $ do
    let row = encodeEpochStateCopy (EpochState Nothing Nothing Nothing 99)
        fields = BS8.split '\t' (BS8.init row)
    fields !! 0 `shouldBe` "\\N"
    fields !! 1 `shouldBe` "\\N"
    fields !! 2 `shouldBe` "\\N"
    fields !! 3 `shouldBe` "99"

  it "encodes populated FK columns as their integer ids" $ do
    let row = encodeEpochStateCopy (EpochState (Just (CommitteeId 10)) (Just (GovActionProposalId 20)) (Just (ConstitutionId 30)) 100)
        fields = BS8.split '\t' (BS8.init row)
    fields !! 0 `shouldBe` "10"
    fields !! 1 `shouldBe` "20"
    fields !! 2 `shouldBe` "30"

-- ---------------------------------------------------------------------------
-- * cost_model
-- ---------------------------------------------------------------------------

costModelSpec :: Spec
costModelSpec = describe "encodeCostModelCopy" $
  it "encodes costs as text and hash as bytea hex" $ do
    let row = encodeCostModelCopy (CostModelId 3) (CostModel "{\"a\":1}" "\xde\xad\xbe\xef")
        fields = BS8.split '\t' (BS8.init row)
    fields !! 0 `shouldBe` "3"
    fields !! 1 `shouldBe` "{\"a\":1}"
    fields !! 2 `shouldBe` "\\\\xdeadbeef"

-- ---------------------------------------------------------------------------
-- * pot_transfer
-- ---------------------------------------------------------------------------

potTransferSpec :: Spec
potTransferSpec = describe "encodePotTransferCopy" $ do
  it "encodes positive deltas as decimal" $ do
    let row = encodePotTransferCopy (PotTransfer 0 (toDbInt65 1000) (toDbInt65 (-1000)) (TxId 9))
        fields = BS8.split '\t' (BS8.init row)
    fields !! 1 `shouldBe` "1000"
    fields !! 2 `shouldBe` "-1000"

  it "encodes Int64 minBound through the DbInt65 sign-bit path" $ do
    let row = encodePotTransferCopy (PotTransfer 0 (toDbInt65 minBound) (toDbInt65 0) (TxId 1))
        fields = BS8.split '\t' (BS8.init row)
    fields !! 1 `shouldBe` BS8.pack (show (minBound :: Int64))

-- ---------------------------------------------------------------------------
-- * treasury
-- ---------------------------------------------------------------------------

treasurySpec :: Spec
treasurySpec = describe "encodeTreasuryCopy" $
  it "encodes a positive payout" $ do
    let row = encodeTreasuryCopy (Treasury (StakeAddressId 5) 2 (toDbInt65 12345) (TxId 99))
        fields = BS8.split '\t' (BS8.init row)
    fields !! 0 `shouldBe` "5"
    fields !! 1 `shouldBe` "2"
    fields !! 2 `shouldBe` "12345"
    fields !! 3 `shouldBe` "99"

-- ---------------------------------------------------------------------------
-- * reserve
-- ---------------------------------------------------------------------------

reserveSpec :: Spec
reserveSpec = describe "encodeReserveCopy" $
  it "encodes a negative delta (reserves paying out)" $ do
    let row = encodeReserveCopy (Reserve (StakeAddressId 7) 4 (toDbInt65 (-50000)) (TxId 11))
        fields = BS8.split '\t' (BS8.init row)
    fields !! 2 `shouldBe` "-50000"

-- ---------------------------------------------------------------------------
-- * Fixtures
-- ---------------------------------------------------------------------------

sampleEpochParam :: EpochParam
sampleEpochParam = EpochParam
  { epochParamEpochNo                    = 42
  , epochParamMinFeeA                    = 44
  , epochParamMinFeeB                    = 155381
  , epochParamMaxBlockSize               = 65536
  , epochParamMaxTxSize                  = 16384
  , epochParamMaxBhSize                  = 1100
  , epochParamKeyDeposit                 = DbLovelace 2000000
  , epochParamPoolDeposit                = DbLovelace 500000000
  , epochParamMaxEpoch                   = 18
  , epochParamOptimalPoolCount           = 500
  , epochParamInfluence                  = 0.3
  , epochParamMonetaryExpandRate         = 0.003
  , epochParamTreasuryGrowthRate         = 0.2
  , epochParamDecentralisation           = 0.5
  , epochParamProtocolMajor              = 8
  , epochParamProtocolMinor              = 0
  , epochParamMinUtxoValue               = DbLovelace 0
  , epochParamMinPoolCost                = DbLovelace 340000000
  , epochParamNonce                      = Just "\x01\x02\x03"
  , epochParamCostModelId                = Just (CostModelId 9)
  , epochParamPriceMem                   = Nothing
  , epochParamPriceStep                  = Nothing
  , epochParamMaxTxExMem                 = Nothing
  , epochParamMaxTxExSteps               = Nothing
  , epochParamMaxBlockExMem              = Nothing
  , epochParamMaxBlockExSteps            = Nothing
  , epochParamMaxValSize                 = Nothing
  , epochParamCollateralPercent          = Nothing
  , epochParamMaxCollateralInputs        = Nothing
  , epochParamBlockId                    = BlockId 555
  , epochParamExtraEntropy               = Nothing
  , epochParamCoinsPerUtxoSize           = Nothing
  , epochParamPvtMotionNoConfidence      = Nothing
  , epochParamPvtCommitteeNormal         = Nothing
  , epochParamPvtCommitteeNoConfidence   = Nothing
  , epochParamPvtHardForkInitiation      = Nothing
  , epochParamDvtMotionNoConfidence      = Nothing
  , epochParamDvtCommitteeNormal         = Nothing
  , epochParamDvtCommitteeNoConfidence   = Nothing
  , epochParamDvtUpdateToConstitution    = Nothing
  , epochParamDvtHardForkInitiation      = Nothing
  , epochParamDvtPPNetworkGroup          = Nothing
  , epochParamDvtPPEconomicGroup         = Nothing
  , epochParamDvtPPTechnicalGroup        = Nothing
  , epochParamDvtPPGovGroup              = Nothing
  , epochParamDvtTreasuryWithdrawal      = Nothing
  , epochParamCommitteeMinSize           = Nothing
  , epochParamCommitteeMaxTermLength     = Nothing
  , epochParamGovActionLifetime          = Nothing
  , epochParamGovActionDeposit           = Nothing
  , epochParamDrepDeposit                = Nothing
  , epochParamDrepActivity               = Nothing
  , epochParamPvtppSecurityGroup         = Nothing
  , epochParamMinFeeRefScriptCostPerByte = Nothing
  }
