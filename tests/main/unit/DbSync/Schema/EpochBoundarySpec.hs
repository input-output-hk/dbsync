{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the six EpochBoundary tables.
--
-- Verifies that each 'TableDef' has the expected shape (column
-- order, nullability, unique constraints, FK metadata) and that
-- each COPY encoder emits the right fields in the right order.
-- No PostgreSQL required.
module DbSync.Schema.EpochBoundarySpec (spec) where

import Cardano.Prelude

import Data.List ((!!), lookup)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import DbSync.Db.Schema.EpochBoundary
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , ForeignKey (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )
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
epochParamSpec = describe "epochParamTableDef" $ do
  it "is named epoch_param" $
    tdName epochParamTableDef `shouldBe` "epoch_param"

  it "is UNLOGGED during ingest" $
    tdMode epochParamTableDef `shouldBe` TableUnlogged

  it "has 55 columns (id + 54 schema fields)" $
    length (tdColumns epochParamTableDef) `shouldBe` 55

  it "has UNIQUE (epoch_no)" $
    tdUniqueConstraints epochParamTableDef
      `shouldBe` [pure "epoch_no"]

  it "declares FK on block_id to block.id" $
    tdForeignKeys epochParamTableDef
      `shouldBe` [ForeignKey "block_id" "block" "id"]

  it "declares cost_model_id as nullable" $ do
    let cols = tdColumns epochParamTableDef
        named = [(cdName c, cdNullable c) | c <- cols]
    lookup "cost_model_id" named `shouldBe` Just True

  it "declares nonce as nullable bytea" $ do
    let cols = [c | c <- tdColumns epochParamTableDef, cdName c == "nonce"]
    case cols of
      [c] -> do
        cdType c `shouldBe` PgBytea
        cdNullable c `shouldBe` True
      _ -> expectationFailure "expected exactly one nonce column"

  it "encodes a sample row as a tab-separated, newline-terminated COPY line" $ do
    let row = encodeEpochParamCopy sampleEpochParam
    BS8.last row `shouldBe` '\n'
    BS.count (fromIntegral (fromEnum '\t')) row `shouldBe` 53

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
epochStateSpec = describe "epochStateTableDef" $ do
  it "is named epoch_state" $
    tdName epochStateTableDef `shouldBe` "epoch_state"

  it "has 5 columns in the documented order" $
    map cdName (tdColumns epochStateTableDef) `shouldBe`
      [ "id", "committee_id", "no_confidence_id", "constitution_id", "epoch_no" ]

  it "marks the three FK columns as nullable" $ do
    let cols = tdColumns epochStateTableDef
        named = [(cdName c, cdNullable c) | c <- cols]
    lookup "committee_id"     named `shouldBe` Just True
    lookup "no_confidence_id" named `shouldBe` Just True
    lookup "constitution_id"  named `shouldBe` Just True

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
costModelSpec = describe "costModelTableDef" $ do
  it "is named cost_model with 3 columns" $ do
    tdName costModelTableDef `shouldBe` "cost_model"
    map cdName (tdColumns costModelTableDef) `shouldBe` ["id", "costs", "hash"]

  it "carries UNIQUE (hash)" $
    tdUniqueConstraints costModelTableDef `shouldBe` [pure "hash"]

  it "uses JSONB for costs and BYTEA for hash" $ do
    let cols = tdColumns costModelTableDef
    cdType (cols !! 1) `shouldBe` PgJsonb
    cdType (cols !! 2) `shouldBe` PgBytea

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
potTransferSpec = describe "potTransferTableDef" $ do
  it "is named pot_transfer with 5 columns" $ do
    tdName potTransferTableDef `shouldBe` "pot_transfer"
    map cdName (tdColumns potTransferTableDef) `shouldBe`
      ["id", "cert_index", "treasury", "reserves", "tx_id"]

  it "carries UNIQUE (tx_id, cert_index)" $
    tdUniqueConstraints potTransferTableDef
      `shouldBe` [NE.fromList ["tx_id", "cert_index"]]

  it "declares FK on tx_id to tx.id" $
    tdForeignKeys potTransferTableDef
      `shouldBe` [ForeignKey "tx_id" "tx" "id"]

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
treasurySpec = describe "treasuryTableDef" $ do
  it "is named treasury with 5 columns and UNIQUE (addr_id, tx_id)" $ do
    tdName treasuryTableDef `shouldBe` "treasury"
    length (tdColumns treasuryTableDef) `shouldBe` 5
    tdUniqueConstraints treasuryTableDef
      `shouldBe` [NE.fromList ["addr_id", "tx_id"]]

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
reserveSpec = describe "reserveTableDef" $ do
  it "is named reserve with 5 columns and UNIQUE (addr_id, tx_id)" $ do
    tdName reserveTableDef `shouldBe` "reserve"
    length (tdColumns reserveTableDef) `shouldBe` 5
    tdUniqueConstraints reserveTableDef
      `shouldBe` [NE.fromList ["addr_id", "tx_id"]]

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
