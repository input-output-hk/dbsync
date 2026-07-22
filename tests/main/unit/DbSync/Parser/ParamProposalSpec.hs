-- | Field mapping from a Conway @PParamsUpdate@ to 'GenericParamProposal'.
module DbSync.Parser.ParamProposalSpec (spec) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (EpochInterval (..), StrictMaybe (..))
import qualified Cardano.Ledger.Babbage.Core as Core
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
  ( ppuCommitteeMaxTermLengthL
  , ppuCommitteeMinSizeL
  , ppuDRepActivityL
  , ppuDRepDepositL
  , ppuGovActionDepositL
  , ppuGovActionLifetimeL
  )
import Lens.Micro ((.~))
import Ouroboros.Consensus.Cardano.Block (ConwayEra)

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Parser.ParamProposal
  ( GenericParamProposal (..)
  , convertConwayParamProposal
  )

spec :: Spec
spec = describe "DbSync.Parser.ParamProposal.convertConwayParamProposal" $ do

  -- Distinct values throughout, so a transposed lens surfaces as a
  -- mismatched field rather than a coincidentally-equal one.
  it "maps each Conway update field onto the matching proposal field" $ do
    let base = Core.emptyPParamsUpdate :: Core.PParamsUpdate ConwayEra
        pu = base
          & Core.ppuMaxBBSizeL .~ SJust 100000
          & Core.ppuMaxTxSizeL .~ SJust 16384
          & Core.ppuMaxBHSizeL .~ SJust 1100
          & Core.ppuKeyDepositL .~ SJust (Coin 2000000)
          & Core.ppuPoolDepositL .~ SJust (Coin 500000000)
          & Core.ppuMinPoolCostL .~ SJust (Coin 340000000)
          & Core.ppuMaxValSizeL .~ SJust 5000
          & Core.ppuCollateralPercentageL .~ SJust 150
          & Core.ppuMaxCollateralInputsL .~ SJust 3
          & ppuCommitteeMinSizeL .~ SJust 7
          & ppuCommitteeMaxTermLengthL .~ SJust (EpochInterval 146)
          & ppuGovActionLifetimeL .~ SJust (EpochInterval 30)
          & ppuGovActionDepositL .~ SJust (Coin 100000000000)
          & ppuDRepDepositL .~ SJust (Coin 500000001)
          & ppuDRepActivityL .~ SJust (EpochInterval 20)
        gpp = convertConwayParamProposal pu
    gppMaxBlockSize gpp        `shouldBe` Just 100000
    gppMaxTxSize gpp           `shouldBe` Just 16384
    gppMaxBhSize gpp           `shouldBe` Just 1100
    gppKeyDeposit gpp          `shouldBe` Just 2000000
    gppPoolDeposit gpp         `shouldBe` Just 500000000
    gppMinPoolCost gpp         `shouldBe` Just 340000000
    gppMaxValSize gpp          `shouldBe` Just 5000
    gppCollateralPercent gpp   `shouldBe` Just 150
    gppMaxCollateralInputs gpp `shouldBe` Just 3
    gppCommitteeMinSize gpp       `shouldBe` Just 7
    gppCommitteeMaxTermLength gpp `shouldBe` Just 146
    gppGovActionLifetime gpp   `shouldBe` Just 30
    gppGovActionDeposit gpp    `shouldBe` Just 100000000000
    gppDrepDeposit gpp         `shouldBe` Just 500000001
    gppDrepActivity gpp        `shouldBe` Just 20

  it "clears the pre-Conway fields and leaves unset fields empty" $ do
    let gpp = convertConwayParamProposal (Core.emptyPParamsUpdate :: Core.PParamsUpdate ConwayEra)
    gppEpochNo gpp          `shouldBe` Nothing
    gppKey gpp              `shouldBe` Nothing
    gppProtocolMajor gpp    `shouldBe` Nothing
    gppProtocolMinor gpp    `shouldBe` Nothing
    gppMinUtxoValue gpp     `shouldBe` Nothing
    gppDecentralisation gpp `shouldBe` Nothing
    gppEntropy gpp          `shouldBe` Nothing
    gppMaxTxSize gpp        `shouldBe` Nothing
    gppGovActionDeposit gpp `shouldBe` Nothing
    gppDrepActivity gpp     `shouldBe` Nothing
