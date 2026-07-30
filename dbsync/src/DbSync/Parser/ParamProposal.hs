{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Era-varied protocol-parameter proposal extraction.
--
-- Pre-Conway txs carry zero or more genesis-key proposals via
-- 'Shelley.Update'. The shape of each proposal varies by era: Alonzo
-- introduced cost-model fields and dropped 'pppMinUtxoValue'; Babbage
-- dropped 'pppDecentralisation' and 'pppEntropy'; Conway abandoned
-- the genesis-key update mechanism entirely and routes parameter
-- changes through 'gov_action_proposal' instead.
--
-- The Conway converter is exported because the governance extractor
-- uses it when materialising a @param_proposal@ row from a
-- @ParameterChange@ governance action.
module DbSync.Parser.ParamProposal
  ( GenericParamProposal (..)

    -- * Per-era builders
  , shelleyParamProposal
  , alonzoParamProposal
  , babbageParamProposal
  , convertConwayParamProposal
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe, unEpochInterval)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (unCoin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
  ( DRepVotingThresholds (..)
  , PoolVotingThresholds (..)
  , ppuCommitteeMaxTermLengthL
  , ppuCommitteeMinSizeL
  , ppuDRepActivityL
  , ppuDRepDepositL
  , ppuDRepVotingThresholdsL
  , ppuGovActionDepositL
  , ppuGovActionLifetimeL
  , ppuPoolVotingThresholdsL
  )
import Cardano.Ledger.Conway.PParams (ppuMinFeeRefScriptCostPerByteL)

-- Babbage.Core re-exports everything from Allegra/Mary/Alonzo/Babbage Core,
-- matching the import pattern used by Parser/Tx.hs.
import Cardano.Ledger.Babbage.Core as Core hiding (Tx, TxOut)
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Plutus.Language (Language)
import qualified Cardano.Ledger.Plutus.ExUnits as Plutus
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))

import Ouroboros.Consensus.Cardano.Block
  ( AlonzoEra
  , BabbageEra
  , ConwayEra
  )

import DbSync.Util (unitIntervalToRational)

-- ---------------------------------------------------------------------------
-- * Type
-- ---------------------------------------------------------------------------

-- | A single protocol-parameter proposal, era-flattened.
--
-- Mirrors the original's
-- @Cardano.DbSync.Era.Shelley.Generic.ParamProposal.ParamProposal@
-- field-for-field. Most fields are 'Maybe' because a proposal only
-- carries the parameters it actually changes; per-era converters
-- below set entries to 'Nothing' for parameters that pre- or
-- post-date the proposing era.
--
-- 'gppCostmdls' is retained as the ledger's @Map Language CostModel@
-- so the extractor can dedup-write the @cost_model@ row and resolve
-- the FK without re-walking the proposal.
data GenericParamProposal = GenericParamProposal
  { gppEpochNo                    :: !(Maybe Word64)
  , gppKey                        :: !(Maybe ByteString)
  , gppMinFeeA                    :: !(Maybe Word64)
  , gppMinFeeB                    :: !(Maybe Word64)
  , gppMaxBlockSize               :: !(Maybe Word64)
  , gppMaxTxSize                  :: !(Maybe Word64)
  , gppMaxBhSize                  :: !(Maybe Word64)
  , gppKeyDeposit                 :: !(Maybe Word64)
  , gppPoolDeposit                :: !(Maybe Word64)
  , gppMaxEpoch                   :: !(Maybe Word64)
  , gppOptimalPoolCount           :: !(Maybe Word64)
  , gppInfluence                  :: !(Maybe Rational)
  , gppMonetaryExpandRate         :: !(Maybe Rational)
  , gppTreasuryGrowthRate         :: !(Maybe Rational)
  , gppDecentralisation           :: !(Maybe Rational)
  , gppEntropy                    :: !(Maybe ByteString)
  , gppProtocolMajor              :: !(Maybe Word16)
  , gppProtocolMinor              :: !(Maybe Word16)
  , gppMinUtxoValue               :: !(Maybe Word64)
  , gppMinPoolCost                :: !(Maybe Word64)
  , gppCoinsPerUtxoSize           :: !(Maybe Word64)
  , gppCostmdls                   :: !(Maybe (Map Language Alonzo.CostModel))
  , gppPriceMem                   :: !(Maybe Rational)
  , gppPriceStep                  :: !(Maybe Rational)
  , gppMaxTxExMem                 :: !(Maybe Word64)
  , gppMaxTxExSteps               :: !(Maybe Word64)
  , gppMaxBlockExMem              :: !(Maybe Word64)
  , gppMaxBlockExSteps            :: !(Maybe Word64)
  , gppMaxValSize                 :: !(Maybe Word64)
  , gppCollateralPercent          :: !(Maybe Word16)
  , gppMaxCollateralInputs        :: !(Maybe Word16)
  , gppPvtMotionNoConfidence      :: !(Maybe Rational)
  , gppPvtCommitteeNormal         :: !(Maybe Rational)
  , gppPvtCommitteeNoConfidence   :: !(Maybe Rational)
  , gppPvtHardForkInitiation      :: !(Maybe Rational)
  , gppPvtppSecurityGroup         :: !(Maybe Rational)
  , gppDvtMotionNoConfidence      :: !(Maybe Rational)
  , gppDvtCommitteeNormal         :: !(Maybe Rational)
  , gppDvtCommitteeNoConfidence   :: !(Maybe Rational)
  , gppDvtUpdateToConstitution    :: !(Maybe Rational)
  , gppDvtHardForkInitiation      :: !(Maybe Rational)
  , gppDvtPPNetworkGroup          :: !(Maybe Rational)
  , gppDvtPPEconomicGroup         :: !(Maybe Rational)
  , gppDvtPPTechnicalGroup        :: !(Maybe Rational)
  , gppDvtPPGovGroup              :: !(Maybe Rational)
  , gppDvtTreasuryWithdrawal      :: !(Maybe Rational)
  , gppCommitteeMinSize           :: !(Maybe Word64)
  , gppCommitteeMaxTermLength     :: !(Maybe Word64)
  , gppGovActionLifetime          :: !(Maybe Word64)
  , gppGovActionDeposit           :: !(Maybe Word64)
  , gppDrepDeposit                :: !(Maybe Word64)
  , gppDrepActivity               :: !(Maybe Word64)
  , gppMinFeeRefScriptCostPerByte :: !(Maybe Rational)
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- * Per-era builders
-- ---------------------------------------------------------------------------

-- | Shelley/Allegra/Mary share the same proposal shape: pre-Alonzo
-- fields are populated, every Alonzo+ field defaults to 'Nothing'.
shelleyParamProposal
  :: ( Core.EraPParams era
     , Core.ProtVerAtMost era 4
     , Core.ProtVerAtMost era 6
     , Core.ProtVerAtMost era 8
     )
  => EpochNo
  -> Shelley.ProposedPPUpdates era
  -> [GenericParamProposal]
shelleyParamProposal epoch (Shelley.ProposedPPUpdates umap) =
  map (convertShelleyParamProposal epoch) (Map.toList umap)

-- | Alonzo proposals: every Alonzo-introduced field is meaningful;
-- 'gppMinUtxoValue' is 'Nothing' (removed in Alonzo).
alonzoParamProposal
  :: EpochNo
  -> Shelley.ProposedPPUpdates AlonzoEra
  -> [GenericParamProposal]
alonzoParamProposal epoch (Shelley.ProposedPPUpdates umap) =
  map (convertAlonzoParamProposal epoch) (Map.toList umap)

-- | Babbage proposals: same as Alonzo with 'gppDecentralisation' and
-- 'gppEntropy' set to 'Nothing' (both removed in Babbage).
babbageParamProposal
  :: EpochNo
  -> Shelley.ProposedPPUpdates BabbageEra
  -> [GenericParamProposal]
babbageParamProposal epoch (Shelley.ProposedPPUpdates umap) =
  map (convertBabbageParamProposal epoch) (Map.toList umap)

-- | Conway proposals come from a governance action's embedded params,
-- not from genesis-key updates. The Conway converter therefore has no
-- @epoch@ or @key@ — those default to 'Nothing'. The governance
-- extractor calls this helper when a @ParameterChange@ proposal lands.
convertConwayParamProposal :: PParamsUpdate ConwayEra -> GenericParamProposal
convertConwayParamProposal pmap =
  emptyProposal
    { gppEpochNo                    = Nothing  -- pre-Conway
    , gppKey                        = Nothing  -- pre-Conway
    , gppMinFeeA                    =
        fromIntegral . unCoin . fromCompact . Core.unCoinPerByte
          <$> strictMaybeToMaybe (pmap ^. Core.ppuTxFeePerByteL)
    , gppMinFeeB                    =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuTxFeeFixedL)
    , gppMaxBlockSize               =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxBBSizeL)
    , gppMaxTxSize                  =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxTxSizeL)
    , gppMaxBhSize                  =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxBHSizeL)
    , gppKeyDeposit                 =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuKeyDepositL)
    , gppPoolDeposit                =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuPoolDepositL)
    , gppMaxEpoch                   =
        fromIntegral . unEpochInterval
          <$> strictMaybeToMaybe (pmap ^. Core.ppuEMaxL)
    , gppOptimalPoolCount           =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuNOptL)
    , gppInfluence                  =
        Ledger.unboundRational
          <$> strictMaybeToMaybe (pmap ^. Core.ppuA0L)
    , gppMonetaryExpandRate         =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuRhoL)
    , gppTreasuryGrowthRate         =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuTauL)
    , gppDecentralisation           = Nothing  -- removed in Babbage
    , gppEntropy                    = Nothing  -- removed in Babbage
    , gppProtocolMajor              = Nothing  -- removed in Conway
    , gppProtocolMinor              = Nothing  -- removed in Conway
    , gppMinUtxoValue               = Nothing  -- removed in Alonzo
    , gppMinPoolCost                =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuMinPoolCostL)
    , gppCoinsPerUtxoSize           =
        fromIntegral . unCoin . fromCompact . Core.unCoinPerByte
          <$> strictMaybeToMaybe (pmap ^. Core.ppuCoinsPerUTxOByteL)
    , gppCostmdls                   =
        strictMaybeToMaybe
          (Alonzo.costModelsValid <$> pmap ^. ppuCostModelsL)
    , gppPriceMem                   =
        Ledger.unboundRational . Alonzo.prMem
          <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , gppPriceStep                  =
        Ledger.unboundRational . Alonzo.prSteps
          <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , gppMaxTxExMem                 =
        fromIntegral . Plutus.exUnitsMem
          <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , gppMaxTxExSteps               =
        fromIntegral . Plutus.exUnitsSteps
          <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , gppMaxBlockExMem              =
        fromIntegral . Plutus.exUnitsMem
          <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , gppMaxBlockExSteps            =
        fromIntegral . Plutus.exUnitsSteps
          <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , gppMaxValSize                 =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. ppuMaxValSizeL)
    , gppCollateralPercent          =
        strictMaybeToMaybe (pmap ^. ppuCollateralPercentageL)
    , gppMaxCollateralInputs        =
        strictMaybeToMaybe (pmap ^. ppuMaxCollateralInputsL)
    , gppPvtMotionNoConfidence      =
        unitIntervalToRational . pvtMotionNoConfidence
          <$> strictMaybeToMaybe (pmap ^. ppuPoolVotingThresholdsL)
    , gppPvtCommitteeNormal         =
        unitIntervalToRational . pvtCommitteeNormal
          <$> strictMaybeToMaybe (pmap ^. ppuPoolVotingThresholdsL)
    , gppPvtCommitteeNoConfidence   =
        unitIntervalToRational . pvtCommitteeNoConfidence
          <$> strictMaybeToMaybe (pmap ^. ppuPoolVotingThresholdsL)
    , gppPvtHardForkInitiation      =
        unitIntervalToRational . pvtHardForkInitiation
          <$> strictMaybeToMaybe (pmap ^. ppuPoolVotingThresholdsL)
    , gppPvtppSecurityGroup         =
        unitIntervalToRational . pvtPPSecurityGroup
          <$> strictMaybeToMaybe (pmap ^. ppuPoolVotingThresholdsL)
    , gppDvtMotionNoConfidence      =
        unitIntervalToRational . dvtMotionNoConfidence
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppDvtCommitteeNormal         =
        unitIntervalToRational . dvtCommitteeNormal
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppDvtCommitteeNoConfidence   =
        unitIntervalToRational . dvtCommitteeNoConfidence
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppDvtUpdateToConstitution    =
        unitIntervalToRational . dvtUpdateToConstitution
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppDvtHardForkInitiation      =
        unitIntervalToRational . dvtHardForkInitiation
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppDvtPPNetworkGroup          =
        unitIntervalToRational . dvtPPNetworkGroup
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppDvtPPEconomicGroup         =
        unitIntervalToRational . dvtPPEconomicGroup
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppDvtPPTechnicalGroup        =
        unitIntervalToRational . dvtPPTechnicalGroup
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppDvtPPGovGroup              =
        unitIntervalToRational . dvtPPGovGroup
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppDvtTreasuryWithdrawal      =
        unitIntervalToRational . dvtTreasuryWithdrawal
          <$> strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , gppCommitteeMinSize           =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. ppuCommitteeMinSizeL)
    , gppCommitteeMaxTermLength     =
        fromIntegral . unEpochInterval
          <$> strictMaybeToMaybe (pmap ^. ppuCommitteeMaxTermLengthL)
    , gppGovActionLifetime          =
        fromIntegral . unEpochInterval
          <$> strictMaybeToMaybe (pmap ^. ppuGovActionLifetimeL)
    , gppGovActionDeposit           =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. ppuGovActionDepositL)
    , gppDrepDeposit                =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. ppuDRepDepositL)
    , gppDrepActivity               =
        fromIntegral . unEpochInterval
          <$> strictMaybeToMaybe (pmap ^. ppuDRepActivityL)
    , gppMinFeeRefScriptCostPerByte =
        Ledger.unboundRational
          <$> strictMaybeToMaybe (pmap ^. ppuMinFeeRefScriptCostPerByteL)
    }

-- ---------------------------------------------------------------------------
-- * Internal: per-era converters
-- ---------------------------------------------------------------------------

convertShelleyParamProposal
  :: ( Core.EraPParams era
     , Core.ProtVerAtMost era 4
     , Core.ProtVerAtMost era 6
     , Core.ProtVerAtMost era 8
     )
  => EpochNo
  -> (Ledger.KeyHash genesis, PParamsUpdate era)
  -> GenericParamProposal
convertShelleyParamProposal epoch (key, pmap) =
  emptyProposal
    { gppEpochNo            = Just (unEpochNo epoch)
    , gppKey                = Just (unKeyHashRaw key)
    , gppMinFeeA            =
        fromIntegral . unCoin . fromCompact . Core.unCoinPerByte
          <$> strictMaybeToMaybe (pmap ^. Core.ppuTxFeePerByteL)
    , gppMinFeeB            =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuTxFeeFixedL)
    , gppMaxBlockSize       =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxBBSizeL)
    , gppMaxTxSize          =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxTxSizeL)
    , gppMaxBhSize          =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxBHSizeL)
    , gppKeyDeposit         =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuKeyDepositL)
    , gppPoolDeposit        =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuPoolDepositL)
    , gppMaxEpoch           =
        fromIntegral . unEpochInterval
          <$> strictMaybeToMaybe (pmap ^. Core.ppuEMaxL)
    , gppOptimalPoolCount   =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuNOptL)
    , gppInfluence          =
        Ledger.unboundRational
          <$> strictMaybeToMaybe (pmap ^. Core.ppuA0L)
    , gppMonetaryExpandRate =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuRhoL)
    , gppTreasuryGrowthRate =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuTauL)
    , gppDecentralisation   =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuDL)
    , gppEntropy            = nonceBytes =<< strictMaybeToMaybe (pmap ^. Core.ppuExtraEntropyL)
    , gppProtocolMajor      = fst <$> protoVer pmap
    , gppProtocolMinor      = snd <$> protoVer pmap
    , gppMinUtxoValue       =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuMinUTxOValueL)
    , gppMinPoolCost        =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuMinPoolCostL)
    }

convertAlonzoParamProposal
  :: EpochNo
  -> (Ledger.KeyHash genesis, PParamsUpdate AlonzoEra)
  -> GenericParamProposal
convertAlonzoParamProposal epoch (key, pmap) =
  emptyProposal
    { gppEpochNo            = Just (unEpochNo epoch)
    , gppKey                = Just (unKeyHashRaw key)
    , gppMinFeeA            =
        fromIntegral . unCoin . fromCompact . Core.unCoinPerByte
          <$> strictMaybeToMaybe (pmap ^. Core.ppuTxFeePerByteL)
    , gppMinFeeB            =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuTxFeeFixedL)
    , gppMaxBlockSize       =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxBBSizeL)
    , gppMaxTxSize          =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxTxSizeL)
    , gppMaxBhSize          =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxBHSizeL)
    , gppKeyDeposit         =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuKeyDepositL)
    , gppPoolDeposit        =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuPoolDepositL)
    , gppMaxEpoch           =
        fromIntegral . unEpochInterval
          <$> strictMaybeToMaybe (pmap ^. Core.ppuEMaxL)
    , gppOptimalPoolCount   =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuNOptL)
    , gppInfluence          =
        Ledger.unboundRational
          <$> strictMaybeToMaybe (pmap ^. Core.ppuA0L)
    , gppMonetaryExpandRate =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuRhoL)
    , gppTreasuryGrowthRate =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuTauL)
    , gppDecentralisation   =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuDL)
    , gppEntropy            = nonceBytes =<< strictMaybeToMaybe (pmap ^. Core.ppuExtraEntropyL)
    , gppProtocolMajor      = fst <$> protoVer pmap
    , gppProtocolMinor      = snd <$> protoVer pmap
    , gppMinUtxoValue       = Nothing  -- removed in Alonzo
    , gppMinPoolCost        =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuMinPoolCostL)
    , gppCoinsPerUtxoSize   =
        fromIntegral . unCoin . unCoinPerWord
          <$> strictMaybeToMaybe (pmap ^. ppuCoinsPerUTxOWordL)
    , gppCostmdls           =
        strictMaybeToMaybe
          (Alonzo.costModelsValid <$> pmap ^. ppuCostModelsL)
    , gppPriceMem           =
        Ledger.unboundRational . Alonzo.prMem
          <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , gppPriceStep          =
        Ledger.unboundRational . Alonzo.prSteps
          <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , gppMaxTxExMem         =
        fromIntegral . Plutus.exUnitsMem
          <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , gppMaxTxExSteps       =
        fromIntegral . Plutus.exUnitsSteps
          <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , gppMaxBlockExMem      =
        fromIntegral . Plutus.exUnitsMem
          <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , gppMaxBlockExSteps    =
        fromIntegral . Plutus.exUnitsSteps
          <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , gppMaxValSize         =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. ppuMaxValSizeL)
    , gppCollateralPercent  =
        strictMaybeToMaybe (pmap ^. ppuCollateralPercentageL)
    , gppMaxCollateralInputs =
        strictMaybeToMaybe (pmap ^. ppuMaxCollateralInputsL)
    }

convertBabbageParamProposal
  :: EpochNo
  -> (Ledger.KeyHash genesis, PParamsUpdate BabbageEra)
  -> GenericParamProposal
convertBabbageParamProposal epoch (key, pmap) =
  emptyProposal
    { gppEpochNo            = Just (unEpochNo epoch)
    , gppKey                = Just (unKeyHashRaw key)
    , gppMinFeeA            =
        fromIntegral . unCoin . fromCompact . Core.unCoinPerByte
          <$> strictMaybeToMaybe (pmap ^. Core.ppuTxFeePerByteL)
    , gppMinFeeB            =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuTxFeeFixedL)
    , gppMaxBlockSize       =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxBBSizeL)
    , gppMaxTxSize          =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxTxSizeL)
    , gppMaxBhSize          =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuMaxBHSizeL)
    , gppKeyDeposit         =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuKeyDepositL)
    , gppPoolDeposit        =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuPoolDepositL)
    , gppMaxEpoch           =
        fromIntegral . unEpochInterval
          <$> strictMaybeToMaybe (pmap ^. Core.ppuEMaxL)
    , gppOptimalPoolCount   =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. Core.ppuNOptL)
    , gppInfluence          =
        Ledger.unboundRational
          <$> strictMaybeToMaybe (pmap ^. Core.ppuA0L)
    , gppMonetaryExpandRate =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuRhoL)
    , gppTreasuryGrowthRate =
        unitIntervalToRational <$> strictMaybeToMaybe (pmap ^. Core.ppuTauL)
    , gppDecentralisation   = Nothing  -- removed in Babbage
    , gppEntropy            = Nothing  -- removed in Babbage
    , gppProtocolMajor      = fst <$> protoVer pmap
    , gppProtocolMinor      = snd <$> protoVer pmap
    , gppMinUtxoValue       = Nothing  -- removed in Alonzo
    , gppMinPoolCost        =
        fromIntegral . unCoin
          <$> strictMaybeToMaybe (pmap ^. Core.ppuMinPoolCostL)
    , gppCoinsPerUtxoSize   =
        fromIntegral . unCoin . fromCompact . Core.unCoinPerByte
          <$> strictMaybeToMaybe (pmap ^. Core.ppuCoinsPerUTxOByteL)
    , gppCostmdls           =
        strictMaybeToMaybe
          (Alonzo.costModelsValid <$> pmap ^. ppuCostModelsL)
    , gppPriceMem           =
        Ledger.unboundRational . Alonzo.prMem
          <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , gppPriceStep          =
        Ledger.unboundRational . Alonzo.prSteps
          <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , gppMaxTxExMem         =
        fromIntegral . Plutus.exUnitsMem
          <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , gppMaxTxExSteps       =
        fromIntegral . Plutus.exUnitsSteps
          <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , gppMaxBlockExMem      =
        fromIntegral . Plutus.exUnitsMem
          <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , gppMaxBlockExSteps    =
        fromIntegral . Plutus.exUnitsSteps
          <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , gppMaxValSize         =
        fromIntegral <$> strictMaybeToMaybe (pmap ^. ppuMaxValSizeL)
    , gppCollateralPercent  =
        strictMaybeToMaybe (pmap ^. ppuCollateralPercentageL)
    , gppMaxCollateralInputs =
        strictMaybeToMaybe (pmap ^. ppuMaxCollateralInputsL)
    }

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | A 'GenericParamProposal' with every optional field at 'Nothing'.
-- Per-era converters override the fields the era supports.
emptyProposal :: GenericParamProposal
emptyProposal = GenericParamProposal
  { gppEpochNo                    = Nothing
  , gppKey                        = Nothing
  , gppMinFeeA                    = Nothing
  , gppMinFeeB                    = Nothing
  , gppMaxBlockSize               = Nothing
  , gppMaxTxSize                  = Nothing
  , gppMaxBhSize                  = Nothing
  , gppKeyDeposit                 = Nothing
  , gppPoolDeposit                = Nothing
  , gppMaxEpoch                   = Nothing
  , gppOptimalPoolCount           = Nothing
  , gppInfluence                  = Nothing
  , gppMonetaryExpandRate         = Nothing
  , gppTreasuryGrowthRate         = Nothing
  , gppDecentralisation           = Nothing
  , gppEntropy                    = Nothing
  , gppProtocolMajor              = Nothing
  , gppProtocolMinor              = Nothing
  , gppMinUtxoValue               = Nothing
  , gppMinPoolCost                = Nothing
  , gppCoinsPerUtxoSize           = Nothing
  , gppCostmdls                   = Nothing
  , gppPriceMem                   = Nothing
  , gppPriceStep                  = Nothing
  , gppMaxTxExMem                 = Nothing
  , gppMaxTxExSteps               = Nothing
  , gppMaxBlockExMem              = Nothing
  , gppMaxBlockExSteps            = Nothing
  , gppMaxValSize                 = Nothing
  , gppCollateralPercent          = Nothing
  , gppMaxCollateralInputs        = Nothing
  , gppPvtMotionNoConfidence      = Nothing
  , gppPvtCommitteeNormal         = Nothing
  , gppPvtCommitteeNoConfidence   = Nothing
  , gppPvtHardForkInitiation      = Nothing
  , gppPvtppSecurityGroup         = Nothing
  , gppDvtMotionNoConfidence      = Nothing
  , gppDvtCommitteeNormal         = Nothing
  , gppDvtCommitteeNoConfidence   = Nothing
  , gppDvtUpdateToConstitution    = Nothing
  , gppDvtHardForkInitiation      = Nothing
  , gppDvtPPNetworkGroup          = Nothing
  , gppDvtPPEconomicGroup         = Nothing
  , gppDvtPPTechnicalGroup        = Nothing
  , gppDvtPPGovGroup              = Nothing
  , gppDvtTreasuryWithdrawal      = Nothing
  , gppCommitteeMinSize           = Nothing
  , gppCommitteeMaxTermLength     = Nothing
  , gppGovActionLifetime          = Nothing
  , gppGovActionDeposit           = Nothing
  , gppDrepDeposit                = Nothing
  , gppDrepActivity               = Nothing
  , gppMinFeeRefScriptCostPerByte = Nothing
  }

-- | Project a 'Ledger.ProtVer' from a parameter update into the
-- @(major, minor)@ tuple stored on the row. Conway abandoned the
-- protocol-version field, hence the 'ProtVerAtMost era 8' bound.
protoVer
  :: (Core.EraPParams era, Core.ProtVerAtMost era 8)
  => Core.PParamsUpdate era
  -> Maybe (Word16, Word16)
protoVer pmap =
  fmap toTuple . strictMaybeToMaybe $ pmap ^. Core.ppuProtocolVersionL
  where
    toTuple :: Ledger.ProtVer -> (Word16, Word16)
    toTuple pv =
      ( fromIntegral @Word64 (Ledger.getVersion (Ledger.pvMajor pv))
      , fromIntegral (Ledger.pvMinor pv)
      )

-- | Project the 'Ledger.Nonce' carried in extra-entropy parameter
-- updates into its raw 32-byte payload, or 'Nothing' for the
-- neutral nonce.
nonceBytes :: Ledger.Nonce -> Maybe ByteString
nonceBytes = \case
  Ledger.Nonce h      -> Just (Crypto.hashToBytes h)
  Ledger.NeutralNonce -> Nothing

-- | Serialise a key-hash to its raw bytes.
unKeyHashRaw :: Ledger.KeyHash r -> ByteString
unKeyHashRaw (Ledger.KeyHash h) = Crypto.hashToBytes h
