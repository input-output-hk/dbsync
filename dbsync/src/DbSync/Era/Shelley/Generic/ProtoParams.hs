{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : DbSync.Era.Shelley.Generic.ProtoParams
Description : Era-agnostic protocol parameters + per-era builders.

The era-collapsed 'ProtoParams' record holds every protocol parameter
we care about across Shelley through Dijkstra. Fields that only
appear from a given era onwards ('ppCoinsPerUtxo' from Alonzo,
'ppGovActionDeposit' from Conway, …) carry a 'Maybe' so earlier eras
can leave them empty.

'epochProtoParams' / 'getDeposits' project the current-epoch params
out of an 'ExtLedgerState'; the per-era @fromXxxParams@ helpers do
the era-specific extraction.
-}
module DbSync.Era.Shelley.Generic.ProtoParams
  ( ProtoParams (..)
  , Deposits (..)
  , epochProtoParams
  , getDeposits
  ) where

import Cardano.Prelude

import Cardano.Ledger.Alonzo.Core
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.BaseTypes (EpochInterval, UnitInterval)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Compactible as Ledger
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Plutus.Language (Language)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano (Nonce (..))
import Ouroboros.Consensus.Cardano.Block
  ( AlonzoEra
  , BabbageEra
  , ConwayEra
  , DijkstraEra
  , LedgerState (..)
  , StandardCrypto
  )
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import Ouroboros.Consensus.Cardano.Block (CardanoBlock)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Era-collapsed view of the on-chain protocol parameters.
--
-- Fields appearing from Alonzo onwards are wrapped in 'Maybe' (and
-- set to 'Nothing' in Shelley\/Allegra\/Mary); the Conway governance
-- knobs are similarly optional until Conway.
data ProtoParams = ProtoParams
  { ppMinfeeA            :: !Natural
  , ppMinfeeB            :: !Natural
  , ppMaxBBSize          :: !Word32
  , ppMaxTxSize          :: !Word32
  , ppMaxBHSize          :: !Word16
  , ppKeyDeposit         :: !Coin
  , ppPoolDeposit        :: !Coin
  , ppMaxEpoch           :: !EpochInterval
  , ppOptimalPoolCount   :: !Word16
  , ppInfluence          :: !Rational
  , ppMonetaryExpandRate :: !UnitInterval
  , ppTreasuryGrowthRate :: !UnitInterval
  , ppDecentralisation   :: !UnitInterval
  , ppExtraEntropy       :: !Nonce
  , ppProtocolVersion    :: !Ledger.ProtVer
  , ppMinUTxOValue       :: !Coin
  , ppMinPoolCost        :: !Coin
    -- Alonzo-era additions.
  , ppCoinsPerUtxo           :: !(Maybe Coin)
  , ppCostmdls               :: !(Maybe (Map Language Alonzo.CostModel))
  , ppPriceMem               :: !(Maybe Rational)
  , ppPriceStep              :: !(Maybe Rational)
  , ppMaxTxExMem             :: !(Maybe Word64)
  , ppMaxTxExSteps           :: !(Maybe Word64)
  , ppMaxBlockExMem          :: !(Maybe Word64)
  , ppMaxBlockExSteps        :: !(Maybe Word64)
  , ppMaxValSize             :: !(Maybe Natural)
  , ppCollateralPercentage   :: !(Maybe Natural)
  , ppMaxCollateralInputs    :: !(Maybe Natural)
    -- Conway-era additions.
  , ppPoolVotingThresholds       :: !(Maybe PoolVotingThresholds)
  , ppDRepVotingThresholds       :: !(Maybe DRepVotingThresholds)
  , ppCommitteeMinSize           :: !(Maybe Natural)
  , ppCommitteeMaxTermLength     :: !(Maybe EpochInterval)
  , ppGovActionLifetime          :: !(Maybe EpochInterval)
  , ppGovActionDeposit           :: !(Maybe Natural)
  , ppDRepDeposit                :: !(Maybe Natural)
  , ppDRepActivity               :: !(Maybe EpochInterval)
  , ppMinFeeRefScriptCostPerByte :: !(Maybe Rational)
  }

-- | Key \/ pool deposits at a given ledger state.
data Deposits = Deposits
  { stakeKeyDeposit :: Coin
  , poolDeposit     :: Coin
  }

-- ---------------------------------------------------------------------------
-- * Projection
-- ---------------------------------------------------------------------------

-- | Project 'ProtoParams' out of the current ledger state.
-- Returns 'Nothing' for Byron (no Shelley-style protocol parameters).
epochProtoParams :: ExtLedgerState (CardanoBlock StandardCrypto) mk -> Maybe ProtoParams
epochProtoParams lstate =
  case ledgerState lstate of
    LedgerStateByron _     -> Nothing
    LedgerStateShelley st  -> Just $ fromShelleyParams  $ getProtoParams st
    LedgerStateAllegra st  -> Just $ fromShelleyParams  $ getProtoParams st
    LedgerStateMary st     -> Just $ fromShelleyParams  $ getProtoParams st
    LedgerStateAlonzo st   -> Just $ fromAlonzoParams   $ getProtoParams st
    LedgerStateBabbage st  -> Just $ fromBabbageParams  $ getProtoParams st
    LedgerStateConway st   -> Just $ fromConwayParams   $ getProtoParams st
    LedgerStateDijkstra st -> Just $ fromDijkstraParams $ getProtoParams st

-- | Extract the current-epoch 'PParams' out of a Shelley-family ledger state.
getProtoParams
  :: EraGov era
  => LedgerState (ShelleyBlock p era) mk
  -> PParams era
getProtoParams st = Shelley.nesEs (Consensus.shelleyLedgerState st) ^. Shelley.curPParamsEpochStateL

-- | Project the key \/ pool deposits out of the current ledger state.
-- Byron has no deposits, so returns 'Nothing'.
getDeposits :: ExtLedgerState (CardanoBlock StandardCrypto) mk -> Maybe Deposits
getDeposits lstate =
  case ledgerState lstate of
    LedgerStateByron _     -> Nothing
    LedgerStateShelley st  -> Just $ getDepositsShelley $ getProtoParams st
    LedgerStateAllegra st  -> Just $ getDepositsShelley $ getProtoParams st
    LedgerStateMary st     -> Just $ getDepositsShelley $ getProtoParams st
    LedgerStateAlonzo st   -> Just $ getDepositsShelley $ getProtoParams st
    LedgerStateBabbage st  -> Just $ getDepositsShelley $ getProtoParams st
    LedgerStateConway st   -> Just $ getDepositsShelley $ getProtoParams st
    LedgerStateDijkstra st -> Just $ getDepositsShelley $ getProtoParams st
  where
    getDepositsShelley :: EraPParams era => PParams era -> Deposits
    getDepositsShelley pp =
      Deposits
        { stakeKeyDeposit = pp ^. ppKeyDepositL
        , poolDeposit     = pp ^. ppPoolDepositL
        }

-- ---------------------------------------------------------------------------
-- * Per-era builders
-- ---------------------------------------------------------------------------

fromDijkstraParams :: PParams DijkstraEra -> ProtoParams
fromDijkstraParams params =
  ProtoParams
    { ppMinfeeA            = fromIntegral . unCoin . Ledger.fromCompact . unCoinPerByte $ params ^. ppTxFeePerByteL
    , ppMinfeeB            = fromIntegral . unCoin $ params ^. ppTxFeeFixedL
    , ppMaxBBSize          = params ^. ppMaxBBSizeL
    , ppMaxTxSize          = params ^. ppMaxTxSizeL
    , ppMaxBHSize          = params ^. ppMaxBHSizeL
    , ppKeyDeposit         = params ^. ppKeyDepositL
    , ppPoolDeposit        = params ^. ppPoolDepositL
    , ppMaxEpoch           = params ^. ppEMaxL
    , ppOptimalPoolCount   = params ^. ppNOptL
    , ppInfluence          = Ledger.unboundRational $ params ^. ppA0L
    , ppMonetaryExpandRate = params ^. ppRhoL
    , ppTreasuryGrowthRate = params ^. ppTauL
    , ppDecentralisation   = minBound   -- decentralisation fixed from Babbage on
    , ppExtraEntropy       = NeutralNonce -- no extra entropy from Babbage on
    , ppProtocolVersion    = params ^. ppProtocolVersionL
    , ppMinUTxOValue       = Coin 0
    , ppMinPoolCost        = params ^. ppMinPoolCostL
    , ppCoinsPerUtxo       = Just $ Ledger.fromCompact $ unCoinPerByte (params ^. ppCoinsPerUTxOByteL)
    , ppCostmdls           = Just $ Alonzo.costModelsValid $ params ^. ppCostModelsL
    , ppPriceMem           = Just . Ledger.unboundRational $ Alonzo.prMem   (params ^. ppPricesL)
    , ppPriceStep          = Just . Ledger.unboundRational $ Alonzo.prSteps (params ^. ppPricesL)
    , ppMaxTxExMem         = Just . fromIntegral $ Alonzo.exUnitsMem   (params ^. ppMaxTxExUnitsL)
    , ppMaxTxExSteps       = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxTxExUnitsL)
    , ppMaxBlockExMem      = Just . fromIntegral $ Alonzo.exUnitsMem   (params ^. ppMaxBlockExUnitsL)
    , ppMaxBlockExSteps    = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxBlockExUnitsL)
    , ppMaxValSize           = Just $ fromIntegral $ params ^. ppMaxValSizeL
    , ppCollateralPercentage = Just $ fromIntegral $ params ^. ppCollateralPercentageL
    , ppMaxCollateralInputs  = Just $ fromIntegral $ params ^. ppMaxCollateralInputsL
    , ppPoolVotingThresholds       = Just $ params ^. ppPoolVotingThresholdsL
    , ppDRepVotingThresholds       = Just $ params ^. ppDRepVotingThresholdsL
    , ppCommitteeMinSize           = Just $ fromIntegral $ params ^. ppCommitteeMinSizeL
    , ppCommitteeMaxTermLength     = Just $ params ^. ppCommitteeMaxTermLengthL
    , ppGovActionLifetime          = Just $ params ^. ppGovActionLifetimeL
    , ppGovActionDeposit           = Just . fromIntegral . unCoin $ params ^. ppGovActionDepositL
    , ppDRepDeposit                = Just . fromIntegral . unCoin $ params ^. ppDRepDepositL
    , ppDRepActivity               = Just $ params ^. ppDRepActivityL
    , ppMinFeeRefScriptCostPerByte = Just $ Ledger.unboundRational $ params ^. ppMinFeeRefScriptCostPerByteL
    }

fromConwayParams :: PParams ConwayEra -> ProtoParams
fromConwayParams params =
  ProtoParams
    { ppMinfeeA            = fromIntegral . unCoin . Ledger.fromCompact . unCoinPerByte $ params ^. ppTxFeePerByteL
    , ppMinfeeB            = fromIntegral . unCoin $ params ^. ppTxFeeFixedL
    , ppMaxBBSize          = params ^. ppMaxBBSizeL
    , ppMaxTxSize          = params ^. ppMaxTxSizeL
    , ppMaxBHSize          = params ^. ppMaxBHSizeL
    , ppKeyDeposit         = params ^. ppKeyDepositL
    , ppPoolDeposit        = params ^. ppPoolDepositL
    , ppMaxEpoch           = params ^. ppEMaxL
    , ppOptimalPoolCount   = params ^. ppNOptL
    , ppInfluence          = Ledger.unboundRational $ params ^. ppA0L
    , ppMonetaryExpandRate = params ^. ppRhoL
    , ppTreasuryGrowthRate = params ^. ppTauL
    , ppDecentralisation   = minBound
    , ppExtraEntropy       = NeutralNonce
    , ppProtocolVersion    = params ^. ppProtocolVersionL
    , ppMinUTxOValue       = Coin 0
    , ppMinPoolCost        = params ^. ppMinPoolCostL
    , ppCoinsPerUtxo       = Just $ Ledger.fromCompact $ unCoinPerByte (params ^. ppCoinsPerUTxOByteL)
    , ppCostmdls           = Just $ Alonzo.costModelsValid $ params ^. ppCostModelsL
    , ppPriceMem           = Just . Ledger.unboundRational $ Alonzo.prMem   (params ^. ppPricesL)
    , ppPriceStep          = Just . Ledger.unboundRational $ Alonzo.prSteps (params ^. ppPricesL)
    , ppMaxTxExMem         = Just . fromIntegral $ Alonzo.exUnitsMem   (params ^. ppMaxTxExUnitsL)
    , ppMaxTxExSteps       = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxTxExUnitsL)
    , ppMaxBlockExMem      = Just . fromIntegral $ Alonzo.exUnitsMem   (params ^. ppMaxBlockExUnitsL)
    , ppMaxBlockExSteps    = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxBlockExUnitsL)
    , ppMaxValSize           = Just $ fromIntegral $ params ^. ppMaxValSizeL
    , ppCollateralPercentage = Just $ fromIntegral $ params ^. ppCollateralPercentageL
    , ppMaxCollateralInputs  = Just $ fromIntegral $ params ^. ppMaxCollateralInputsL
    , ppPoolVotingThresholds       = Just $ params ^. ppPoolVotingThresholdsL
    , ppDRepVotingThresholds       = Just $ params ^. ppDRepVotingThresholdsL
    , ppCommitteeMinSize           = Just $ fromIntegral $ params ^. ppCommitteeMinSizeL
    , ppCommitteeMaxTermLength     = Just $ params ^. ppCommitteeMaxTermLengthL
    , ppGovActionLifetime          = Just $ params ^. ppGovActionLifetimeL
    , ppGovActionDeposit           = Just . fromIntegral . unCoin $ params ^. ppGovActionDepositL
    , ppDRepDeposit                = Just . fromIntegral . unCoin $ params ^. ppDRepDepositL
    , ppDRepActivity               = Just $ params ^. ppDRepActivityL
    , ppMinFeeRefScriptCostPerByte = Just $ Ledger.unboundRational $ params ^. ppMinFeeRefScriptCostPerByteL
    }

fromBabbageParams :: PParams BabbageEra -> ProtoParams
fromBabbageParams params =
  ProtoParams
    { ppMinfeeA            = fromIntegral . unCoin . Ledger.fromCompact . unCoinPerByte $ params ^. ppTxFeePerByteL
    , ppMinfeeB            = fromIntegral . unCoin $ params ^. ppTxFeeFixedL
    , ppMaxBBSize          = params ^. ppMaxBBSizeL
    , ppMaxTxSize          = params ^. ppMaxTxSizeL
    , ppMaxBHSize          = params ^. ppMaxBHSizeL
    , ppKeyDeposit         = params ^. ppKeyDepositL
    , ppPoolDeposit        = params ^. ppPoolDepositL
    , ppMaxEpoch           = params ^. ppEMaxL
    , ppOptimalPoolCount   = params ^. ppNOptL
    , ppInfluence          = Ledger.unboundRational $ params ^. ppA0L
    , ppMonetaryExpandRate = params ^. ppRhoL
    , ppTreasuryGrowthRate = params ^. ppTauL
    , ppDecentralisation   = minBound
    , ppExtraEntropy       = NeutralNonce
    , ppProtocolVersion    = params ^. ppProtocolVersionL
    , ppMinUTxOValue       = Coin 0
    , ppMinPoolCost        = params ^. ppMinPoolCostL
    , ppCoinsPerUtxo       = Just $ Ledger.fromCompact $ unCoinPerByte (params ^. ppCoinsPerUTxOByteL)
    , ppCostmdls           = Just $ Alonzo.costModelsValid $ params ^. ppCostModelsL
    , ppPriceMem           = Just . Ledger.unboundRational $ Alonzo.prMem   (params ^. ppPricesL)
    , ppPriceStep          = Just . Ledger.unboundRational $ Alonzo.prSteps (params ^. ppPricesL)
    , ppMaxTxExMem         = Just . fromIntegral $ Alonzo.exUnitsMem   (params ^. ppMaxTxExUnitsL)
    , ppMaxTxExSteps       = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxTxExUnitsL)
    , ppMaxBlockExMem      = Just . fromIntegral $ Alonzo.exUnitsMem   (params ^. ppMaxBlockExUnitsL)
    , ppMaxBlockExSteps    = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxBlockExUnitsL)
    , ppMaxValSize           = Just $ fromIntegral $ params ^. ppMaxValSizeL
    , ppCollateralPercentage = Just $ fromIntegral $ params ^. ppCollateralPercentageL
    , ppMaxCollateralInputs  = Just $ fromIntegral $ params ^. ppMaxCollateralInputsL
    , ppPoolVotingThresholds       = Nothing
    , ppDRepVotingThresholds       = Nothing
    , ppCommitteeMinSize           = Nothing
    , ppCommitteeMaxTermLength     = Nothing
    , ppGovActionLifetime          = Nothing
    , ppGovActionDeposit           = Nothing
    , ppDRepDeposit                = Nothing
    , ppDRepActivity               = Nothing
    , ppMinFeeRefScriptCostPerByte = Nothing
    }

fromAlonzoParams :: PParams AlonzoEra -> ProtoParams
fromAlonzoParams params =
  ProtoParams
    { ppMinfeeA            = fromIntegral . unCoin . Ledger.fromCompact . unCoinPerByte $ params ^. ppTxFeePerByteL
    , ppMinfeeB            = fromIntegral . unCoin $ params ^. ppTxFeeFixedL
    , ppMaxBBSize          = params ^. ppMaxBBSizeL
    , ppMaxTxSize          = params ^. ppMaxTxSizeL
    , ppMaxBHSize          = params ^. ppMaxBHSizeL
    , ppKeyDeposit         = params ^. ppKeyDepositL
    , ppPoolDeposit        = params ^. ppPoolDepositL
    , ppMaxEpoch           = params ^. ppEMaxL
    , ppOptimalPoolCount   = params ^. ppNOptL
    , ppInfluence          = Ledger.unboundRational $ params ^. ppA0L
    , ppMonetaryExpandRate = params ^. ppRhoL
    , ppTreasuryGrowthRate = params ^. ppTauL
    , ppDecentralisation   = params ^. ppDL
    , ppExtraEntropy       = params ^. ppExtraEntropyL
    , ppProtocolVersion    = params ^. ppProtocolVersionL
    , ppMinUTxOValue       = Coin 0
    , ppMinPoolCost        = params ^. ppMinPoolCostL
    , ppCoinsPerUtxo       = Just $ unCoinPerWord (params ^. ppCoinsPerUTxOWordL)
    , ppCostmdls           = Just $ Alonzo.costModelsValid $ params ^. ppCostModelsL
    , ppPriceMem           = Just . Ledger.unboundRational $ Alonzo.prMem   (params ^. ppPricesL)
    , ppPriceStep          = Just . Ledger.unboundRational $ Alonzo.prSteps (params ^. ppPricesL)
    , ppMaxTxExMem         = Just . fromIntegral $ Alonzo.exUnitsMem   (params ^. ppMaxTxExUnitsL)
    , ppMaxTxExSteps       = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxTxExUnitsL)
    , ppMaxBlockExMem      = Just . fromIntegral $ Alonzo.exUnitsMem   (params ^. ppMaxBlockExUnitsL)
    , ppMaxBlockExSteps    = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxBlockExUnitsL)
    , ppMaxValSize           = Just $ fromIntegral $ params ^. ppMaxValSizeL
    , ppCollateralPercentage = Just $ fromIntegral $ params ^. ppCollateralPercentageL
    , ppMaxCollateralInputs  = Just $ fromIntegral $ params ^. ppMaxCollateralInputsL
    , ppPoolVotingThresholds       = Nothing
    , ppDRepVotingThresholds       = Nothing
    , ppCommitteeMinSize           = Nothing
    , ppCommitteeMaxTermLength     = Nothing
    , ppGovActionLifetime          = Nothing
    , ppGovActionDeposit           = Nothing
    , ppDRepDeposit                = Nothing
    , ppDRepActivity               = Nothing
    , ppMinFeeRefScriptCostPerByte = Nothing
    }

fromShelleyParams
  :: (ProtVerAtMost era 6, ProtVerAtMost era 4, EraPParams era)
  => PParams era
  -> ProtoParams
fromShelleyParams params =
  ProtoParams
    { ppMinfeeA            = fromIntegral . unCoin . Ledger.fromCompact . unCoinPerByte $ params ^. ppTxFeePerByteL
    , ppMinfeeB            = fromIntegral . unCoin $ params ^. ppTxFeeFixedL
    , ppMaxBBSize          = params ^. ppMaxBBSizeL
    , ppMaxTxSize          = params ^. ppMaxTxSizeL
    , ppMaxBHSize          = params ^. ppMaxBHSizeL
    , ppKeyDeposit         = params ^. ppKeyDepositL
    , ppPoolDeposit        = params ^. ppPoolDepositL
    , ppMaxEpoch           = params ^. ppEMaxL
    , ppOptimalPoolCount   = params ^. ppNOptL
    , ppInfluence          = Ledger.unboundRational $ params ^. ppA0L
    , ppMonetaryExpandRate = params ^. ppRhoL
    , ppTreasuryGrowthRate = params ^. ppTauL
    , ppDecentralisation   = params ^. ppDL
    , ppExtraEntropy       = params ^. ppExtraEntropyL
    , ppProtocolVersion    = params ^. ppProtocolVersionL
    , ppMinUTxOValue       = params ^. ppMinUTxOValueL
    , ppMinPoolCost        = params ^. ppMinPoolCostL
    , ppCoinsPerUtxo       = Nothing
    , ppCostmdls           = Nothing
    , ppPriceMem           = Nothing
    , ppPriceStep          = Nothing
    , ppMaxTxExMem         = Nothing
    , ppMaxTxExSteps       = Nothing
    , ppMaxBlockExMem      = Nothing
    , ppMaxBlockExSteps    = Nothing
    , ppMaxValSize           = Nothing
    , ppCollateralPercentage = Nothing
    , ppMaxCollateralInputs  = Nothing
    , ppPoolVotingThresholds       = Nothing
    , ppDRepVotingThresholds       = Nothing
    , ppCommitteeMinSize           = Nothing
    , ppCommitteeMaxTermLength     = Nothing
    , ppGovActionLifetime          = Nothing
    , ppGovActionDeposit           = Nothing
    , ppDRepDeposit                = Nothing
    , ppDRepActivity               = Nothing
    , ppMinFeeRefScriptCostPerByte = Nothing
    }
