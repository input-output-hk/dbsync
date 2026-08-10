{-# LANGUAGE OverloadedStrings #-}

-- | Owns the boundary-triggered tables @ada_pots@, @epoch_param@,
-- @epoch_state@ and @cost_model@. @pdProcess@ is a no-op: the consumer
-- calls 'runEpochBoundary' when it sees an epoch transition and the
-- LedgerWorker has produced the matching 'ApplyResult'.
--
-- With the ledger off, the consumer never calls 'runEpochBoundary' and
-- these tables stay empty. The schema still creates them, so an
-- operator can turn the ledger on without a re-sync.
module DbSync.Extractor.EpochBoundary
  ( -- * Extractor registration
    epochBoundaryExtractor

    -- * Boundary handler (called by the consumer)
  , runEpochBoundary

    -- * Internal helpers (exported for tests)
  , mkEpochParamRow
  , mkEpochStateRow
  , mkCostModelRow
  , hashCostModels
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto as Crypto
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Conway.Core
  ( DRepVotingThresholds (..)
  , PoolVotingThresholds (..)
  )
import Cardano.Ledger.Plutus.CostModels (mkCostModels)
import Cardano.Ledger.Plutus.Language (Language)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Shelley.AdaPots as Shelley
import qualified Cardano.Ledger.State as Ledger
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text.Encoding as Text

import qualified DbSync.Worker.Ledger.EpochUpdate as Generic
import DbSync.Db.Schema.AdaPots (AdaPots (..), adaPotsTableDef)
import DbSync.Db.Schema.EpochBoundary
  ( CostModel (..)
  , EpochParam (..)
  , EpochState (..)
  , costModelTableDef
  , epochParamTableDef
  , epochStateTableDef
  )
import DbSync.Db.Schema.Ids
  ( BlockId
  , CommitteeId (..)
  , ConstitutionId (..)
  , CostModelId
  , GovActionProposalId (..)
  )
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor (ExtractorDef (..))
import qualified DbSync.Worker.Ledger.ProtoParams as Proto
import DbSync.Worker.Ledger.Types (BoundaryApplyData (..))
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Util (coinToDbLovelace, nonceToBytes, unitIntervalToRational)
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor registration
-- ---------------------------------------------------------------------------

epochBoundaryExtractor :: ExtractorDef
epochBoundaryExtractor = ExtractorDef
  { pdName    = "epoch_boundary"
  , pdTables  =
      [ adaPotsTableDef
      , epochParamTableDef
      , epochStateTableDef
      , costModelTableDef
      ]
  , pdProcess = \_ -> pure ()
  }

-- ---------------------------------------------------------------------------
-- * Boundary handler
-- ---------------------------------------------------------------------------

-- | Run the epoch-boundary writes for one transition. A populated
-- @bndNewEpoch@ dispatches to the per-table writers:
--
--   * @ada_pots@ — when the ledger reports pots data, which it does
--     from Shelley onward and never from Byron.
--   * @cost_model@ — when the protocol parameters carry Plutus cost
--     models (Alonzo onward). Dedup keys on the Blake2b hash of the
--     canonical CBOR encoding.
--   * @epoch_param@ — every Shelley-onward boundary.
--   * @epoch_state@ — every Conway-onward boundary. The three
--     governance FK columns stay 'Nothing' until the governance
--     boundary pass resolves them. A pre-Conway boundary writes no
--     row.
--
-- The caller owns idempotency: two calls for one boundary write
-- duplicate rows.
runEpochBoundary
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => BoundaryApplyData
  -> BlockId
  -> m ()
runEpochBoundary applyResult blockId =
  case bndNewEpoch applyResult of
    Strict.Nothing -> pure ()
    Strict.Just newEpoch -> do
      writeBoundaryAdaPots applyResult newEpoch blockId
      writeBoundaryProtoParams newEpoch blockId
      when (isJust (bndGovActionState applyResult)) $
        writeBoundaryEpochState newEpoch

-- ---------------------------------------------------------------------------
-- * AdaPots
-- ---------------------------------------------------------------------------

-- | Writes nothing when the ledger reports no pots data.
writeBoundaryAdaPots
  :: (HasWriter env, MonadReader env m, MonadIO m)
  => BoundaryApplyData
  -> Generic.NewEpoch
  -> BlockId
  -> m ()
writeBoundaryAdaPots applyResult newEpoch blockId =
  case Generic.neAdaPots newEpoch of
    Strict.Nothing -> pure ()  -- Pre-Shelley; nothing to write
    Strict.Just pots -> do
      writer <- asks getWriter
      liftIO $ writeAdaPots writer (mkAdaPotsRow applyResult newEpoch blockId pots)

-- | The deposit pots (stake, drep, proposal) come from
-- 'Shelley.obligationsPot'; they are not fields on 'Shelley.AdaPots'
-- itself.
--
-- @utxo@ is taken /verbatim/ from the supplied pots. The caller, the
-- LedgerWorker in 'DbSync.Worker.Ledger.State.applyBlock', already
-- applied the @fixUTxOPots@ correction, so the pots sum to
-- @maxLovelaceSupply@.
mkAdaPotsRow
  :: BoundaryApplyData
  -> Generic.NewEpoch
  -> BlockId
  -> Shelley.AdaPots
  -> AdaPots
mkAdaPotsRow applyResult newEpoch blockId pots =
  AdaPots
    { adaPotsSlotNo            = unSlotNo (sdSlotNo (bndSlotDetails applyResult))
    , adaPotsEpochNo           = unEpochNo (Generic.neEpoch newEpoch)
    , adaPotsTreasury          = coinToDbLovelace (Shelley.treasuryAdaPot pots)
    , adaPotsReserves          = coinToDbLovelace (Shelley.reservesAdaPot pots)
    , adaPotsRewards           = coinToDbLovelace (Shelley.rewardsAdaPot pots)
    , adaPotsUtxo              = coinToDbLovelace (Shelley.utxoAdaPot pots)
    , adaPotsDepositsStake     =
        coinToDbLovelace (Ledger.oblStake oblgs <> Ledger.oblPool oblgs)
    , adaPotsFees              = coinToDbLovelace (Shelley.feesAdaPot pots)
    , adaPotsBlockId           = blockId
    , adaPotsDepositsDrep      = coinToDbLovelace (Ledger.oblDRep oblgs)
    , adaPotsDepositsProposal  = coinToDbLovelace (Ledger.oblProposal oblgs)
    }
  where
    oblgs :: Ledger.Obligations
    oblgs = Shelley.obligationsPot pots

-- ---------------------------------------------------------------------------
-- * Protocol parameters and epoch state
-- ---------------------------------------------------------------------------

-- | Writes the @cost_model@ and @epoch_param@ rows. A Byron boundary
-- carries no protocol params, so it writes nothing.
writeBoundaryProtoParams
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => Generic.NewEpoch
  -> BlockId
  -> m ()
writeBoundaryProtoParams newEpoch blockId =
  case Generic.euProtoParams (Generic.neEpochUpdate newEpoch) of
    Strict.Nothing -> pure ()
    Strict.Just params -> do
      resolver <- asks getResolver
      writer   <- asks getWriter
      let epoch = unEpochNo (Generic.neEpoch newEpoch)
          nonce = Generic.euNonce (Generic.neEpochUpdate newEpoch)
      mCostModelId <- writeBoundaryCostModel resolver writer (Proto.ppCostmdls params)
      liftIO $ writeEpochParam writer (mkEpochParamRow params epoch blockId nonce mCostModelId)

-- | Write the @epoch_state@ row for a Conway-onward boundary. The
-- enacted committee \/ no-confidence \/ constitution ids come from the
-- governance boundary pass, which runs first and stashes them on the
-- resolver scratchpad. The caller gates on the ledger reporting Conway
-- governance state, so @epoch_state@ begins at the Conway bootstrap
-- epoch — the enacted snapshot it represents does not exist earlier.
writeBoundaryEpochState
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => Generic.NewEpoch
  -> m ()
writeBoundaryEpochState newEpoch = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let epoch = unEpochNo (Generic.neEpoch newEpoch)
  (mCommitteeId, mNoConfId, mConstId) <- liftIO $ readEnactedEpochStateIds resolver
  liftIO $ writeEpochState writer
    (mkEpochStateRow epoch mCommitteeId mNoConfId mConstId)

-- | Returns the row id, new or existing, when the protocol params
-- carry cost models. 'Nothing' for a pre-Alonzo era.
writeBoundaryCostModel
  :: MonadIO m
  => IdResolver IO
  -> Writer IO
  -> Maybe (Map Language Alonzo.CostModel)
  -> m (Maybe CostModelId)
writeBoundaryCostModel _ _ Nothing = pure Nothing
writeBoundaryCostModel resolver writer (Just cms) = do
  let row = mkCostModelRow cms
  (cmId, isNew) <- liftIO $ resolveCostModel resolver (costModelHash row) row
  when isNew $ liftIO $ writeCostModel writer cmId row
  pure (Just cmId)

-- ---------------------------------------------------------------------------
-- * Row builders
-- ---------------------------------------------------------------------------

mkEpochParamRow
  :: Proto.ProtoParams
  -> Word64
  -> BlockId
  -> Ledger.Nonce
  -> Maybe CostModelId
  -> EpochParam
mkEpochParamRow pp epoch blockId nonce mCostModelId =
  EpochParam
    { epochParamEpochNo                    = epoch
    , epochParamMinFeeA                    = fromIntegral (Proto.ppMinfeeA pp)
    , epochParamMinFeeB                    = fromIntegral (Proto.ppMinfeeB pp)
    , epochParamMaxBlockSize               = fromIntegral (Proto.ppMaxBBSize pp)
    , epochParamMaxTxSize                  = fromIntegral (Proto.ppMaxTxSize pp)
    , epochParamMaxBhSize                  = fromIntegral (Proto.ppMaxBHSize pp)
    , epochParamKeyDeposit                 = coinToDbLovelace (Proto.ppKeyDeposit pp)
    , epochParamPoolDeposit                = coinToDbLovelace (Proto.ppPoolDeposit pp)
    , epochParamMaxEpoch                   =
        fromIntegral (Ledger.unEpochInterval (Proto.ppMaxEpoch pp))
    , epochParamOptimalPoolCount           = fromIntegral (Proto.ppOptimalPoolCount pp)
    , epochParamInfluence                  = Proto.ppInfluence pp
    , epochParamMonetaryExpandRate         =
        unitIntervalToRational (Proto.ppMonetaryExpandRate pp)
    , epochParamTreasuryGrowthRate         =
        unitIntervalToRational (Proto.ppTreasuryGrowthRate pp)
    , epochParamDecentralisation           =
        unitIntervalToRational (Proto.ppDecentralisation pp)
    , epochParamProtocolMajor              =
        fromIntegral @Word64 @Word16
          (Ledger.getVersion (Ledger.pvMajor (Proto.ppProtocolVersion pp)))
    , epochParamProtocolMinor              =
        fromIntegral (Ledger.pvMinor (Proto.ppProtocolVersion pp))
    , epochParamMinUtxoValue               = coinToDbLovelace (Proto.ppMinUTxOValue pp)
    , epochParamMinPoolCost                = coinToDbLovelace (Proto.ppMinPoolCost pp)
    , epochParamNonce                      = nonceToBytes nonce
    , epochParamCostModelId                = mCostModelId
    , epochParamPriceMem                   = Proto.ppPriceMem pp
    , epochParamPriceStep                  = Proto.ppPriceStep pp
    , epochParamMaxTxExMem                 = DbWord64 <$> Proto.ppMaxTxExMem pp
    , epochParamMaxTxExSteps               = DbWord64 <$> Proto.ppMaxTxExSteps pp
    , epochParamMaxBlockExMem              = DbWord64 <$> Proto.ppMaxBlockExMem pp
    , epochParamMaxBlockExSteps            = DbWord64 <$> Proto.ppMaxBlockExSteps pp
    , epochParamMaxValSize                 =
        DbWord64 . fromIntegral <$> Proto.ppMaxValSize pp
    , epochParamCollateralPercent          =
        fromIntegral <$> Proto.ppCollateralPercentage pp
    , epochParamMaxCollateralInputs        =
        fromIntegral <$> Proto.ppMaxCollateralInputs pp
    , epochParamBlockId                    = blockId
    , epochParamExtraEntropy               = nonceToBytes (Proto.ppExtraEntropy pp)
    , epochParamCoinsPerUtxoSize           = coinToDbLovelace <$> Proto.ppCoinsPerUtxo pp
    , epochParamPvtMotionNoConfidence      =
        unitIntervalToRational . pvtMotionNoConfidence <$> Proto.ppPoolVotingThresholds pp
    , epochParamPvtCommitteeNormal         =
        unitIntervalToRational . pvtCommitteeNormal <$> Proto.ppPoolVotingThresholds pp
    , epochParamPvtCommitteeNoConfidence   =
        unitIntervalToRational . pvtCommitteeNoConfidence <$> Proto.ppPoolVotingThresholds pp
    , epochParamPvtHardForkInitiation      =
        unitIntervalToRational . pvtHardForkInitiation <$> Proto.ppPoolVotingThresholds pp
    , epochParamDvtMotionNoConfidence      =
        unitIntervalToRational . dvtMotionNoConfidence <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtCommitteeNormal         =
        unitIntervalToRational . dvtCommitteeNormal <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtCommitteeNoConfidence   =
        unitIntervalToRational . dvtCommitteeNoConfidence <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtUpdateToConstitution    =
        unitIntervalToRational . dvtUpdateToConstitution <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtHardForkInitiation      =
        unitIntervalToRational . dvtHardForkInitiation <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtPPNetworkGroup          =
        unitIntervalToRational . dvtPPNetworkGroup <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtPPEconomicGroup         =
        unitIntervalToRational . dvtPPEconomicGroup <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtPPTechnicalGroup        =
        unitIntervalToRational . dvtPPTechnicalGroup <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtPPGovGroup              =
        unitIntervalToRational . dvtPPGovGroup <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtTreasuryWithdrawal      =
        unitIntervalToRational . dvtTreasuryWithdrawal <$> Proto.ppDRepVotingThresholds pp
    , epochParamCommitteeMinSize           =
        DbWord64 . fromIntegral <$> Proto.ppCommitteeMinSize pp
    , epochParamCommitteeMaxTermLength     =
        DbWord64 . fromIntegral . Ledger.unEpochInterval <$> Proto.ppCommitteeMaxTermLength pp
    , epochParamGovActionLifetime          =
        DbWord64 . fromIntegral . Ledger.unEpochInterval <$> Proto.ppGovActionLifetime pp
    , epochParamGovActionDeposit           =
        DbWord64 . fromIntegral <$> Proto.ppGovActionDeposit pp
    , epochParamDrepDeposit                =
        DbWord64 . fromIntegral <$> Proto.ppDRepDeposit pp
    , epochParamDrepActivity               =
        DbWord64 . fromIntegral . Ledger.unEpochInterval <$> Proto.ppDRepActivity pp
    , epochParamPvtppSecurityGroup         =
        unitIntervalToRational . pvtPPSecurityGroup <$> Proto.ppPoolVotingThresholds pp
    , epochParamMinFeeRefScriptCostPerByte = Proto.ppMinFeeRefScriptCostPerByte pp
    }

-- | The three FK columns come from 'readEnactedEpochStateIds' on the
-- resolver, which the governance boundary handler refreshes from
-- @bndGovActionState@.
mkEpochStateRow
  :: Word64
  -> Maybe Int64     -- ^ committee.id
  -> Maybe Int64     -- ^ no_confidence_id (gov_action_proposal.id)
  -> Maybe Int64     -- ^ constitution.id
  -> EpochState
mkEpochStateRow epoch mCommitteeId mNoConfId mConstitutionId = EpochState
  { epochStateCommitteeId    = CommitteeId <$> mCommitteeId
  , epochStateNoConfidenceId = GovActionProposalId <$> mNoConfId
  , epochStateConstitutionId = ConstitutionId <$> mConstitutionId
  , epochStateEpochNo        = epoch
  }

-- | The @costs@ column takes the canonical JSON encoding. The @hash@
-- column takes the Blake2b_256 of the CBOR-serialised 'CostModels'
-- wrapper, which is also the dedup key.
mkCostModelRow :: Map Language Alonzo.CostModel -> CostModel
mkCostModelRow cms = CostModel
  { costModelCosts = Text.decodeUtf8 . LBS.toStrict $ Aeson.encode cms
  , costModelHash  = hashCostModels cms
  }

hashCostModels :: Map Language Alonzo.CostModel -> ByteString
hashCostModels =
  Crypto.abstractHashToBytes . Crypto.serializeCborHash . mkCostModels

