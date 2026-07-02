{-# LANGUAGE OverloadedStrings #-}

-- | Epoch-boundary projection.
--
-- Owns the boundary-triggered tables: @ada_pots@, @epoch_param@,
-- @epoch_state@, @cost_model@. The per-block 'pdProcess' callback
-- is a no-op; 'runEpochBoundary' is invoked by the consumer when
-- it detects an epoch transition and the LedgerWorker has produced
-- the matching 'ApplyResult'.
--
-- When the ledger feature is off the consumer never calls
-- 'runEpochBoundary' and these tables stay empty. The schemas are
-- created unconditionally so operators can flip the ledger flag
-- without re-syncing.
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
import DbSync.Util (coinToDbLovelace, nonceToBytes, unitIntervalToDouble)
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor registration
-- ---------------------------------------------------------------------------

-- | The EpochBoundary extractor.
--
-- Registers the four boundary-triggered tables. The per-block
-- 'pdProcess' is a no-op; 'runEpochBoundary' is the entry point
-- the consumer calls when an epoch crosses.
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

-- | Run the epoch-boundary writes for a single transition.
--
-- Dispatches to four per-table writers when 'apNewEpoch' is
-- populated:
--
--   * @ada_pots@ — when the ledger reported pots data for the
--     boundary (always from Shelley onward, never from Byron).
--   * @cost_model@ — when the protocol parameters carry Plutus
--     cost models (Alonzo onward); deduped by Blake2b hash of the
--     canonical CBOR encoding.
--   * @epoch_param@ — every Shelley-onward boundary.
--   * @epoch_state@ — every Shelley-onward boundary. The three
--     governance FK columns are written 'Nothing' until the
--     governance writers produce committee \/ no-confidence \/
--     constitution IDs.
--
-- Idempotency is the caller's responsibility — invoking this twice
-- for the same boundary writes duplicate rows.
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

-- ---------------------------------------------------------------------------
-- * AdaPots
-- ---------------------------------------------------------------------------

-- | Build and dispatch the 'AdaPots' row for the boundary, if the
-- ledger reported any pots data.
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

-- | Build an 'AdaPots' record from the boundary's
-- 'Shelley.AdaPots' value.
--
-- The deposit pots (stake, drep, proposal) come from
-- 'Shelley.obligationsPot' — they are not direct fields on
-- 'Shelley.AdaPots' itself.
--
-- @utxo@ is taken /verbatim/ from the supplied pots — the caller
-- (the LedgerWorker via 'DbSync.Worker.Ledger.State.applyBlock') has
-- already applied the @fixUTxOPots@ correction so that the sum of
-- pots equals @maxLovelaceSupply@.
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
-- * Protocol parameters (cost_model, epoch_param, epoch_state)
-- ---------------------------------------------------------------------------

-- | Build and dispatch the cost_model, epoch_param, and epoch_state
-- rows for the boundary. No-op if the ledger emitted no protocol
-- params (Byron boundaries).
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
      (mCommitteeId, mNoConfId, mConstId) <- liftIO $ readEnactedEpochStateIds resolver
      liftIO $ writeEpochParam writer (mkEpochParamRow params epoch blockId nonce mCostModelId)
      liftIO $ writeEpochState writer
        (mkEpochStateRow epoch mCommitteeId mNoConfId mConstId)

-- | Dedup-write a cost_model row. Returns the (possibly already
-- existing) row id when cost models are present in the protocol
-- params, 'Nothing' for pre-Alonzo eras.
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

-- | Map a 'ProtoParams' snapshot to the 53-column 'EpochParam' row.
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
    , epochParamInfluence                  = fromRational (Proto.ppInfluence pp)
    , epochParamMonetaryExpandRate         =
        unitIntervalToDouble (Proto.ppMonetaryExpandRate pp)
    , epochParamTreasuryGrowthRate         =
        unitIntervalToDouble (Proto.ppTreasuryGrowthRate pp)
    , epochParamDecentralisation           =
        unitIntervalToDouble (Proto.ppDecentralisation pp)
    , epochParamProtocolMajor              =
        fromIntegral @Word64 @Word16
          (Ledger.getVersion (Ledger.pvMajor (Proto.ppProtocolVersion pp)))
    , epochParamProtocolMinor              =
        fromIntegral (Ledger.pvMinor (Proto.ppProtocolVersion pp))
    , epochParamMinUtxoValue               = coinToDbLovelace (Proto.ppMinUTxOValue pp)
    , epochParamMinPoolCost                = coinToDbLovelace (Proto.ppMinPoolCost pp)
    , epochParamNonce                      = nonceToBytes nonce
    , epochParamCostModelId                = mCostModelId
    , epochParamPriceMem                   = fmap fromRational (Proto.ppPriceMem pp)
    , epochParamPriceStep                  = fmap fromRational (Proto.ppPriceStep pp)
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
        unitIntervalToDouble . pvtMotionNoConfidence <$> Proto.ppPoolVotingThresholds pp
    , epochParamPvtCommitteeNormal         =
        unitIntervalToDouble . pvtCommitteeNormal <$> Proto.ppPoolVotingThresholds pp
    , epochParamPvtCommitteeNoConfidence   =
        unitIntervalToDouble . pvtCommitteeNoConfidence <$> Proto.ppPoolVotingThresholds pp
    , epochParamPvtHardForkInitiation      =
        unitIntervalToDouble . pvtHardForkInitiation <$> Proto.ppPoolVotingThresholds pp
    , epochParamDvtMotionNoConfidence      =
        unitIntervalToDouble . dvtMotionNoConfidence <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtCommitteeNormal         =
        unitIntervalToDouble . dvtCommitteeNormal <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtCommitteeNoConfidence   =
        unitIntervalToDouble . dvtCommitteeNoConfidence <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtUpdateToConstitution    =
        unitIntervalToDouble . dvtUpdateToConstitution <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtHardForkInitiation      =
        unitIntervalToDouble . dvtHardForkInitiation <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtPPNetworkGroup          =
        unitIntervalToDouble . dvtPPNetworkGroup <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtPPEconomicGroup         =
        unitIntervalToDouble . dvtPPEconomicGroup <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtPPTechnicalGroup        =
        unitIntervalToDouble . dvtPPTechnicalGroup <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtPPGovGroup              =
        unitIntervalToDouble . dvtPPGovGroup <$> Proto.ppDRepVotingThresholds pp
    , epochParamDvtTreasuryWithdrawal      =
        unitIntervalToDouble . dvtTreasuryWithdrawal <$> Proto.ppDRepVotingThresholds pp
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
        unitIntervalToDouble . pvtPPSecurityGroup <$> Proto.ppPoolVotingThresholds pp
    , epochParamMinFeeRefScriptCostPerByte =
        fmap fromRational (Proto.ppMinFeeRefScriptCostPerByte pp)
    }

-- | Build the 'EpochState' row. The three FK columns come from
-- 'readEnactedEpochStateIds' on the resolver, which the governance
-- boundary handler refreshes from @apGovActionState@.
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

-- | Build the 'CostModel' row from a Plutus cost-model map. The
-- @costs@ column is the canonical JSON encoding; @hash@ is the
-- Blake2b_256 of the CBOR-serialised 'CostModels' wrapper — same
-- bytes as the dedup key.
mkCostModelRow :: Map Language Alonzo.CostModel -> CostModel
mkCostModelRow cms = CostModel
  { costModelCosts = Text.decodeUtf8 . LBS.toStrict $ Aeson.encode cms
  , costModelHash  = hashCostModels cms
  }

-- | Blake2b_256 of the CBOR-encoded 'CostModels' newtype.
hashCostModels :: Map Language Alonzo.CostModel -> ByteString
hashCostModels =
  Crypto.abstractHashToBytes . Crypto.serializeCborHash . mkCostModels

