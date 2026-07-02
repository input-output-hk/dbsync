{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Conway-era governance extractor.
--
-- Owns 16 tables across three sub-groups:
--
--   * Cert-driven: @drep_hash@, @drep_registration@,
--     @delegation_vote@, @committee_hash@, @committee_registration@,
--     @committee_de_registration@.
--   * Proposal-driven: @voting_anchor@, @gov_action_proposal@,
--     @param_proposal@, @voting_procedure@, @treasury_withdrawal@,
--     @constitution@, @committee@, @committee_member@.
--   * Ledger-derived (boundary-triggered): @drep_distr@, @event_info@.
--
-- The @(tx_hash, proposal_index) -> gov_action_proposal.id@ cache
-- on 'ExtractState' carries proposal ids across blocks so the vote
-- pass and the boundary status-column updates can resolve their
-- @GovActionId@ references.
--
-- Empty cells:
--
--   * @event_info@ — never populated; FK target only.
--   * @voting_procedure.invalid@ — always 'Nothing' (FK target is
--     @event_info@).
--   * Cross-block vote references where the proposal predates the
--     cache's coverage window (i.e. the boot rebuild ran against a
--     pre-Conway PG) silently skip the row.
--   * Dijkstra parser arms emit empty lists; the extractor walks them
--     as no-ops.
module DbSync.Extractor.Governance
  ( governanceExtractor
  , runGovernanceBoundary
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Compactible as Ledger
import qualified Cardano.Ledger.Conway.Governance as Gov
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.DRep as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict
import Ouroboros.Consensus.Cardano.Block (ConwayEra)

import DbSync.Db.Schema.Governance
  ( Committee (..)
  , CommitteeDeRegistration (..)
  , CommitteeMember (..)
  , CommitteeRegistration (..)
  , Constitution (..)
  , DelegationVote (..)
  , DrepDistr (..)
  , DrepRegistration (..)
  , GovActionProposal (..)
  , ParamProposal (..)
  , TreasuryWithdrawal (..)
  , VotingProcedure (..)
  , committeeDeRegistrationTableDef
  , committeeHashTableDef
  , committeeMemberTableDef
  , committeeRegistrationTableDef
  , committeeTableDef
  , constitutionTableDef
  , delegationVoteTableDef
  , drepDistrTableDef
  , drepHashTableDef
  , drepRegistrationTableDef
  , eventInfoTableDef
  , govActionProposalTableDef
  , paramProposalTableDef
  , treasuryWithdrawalTableDef
  , votingAnchorTableDef
  , votingProcedureTableDef
  )
import DbSync.Db.Schema.Ids
  ( BlockId
  , CommitteeHashId
  , DrepHashId
  , GovActionProposalId
  , ParamProposalId
  , PoolHashId
  , TxId
  , VotingAnchorId
  , getGovActionProposalId
  )
import DbSync.Db.Types
  ( AnchorType (..)
  , DbLovelace (..)
  , DbWord64 (..)
  , GovActionType (..)
  , Vote
  , VoterRole (..)
  )
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , HasNetwork
  , ProcessBlockFn
  , TxContext (..)
  , blockGovExpiresAfter
  )
import DbSync.Worker.Ledger.EpochUpdate (NewEpoch (..))
import DbSync.Extractor.SharedDedup
  ( resolveAndWriteAbstractDrep
  , resolveAndWriteCommitteeHash
  , resolveAndWriteCostModel
  , resolveAndWriteDrepHash
  , resolveAndWritePoolHash
  , resolveAndWriteVotingAnchor
  , resolveStakeCred
  )
import qualified DbSync.Parser.Types as G
import DbSync.Parser.ParamProposal (GenericParamProposal (..))
import DbSync.Parser.Types
  ( AnchorData (..)
  , CertAction (..)
  , CredHash (..)
  , DRepIdent (..)
  , GenericGovAction (..)
  , GenericGovActionProposal (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericVoter (..)
  , GenericVotingProcedure (..)
  , GovActionRef (..)
  )
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.SyncState.Row
  ( HasControlConnection
  , markGovActionDropped
  , markGovActionEnacted
  , markGovActionExpired
  , markGovActionRatified
  , queryCommitteeByProposal
  , queryConstitutionByProposal
  )
import DbSync.Worker.Ledger.Event
  ( GovActionRefunded (..)
  , LedgerEvent (..)
  )
import DbSync.Worker.Ledger.Types (BoundaryApplyData (..))
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

governanceExtractor :: ExtractorDef
governanceExtractor = ExtractorDef
  { pdName    = "governance"
  , pdTables  =
      [ drepHashTableDef
      , drepRegistrationTableDef
      , drepDistrTableDef
      , delegationVoteTableDef
      , govActionProposalTableDef
      , votingProcedureTableDef
      , votingAnchorTableDef
      , constitutionTableDef
      , committeeTableDef
      , committeeHashTableDef
      , committeeMemberTableDef
      , committeeRegistrationTableDef
      , committeeDeRegistrationTableDef
      , paramProposalTableDef
      , treasuryWithdrawalTableDef
      , eventInfoTableDef
      ]
  , pdProcess = processGovernance
  }

-- ---------------------------------------------------------------------------
-- * Per-block processing
-- ---------------------------------------------------------------------------

processGovernance :: ProcessBlockFn
processGovernance ctx =
  forM_ (bcTxs ctx) $ \tc -> when (txValidContract (tcGenTx tc)) $ do
    processCerts ctx tc
    processProposals ctx tc
    processVotes ctx tc

-- ---------------------------------------------------------------------------
-- * Cert pass
-- ---------------------------------------------------------------------------

processCerts
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => BlockContext -> TxContext -> m ()
processCerts ctx tc =
  forM_ (txCertificates (tcGenTx tc)) $ \cert -> do
    let txId = tcTxId tc
        certIdx = txCertIndex cert
    case txCertAction cert of
      CertDRepRegistration credHash deposit mAnchor -> do
        drepId <- resolveAndWriteDrepHash (chHash credHash) (chIsScript credHash)
        mAnchorId <- resolveAnchor ctx DrepAnchor mAnchor
        writer <- asks getWriter
        liftIO $ writeDrepRegistration writer DrepRegistration
          { drepRegistrationTxId           = txId
          , drepRegistrationCertIndex      = certIdx
          , drepRegistrationDeposit        = Just (fromIntegral deposit)
          , drepRegistrationDrepHashId     = drepId
          , drepRegistrationVotingAnchorId = mAnchorId
          }

      CertDRepDeregistration credHash refund -> do
        drepId <- resolveAndWriteDrepHash (chHash credHash) (chIsScript credHash)
        writer <- asks getWriter
        liftIO $ writeDrepRegistration writer DrepRegistration
          { drepRegistrationTxId           = txId
          , drepRegistrationCertIndex      = certIdx
            -- Deregistration is recorded as a negative deposit
            -- against the original registration.
          , drepRegistrationDeposit        = Just (negate (fromIntegral refund))
          , drepRegistrationDrepHashId     = drepId
          , drepRegistrationVotingAnchorId = Nothing
          }

      CertDRepUpdate credHash mAnchor -> do
        drepId <- resolveAndWriteDrepHash (chHash credHash) (chIsScript credHash)
        mAnchorId <- resolveAnchor ctx DrepAnchor mAnchor
        writer <- asks getWriter
        liftIO $ writeDrepRegistration writer DrepRegistration
          { drepRegistrationTxId           = txId
          , drepRegistrationCertIndex      = certIdx
          , drepRegistrationDeposit        = Nothing
          , drepRegistrationDrepHashId     = drepId
          , drepRegistrationVotingAnchorId = mAnchorId
          }

      CertCommitteeAuth coldKey hotKey -> do
        coldId <- resolveAndWriteCommitteeHash (chHash coldKey) (chIsScript coldKey)
        hotId  <- resolveAndWriteCommitteeHash (chHash hotKey) (chIsScript hotKey)
        writer <- asks getWriter
        liftIO $ writeCommitteeRegistration writer CommitteeRegistration
          { committeeRegistrationTxId      = txId
          , committeeRegistrationCertIndex = certIdx
          , committeeRegistrationColdKeyId = coldId
          , committeeRegistrationHotKeyId  = hotId
          }

      CertCommitteeResign coldKey mAnchor -> do
        coldId    <- resolveAndWriteCommitteeHash (chHash coldKey) (chIsScript coldKey)
        mAnchorId <- resolveAnchor ctx CommitteeDeRegAnchor mAnchor
        writer <- asks getWriter
        liftIO $ writeCommitteeDeRegistration writer CommitteeDeRegistration
          { committeeDeRegistrationTxId           = txId
          , committeeDeRegistrationCertIndex      = certIdx
          , committeeDeRegistrationVotingAnchorId = mAnchorId
          , committeeDeRegistrationColdKeyId      = coldId
          }

      -- Combined vote-delegation certs.
      CertConwayDelegVote credHash drepIdent _mDeposit ->
        writeDelegationVoteRow tc certIdx credHash drepIdent
      CertConwayDelegStakeVote credHash _poolKeyHash drepIdent _mDeposit ->
        writeDelegationVoteRow tc certIdx credHash drepIdent

      _ -> pure ()

-- | Write a @delegation_vote@ row from a Conway delegation cert.
writeDelegationVoteRow
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => TxContext -> Word16 -> CredHash -> DRepIdent -> m ()
writeDelegationVoteRow tc certIdx cred drepIdent = do
  writer <- asks getWriter
  saId   <- resolveStakeCred cred
  drepId <- resolveDRep drepIdent
  liftIO $ writeDelegationVote writer DelegationVote
    { delegationVoteAddrId     = saId
    , delegationVoteCertIndex  = certIdx
    , delegationVoteDrepHashId = drepId
    , delegationVoteTxId       = tcTxId tc
    , delegationVoteRedeemerId = Nothing
    }

-- | Project a 'DRepIdent' into a @drep_hash@ id, materialising the
-- corresponding dedup row on first sighting.
resolveDRep
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => DRepIdent -> m DrepHashId
resolveDRep = \case
  DRepCred credHash      -> resolveAndWriteDrepHash (chHash credHash) (chIsScript credHash)
  DRepAlwaysAbstain      -> resolveAndWriteAbstractDrep "drep_always_abstain"
  DRepAlwaysNoConfidence -> resolveAndWriteAbstractDrep "drep_always_no_confidence"

-- ---------------------------------------------------------------------------
-- * Proposal pass
-- ---------------------------------------------------------------------------

processProposals
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => BlockContext -> TxContext -> m ()
processProposals ctx tc =
  forM_ (txProposals (tcGenTx tc)) $ \prop -> do
    resolver <- asks getResolver
    writer   <- asks getWriter
    let txId         = tcTxId tc
        proposingTx  = txHash (tcGenTx tc)
        anchor       = ggapAnchor prop
        action       = ggapAction prop
        currentEpoch = unEpochNo (G.blkEpochNo (bcGenBlock ctx))

    addrId    <- resolveStakeCred (ggapReturnAddrCred prop)
    anchorId  <- resolveAndWriteVotingAnchor (adUrl anchor) (adHash anchor)
                   GovActionAnchor (bcBlockId ctx)

    mParamProposalId <- case action of
      GovParameterChange _ gpp _ ->
        Just <$> writeParamProposalRow txId gpp
      _ -> pure Nothing

    mPrevId <- case prevRefFor action of
      Nothing  -> pure Nothing
      Just ref ->
        liftIO $ lookupGovActionProposalId resolver (garTxHash ref) (garIndex ref)

    -- Prefer this block's own gov-action lifetime; fall back to the
    -- boundary-stashed value for the first proposal of an epoch before
    -- any boundary has run.
    stashed <- liftIO $ readGovExpiresAfter resolver
    let mLifetime   = blockGovExpiresAfter (bcLedgerData ctx) <|> stashed
        mExpiration = (\lt -> currentEpoch + 1 + lt) <$> mLifetime

    gapId <- liftIO $ assignGovActionProposalId resolver
    liftIO $ writeGovActionProposal writer gapId GovActionProposal
      { govActionProposalTxId                  = txId
      , govActionProposalIndex                 = ggapTxIndex prop
      , govActionProposalPrevGovActionProposal = mPrevId
      , govActionProposalDeposit               = DbLovelace (ggapDeposit prop)
      , govActionProposalReturnAddress         = addrId
      , govActionProposalExpiration            = mExpiration
      , govActionProposalVotingAnchorId        = Just anchorId
      , govActionProposalType                  = govActionType action
      , govActionProposalDescription           = ggapDescriptionJson prop
      , govActionProposalParamProposal         = mParamProposalId
      , govActionProposalRatifiedEpoch         = Nothing
      , govActionProposalEnactedEpoch          = Nothing
      , govActionProposalDroppedEpoch          = Nothing
      , govActionProposalExpiredEpoch          = Nothing
      }
    liftIO $ recordGovActionProposalId resolver proposingTx (ggapTxIndex prop) gapId

    case action of
      GovTreasuryWithdraw recipients _ ->
        forM_ recipients $ \(rewardAcctCred, amount) -> do
          recipientId <- resolveStakeCred rewardAcctCred
          liftIO $ writeTreasuryWithdrawal writer TreasuryWithdrawal
            { treasuryWithdrawalGovActionProposalId = gapId
            , treasuryWithdrawalStakeAddressId      = recipientId
            , treasuryWithdrawalAmount              = DbLovelace amount
            }

      GovNewConstitution _ constAnchor scriptH -> do
        cAnchorId <- resolveAndWriteVotingAnchor
                       (adUrl constAnchor) (adHash constAnchor)
                       ConstitutionAnchor (bcBlockId ctx)
        cId <- liftIO $ assignConstitutionId resolver
        liftIO $ writeConstitution writer cId Constitution
          { constitutionGovActionProposalId = Just gapId
          , constitutionVotingAnchorId      = cAnchorId
          , constitutionScriptHash          = scriptH
          }

      GovUpdateCommittee _ _removed added qNum qDen -> do
        cId <- liftIO $ assignCommitteeId resolver
        liftIO $ writeCommittee writer cId Committee
          { committeeGovActionProposalId = Just gapId
          , committeeQuorumNumerator     = qNum
          , committeeQuorumDenominator   = qDen
          }
        forM_ added $ \(coldKey, expiry) -> do
          chId <- resolveAndWriteCommitteeHash coldKey False
          liftIO $ writeCommitteeMember writer CommitteeMember
            { committeeMemberCommitteeId     = cId
            , committeeMemberCommitteeHashId = chId
            , committeeMemberExpirationEpoch = expiry
            }

      _ -> pure ()

-- | Pick the previous-action reference embedded in a 'GenericGovAction'
-- arm, or 'Nothing' for arms that don't link an amendment chain.
prevRefFor :: GenericGovAction -> Maybe GovActionRef
prevRefFor = \case
  GovParameterChange p _ _    -> p
  GovHardForkInit p _ _       -> p
  GovNoConfidence p           -> p
  GovUpdateCommittee p _ _ _ _ -> p
  GovNewConstitution p _ _    -> p
  _                           -> Nothing

-- | The 'GovActionType' enum value for each action arm.
govActionType :: GenericGovAction -> GovActionType
govActionType = \case
  GovParameterChange  {}     -> ParameterChange
  GovHardForkInit     {}     -> HardForkInitiation
  GovTreasuryWithdraw {}     -> TreasuryWithdrawals
  GovNoConfidence     {}     -> NoConfidence
  GovUpdateCommittee  {}     -> NewCommitteeType
  GovNewConstitution  {}     -> NewConstitution
  GovInfoAction              -> InfoAction

-- | Materialise a @param_proposal@ row for a 'ParameterChange' arm,
-- writing the embedded cost model (if any) via dedup and returning
-- the row's id so 'gov_action_proposal.param_proposal' can reference
-- it.
writeParamProposalRow
  :: ( HasResolver env
     , HasWriter env
     , MonadReader env m
     , MonadIO m
     )
  => TxId
  -> GenericParamProposal
  -> m ParamProposalId
writeParamProposalRow txId gpp = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  cmId <- traverse resolveAndWriteCostModel (gppCostmdls gpp)
  ppId <- liftIO $ assignParamProposalId resolver
  liftIO $ writeParamProposal writer ppId ParamProposal
    { paramProposalEpochNo                    = gppEpochNo gpp
    , paramProposalKey                        = gppKey gpp
    , paramProposalMinFeeA                    = DbWord64 <$> gppMinFeeA gpp
    , paramProposalMinFeeB                    = DbWord64 <$> gppMinFeeB gpp
    , paramProposalMaxBlockSize               = DbWord64 <$> gppMaxBlockSize gpp
    , paramProposalMaxTxSize                  = DbWord64 <$> gppMaxTxSize gpp
    , paramProposalMaxBhSize                  = DbWord64 <$> gppMaxBhSize gpp
    , paramProposalKeyDeposit                 = DbLovelace <$> gppKeyDeposit gpp
    , paramProposalPoolDeposit                = DbLovelace <$> gppPoolDeposit gpp
    , paramProposalMaxEpoch                   = DbWord64 <$> gppMaxEpoch gpp
    , paramProposalOptimalPoolCount           = DbWord64 <$> gppOptimalPoolCount gpp
    , paramProposalInfluence                  = gppInfluence gpp
    , paramProposalMonetaryExpandRate         = gppMonetaryExpandRate gpp
    , paramProposalTreasuryGrowthRate         = gppTreasuryGrowthRate gpp
    , paramProposalDecentralisation           = gppDecentralisation gpp
    , paramProposalEntropy                    = gppEntropy gpp
    , paramProposalProtocolMajor              = gppProtocolMajor gpp
    , paramProposalProtocolMinor              = gppProtocolMinor gpp
    , paramProposalMinUtxoValue               = DbLovelace <$> gppMinUtxoValue gpp
    , paramProposalMinPoolCost                = DbLovelace <$> gppMinPoolCost gpp
    , paramProposalCostModelId                = cmId
    , paramProposalPriceMem                   = gppPriceMem gpp
    , paramProposalPriceStep                  = gppPriceStep gpp
    , paramProposalMaxTxExMem                 = DbWord64 <$> gppMaxTxExMem gpp
    , paramProposalMaxTxExSteps               = DbWord64 <$> gppMaxTxExSteps gpp
    , paramProposalMaxBlockExMem              = DbWord64 <$> gppMaxBlockExMem gpp
    , paramProposalMaxBlockExSteps            = DbWord64 <$> gppMaxBlockExSteps gpp
    , paramProposalMaxValSize                 = DbWord64 <$> gppMaxValSize gpp
    , paramProposalCollateralPercent          = gppCollateralPercent gpp
    , paramProposalMaxCollateralInputs        = gppMaxCollateralInputs gpp
    , paramProposalRegisteredTxId             = txId
    , paramProposalCoinsPerUtxoSize           = DbLovelace <$> gppCoinsPerUtxoSize gpp
    , paramProposalPvtMotionNoConfidence      = gppPvtMotionNoConfidence gpp
    , paramProposalPvtCommitteeNormal         = gppPvtCommitteeNormal gpp
    , paramProposalPvtCommitteeNoConfidence   = gppPvtCommitteeNoConfidence gpp
    , paramProposalPvtHardForkInitiation      = gppPvtHardForkInitiation gpp
    , paramProposalPvtppSecurityGroup         = gppPvtppSecurityGroup gpp
    , paramProposalDvtMotionNoConfidence      = gppDvtMotionNoConfidence gpp
    , paramProposalDvtCommitteeNormal         = gppDvtCommitteeNormal gpp
    , paramProposalDvtCommitteeNoConfidence   = gppDvtCommitteeNoConfidence gpp
    , paramProposalDvtUpdateToConstitution    = gppDvtUpdateToConstitution gpp
    , paramProposalDvtHardForkInitiation      = gppDvtHardForkInitiation gpp
    , paramProposalDvtPPNetworkGroup          = gppDvtPPNetworkGroup gpp
    , paramProposalDvtPPEconomicGroup         = gppDvtPPEconomicGroup gpp
    , paramProposalDvtPPTechnicalGroup        = gppDvtPPTechnicalGroup gpp
    , paramProposalDvtPPGovGroup              = gppDvtPPGovGroup gpp
    , paramProposalDvtTreasuryWithdrawal      = gppDvtTreasuryWithdrawal gpp
    , paramProposalCommitteeMinSize           = DbWord64 <$> gppCommitteeMinSize gpp
    , paramProposalCommitteeMaxTermLength     = DbWord64 <$> gppCommitteeMaxTermLength gpp
    , paramProposalGovActionLifetime          = DbWord64 <$> gppGovActionLifetime gpp
    , paramProposalGovActionDeposit           = DbWord64 <$> gppGovActionDeposit gpp
    , paramProposalDrepDeposit                = DbWord64 <$> gppDrepDeposit gpp
    , paramProposalDrepActivity               = DbWord64 <$> gppDrepActivity gpp
    , paramProposalMinFeeRefScriptCostPerByte = gppMinFeeRefScriptCostPerByte gpp
    }
  pure ppId

-- ---------------------------------------------------------------------------
-- * Vote pass
-- ---------------------------------------------------------------------------

processVotes
  :: ( HasResolver env
     , HasWriter env
     , MonadReader env m
     , MonadIO m
     )
  => BlockContext -> TxContext -> m ()
processVotes ctx tc =
  forM_ (txVotingProcedures (tcGenTx tc)) $ \vp -> do
    resolver <- asks getResolver
    let GovActionRef refTx refIdx = gvpGovActionId vp
    mGapId <- liftIO $ lookupGovActionProposalId resolver refTx refIdx
    case mGapId of
      Nothing    -> pure ()  -- proposal not in cache; skip the row
      Just gapId -> writeVoteRow ctx tc vp gapId

writeVoteRow
  :: ( HasResolver env
     , HasWriter env
     , MonadReader env m
     , MonadIO m
     )
  => BlockContext
  -> TxContext
  -> GenericVotingProcedure
  -> GovActionProposalId
  -> m ()
writeVoteRow ctx tc vp gapId = do
  writer <- asks getWriter
  (role, mDrep, mPool, mCommittee) <- resolveVoter (gvpVoter vp)
  mAnchorId <- case gvpAnchor vp of
    Nothing -> pure Nothing
    Just a  -> Just <$> resolveAndWriteVotingAnchor (adUrl a) (adHash a)
                            VoteAnchor (bcBlockId ctx)
  liftIO $ writeVotingProcedure writer VotingProcedure
    { votingProcedureTxId                = tcTxId tc
    , votingProcedureIndex               = gvpTxIndex vp
    , votingProcedureGovActionProposalId = gapId
    , votingProcedureVoterRole           = role
    , votingProcedureDrepVoter           = mDrep
    , votingProcedurePoolVoter           = mPool
    , votingProcedureVote                = gvpVote vp :: Vote
    , votingProcedureVotingAnchorId      = mAnchorId
    , votingProcedureCommitteeVoter      = mCommittee
    , votingProcedureInvalid             = Nothing
    }

-- | Resolve a 'GenericVoter' into the @voter_role@ enum plus the
-- three optional voter-id columns.
resolveVoter
  :: ( HasResolver env
     , HasWriter env
     , MonadReader env m
     , MonadIO m
     )
  => GenericVoter
  -> m (VoterRole, Maybe DrepHashId, Maybe PoolHashId, Maybe CommitteeHashId)
resolveVoter = \case
  VoterDRep ident -> do
    did <- resolveDRep ident
    pure (DRep, Just did, Nothing, Nothing)
  VoterStakePool poolHash -> do
    phId <- resolveAndWritePoolHash poolHash
    pure (SPO, Nothing, Just phId, Nothing)
  VoterCommittee credHash hasScript -> do
    chId <- resolveAndWriteCommitteeHash credHash hasScript
    pure (ConstitutionalCommittee, Nothing, Nothing, Just chId)

-- ---------------------------------------------------------------------------
-- * Anchor convenience
-- ---------------------------------------------------------------------------

-- | Resolve an optional anchor reference. 'Nothing' on input passes
-- through; 'Just' triggers the dedup write with the given type.
resolveAnchor
  :: ( HasResolver env
     , HasWriter env
     , MonadReader env m
     , MonadIO m
     )
  => BlockContext -> AnchorType -> Maybe AnchorData -> m (Maybe VotingAnchorId)
resolveAnchor _ _ Nothing = pure Nothing
resolveAnchor ctx anchorType (Just a) =
  Just <$> resolveAndWriteVotingAnchor (adUrl a) (adHash a) anchorType (bcBlockId ctx)

-- ---------------------------------------------------------------------------
-- * Boundary handler
-- ---------------------------------------------------------------------------

-- | Boundary entry point. Four jobs:
--
--   * Stash @apGovExpiresAfter@ on 'ExtractState' so the next block's
--     proposal pass can compute @gov_action_proposal.expiration@.
--   * Resolve the currently enacted committee / no-confidence /
--     constitution ids from @apGovActionState@ and stash them so
--     @epoch_state@'s gov FK columns are populated.
--   * Apply @LedgerGovInfo@ events to set
--     @gov_action_proposal.{enacted,dropped,expired}_epoch@, and use
--     the ratify state to set @ratified_epoch@.
--   * Emit one @drep_distr@ row per pulsing-snapshot entry.
runGovernanceBoundary
  :: ( HasResolver env
     , HasWriter env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => BoundaryApplyData
  -> BlockId
  -> m ()
runGovernanceBoundary applyResult _blockId = do
  resolver <- asks getResolver
  liftIO $ writeGovExpiresAfter resolver $
    case bndGovExpiresAfter applyResult of
      Strict.Just (Ledger.EpochInterval n) -> Just (fromIntegral n)
      Strict.Nothing                       -> Nothing
  refreshEnactedEpochStateIds applyResult
  case bndNewEpoch applyResult of
    Strict.Nothing -> pure ()
    Strict.Just newEpoch -> do
      let epoch = neEpoch newEpoch
      applyGovInfoEvents (bndEvents applyResult) epoch
      case neDRepState newEpoch of
        Strict.Nothing -> pure ()
        Strict.Just pulsingState ->
          emitDrepDistrAndRatify epoch pulsingState

-- | Walk @apEvents@ for the single 'LedgerGovInfo' entry (Conway+
-- emits at most one per boundary) and apply its enacted \/ dropped \/
-- expired action lists to @gov_action_proposal@.
applyGovInfoEvents
  :: ( HasResolver env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => [LedgerEvent] -> EpochNo -> m ()
applyGovInfoEvents events epoch =
  forM_ events $ \case
    LedgerGovInfo enacted dropped expired _uncl -> do
      let epochW = unEpochNo epoch
      forM_ enacted $ \gar ->
        resolveGovActionRowId (garGovActionId gar) >>= \case
          Just gid -> markGovActionEnacted gid epochW
          Nothing  -> pure ()
      forM_ (dropped <> expired) $ \gar ->
        resolveGovActionRowId (garGovActionId gar) >>= \case
          Just gid -> markGovActionDropped gid epochW
          Nothing  -> pure ()
    _ -> pure ()

-- | Look up our @gov_action_proposal.id@ for a ledger 'GovActionId'.
-- Misses on the in-process cache silently skip — the proposal predates
-- the cache's coverage window (boot rebuild missed it).
resolveGovActionRowId
  :: (HasResolver env, MonadReader env m, MonadIO m)
  => Gov.GovActionId -> m (Maybe Int64)
resolveGovActionRowId gaId = do
  resolver <- asks getResolver
  let (txHashBs, ix) = govActionIdParts gaId
  fmap getGovActionProposalId
    <$> liftIO (lookupGovActionProposalId resolver txHashBs ix)

govActionIdParts :: Gov.GovActionId -> (ByteString, Word64)
govActionIdParts (Gov.GovActionId (Ledger.TxId txid) (Gov.GovActionIx ix)) =
  (Crypto.hashToBytes (Ledger.extractHash txid), fromIntegral ix)

-- | Resolve current committee / no-confidence / constitution ids
-- from @apGovActionState@ and stash on 'ExtractState'. Looks the
-- gov-action ids up in the proposal cache, then SELECTs the
-- committee / constitution rows by their originating proposal id.
refreshEnactedEpochStateIds
  :: ( HasResolver env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => BoundaryApplyData -> m ()
refreshEnactedEpochStateIds applyResult = case bndGovActionState applyResult of
  Nothing  -> pure ()
  Just cgs -> do
    resolver <- asks getResolver
    let prevIds       = Gov.govStatePrevGovActionIds cgs
        mCommitteeGid = Ledger.strictMaybeToMaybe (Gov.grCommittee    prevIds)
        mConstGid     = Ledger.strictMaybeToMaybe (Gov.grConstitution prevIds)
        hasCommittee  = case Gov.cgsCommittee cgs of
          Ledger.SJust _  -> True
          Ledger.SNothing -> False

    mCommitteeProp <- traverse (govPurposeProposalId resolver) mCommitteeGid
    mConstProp     <- traverse (govPurposeProposalId resolver) mConstGid

    mCommitteeId <-
      if hasCommittee
        then queryCommitteeByProposal (join mCommitteeProp)
        else pure Nothing
    let mNoConfId = if hasCommittee then Nothing else join mCommitteeProp
    mConstitutionId <- queryConstitutionByProposal (join mConstProp)

    liftIO $ writeEnactedEpochStateIds resolver
      (mCommitteeId, mNoConfId, mConstitutionId)
  where
    govPurposeProposalId resolver (Gov.GovPurposeId gaId) = do
      let (txHashBs, ix) = govActionIdParts gaId
      mGid <- liftIO $ lookupGovActionProposalId resolver txHashBs ix
      pure $ fmap getGovActionProposalId mGid

emitDrepDistrAndRatify
  :: ( HasResolver env
     , HasWriter env
     , HasControlConnection env
     , MonadReader env m
     , MonadIO m
     )
  => EpochNo
  -> Gov.DRepPulsingState ConwayEra
  -> m ()
emitDrepDistrAndRatify epoch pulsingState = do
  writer <- asks getWriter
  let (snapshot, ratifyState) = Gov.finishDRepPulser pulsingState
      drepStates = Gov.psDRepState snapshot
      epochW     = unEpochNo epoch
  forM_ (Map.toList (Gov.psDRepDistr snapshot)) $ \(drep, coin) -> do
    drepId <- resolveDRepEntry drep
    let amount = fromIntegral (unCoin (Ledger.fromCompact coin))
        activeUntil = case drep of
          Ledger.DRepCredential cred ->
            unEpochNo . Ledger.drepExpiry <$> Map.lookup cred drepStates
          _ -> Nothing
    liftIO $ writeDrepDistr writer DrepDistr
      { drepDistrHashId      = drepId
      , drepDistrAmount      = amount
      , drepDistrEpochNo     = epochW
      , drepDistrActiveUntil = activeUntil
      }
  forM_ (toList (Gov.rsEnacted ratifyState)) $ \gas ->
    resolveGovActionRowId (Gov.gasId gas) >>= \case
      Just gid -> markGovActionRatified gid epochW
      Nothing  -> pure ()
  forM_ (toList (Gov.rsExpired ratifyState)) $ \gaId ->
    resolveGovActionRowId gaId >>= \case
      Just gid -> markGovActionExpired gid epochW
      Nothing  -> pure ()

-- | Project a ledger 'DRep' value to a @drep_hash@ row id.
resolveDRepEntry
  :: ( HasResolver env
     , HasWriter env
     , MonadReader env m
     , MonadIO m
     )
  => Ledger.DRep
  -> m DrepHashId
resolveDRepEntry = \case
  Ledger.DRepAlwaysAbstain      ->
    resolveAndWriteAbstractDrep "drep_always_abstain"
  Ledger.DRepAlwaysNoConfidence ->
    resolveAndWriteAbstractDrep "drep_always_no_confidence"
  Ledger.DRepCredential cred    ->
    resolveAndWriteDrepHash (credBytes cred) (credHasScript cred)
  where
    credBytes (Ledger.KeyHashObj (Ledger.KeyHash h))      = Crypto.hashToBytes h
    credBytes (Ledger.ScriptHashObj (Core.ScriptHash h))  = Crypto.hashToBytes h

    credHasScript Ledger.KeyHashObj    {} = False
    credHasScript Ledger.ScriptHashObj {} = True


