{-# LANGUAGE OverloadedStrings #-}

-- | One shape check per @encode*Copy@ encoder: the emitted row must
-- line up field-for-field with the production 'copyableColumnList'.
-- A completeness guard forces every declared table to either appear
-- here or be excluded with a reason, so new tables cannot skip the
-- decision.
--
-- Samples set every 'Maybe' field to 'Nothing' so each nullable
-- position emits @\\N@ — a swapped or omitted encoder field then
-- shifts a @\\N@ into a non-nullable slot and fails the positional
-- check.
module DbSync.Schema.CopyShapeSpec (spec) where

import Cardano.Prelude

import Data.List ((\\))
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified DbSync.Db.Schema.AdaPots as AdaPots
import qualified DbSync.Db.Schema.Address as Address
import qualified DbSync.Db.Schema.CBOR as CBOR
import qualified DbSync.Db.Schema.Core as Core
import qualified DbSync.Db.Schema.EpochBoundary as EB
import qualified DbSync.Db.Schema.EpochSyncStats as ESS
import qualified DbSync.Db.Schema.EpochView as EV
import qualified DbSync.Db.Schema.Governance as Gov
import qualified DbSync.Db.Schema.Metadata as Metadata
import qualified DbSync.Db.Schema.MultiAsset as MA
import qualified DbSync.Db.Schema.OffChainPool as OCP
import qualified DbSync.Db.Schema.OffChainVote as OCV
import qualified DbSync.Db.Schema.Pool as Pool
import qualified DbSync.Db.Schema.ScriptsDatums as SD
import qualified DbSync.Db.Schema.StakeDelegation as SDel
import qualified DbSync.Db.Schema.UTxO as UTxO
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Types
  ( AnchorType (..)
  , DbLovelace (..)
  , DbWord64 (..)
  , RewardSource (..)
  , ScriptPurpose (..)
  , ScriptType (..)
  , SyncState (..)
  , Vote (..)
  , VoteUrl (..)
  , VoterRole (..)
  , toDbInt65
  )
import DbSync.Extractor.Registry (allDeclaredTables)
import DbSync.Test.Copy (shouldMatchCopyShape)

spec :: Spec
spec = do
  describe "COPY row shape matches copyable columns" $
    forM_ copyShapeCases $ \(td, row) ->
      it (T.unpack (tdName td)) $ shouldMatchCopyShape td row

  describe "COPY shape registry completeness" $ do
    it "covers every declared table except the documented non-COPY tables" $ do
      let declared = map tdName allDeclaredTables
          covered  = map (tdName . fst) copyShapeCases
      List.sort (declared \\ covered) `shouldBe` List.sort tablesWithoutCopyEncoder

    it "contains no undocumented table outside the declared schema" $ do
      let declared = map tdName allDeclaredTables
          covered  = map (tdName . fst) copyShapeCases
      List.sort (covered \\ declared)
        `shouldBe` List.sort encodersOutsideDeclaredSchema

-- | Declared tables with no @encode*Copy@; each stays out of the
-- COPY loader path for the stated reason.
tablesWithoutCopyEncoder :: [Text]
tablesWithoutCopyEncoder =
  [ "dbsync_sync_state"   -- single-row, INSERT/UPDATE via control connection
  , "epoch_param_pending" -- INSERT-managed staging table
  , "gov_action_proposal" -- Follow-only writes via INSERT statements
  ]

-- | Tables with a COPY encoder but no place in 'allDeclaredTables'.
-- Their encoders are still shape-checked above; this list keeps the
-- discrepancy visible until they are either declared or deleted.
encodersOutsideDeclaredSchema :: [Text]
encodersOutsideDeclaredSchema =
  [ "epoch_sync_time"
  ]

-- ---------------------------------------------------------------------------
-- * Registry
-- ---------------------------------------------------------------------------

copyShapeCases :: [(TableDef, ByteString)]
copyShapeCases =
  [ -- Core
    (Core.blockTableDef,        Core.encodeBlockCopy (BlockId 1) sampleBlock)
  , (Core.txTableDef,           Core.encodeTxCopy (TxId 1) sampleTx)
  , (Core.slotLeaderTableDef,   Core.encodeSlotLeaderCopy (SlotLeaderId 1) sampleSlotLeader)
  , (Core.stakeAddressTableDef, Core.encodeStakeAddressCopy (StakeAddressId 1) sampleStakeAddress)
  , (Core.poolHashTableDef,     Core.encodePoolHashCopy (PoolHashId 1) samplePoolHash)

    -- Address
  , (Address.addressTableDef, Address.encodeAddressCopy (AddressId 1) sampleAddress)

    -- CBOR
  , (CBOR.txCborTableDef, CBOR.encodeTxCborCopy sampleTxCbor)

    -- AdaPots
  , (AdaPots.adaPotsTableDef, AdaPots.encodeAdaPotsCopy sampleAdaPots)

    -- EpochBoundary
  , (EB.epochParamTableDef,  EB.encodeEpochParamCopy sampleEpochParam)
  , (EB.epochStateTableDef,  EB.encodeEpochStateCopy sampleEpochState)
  , (EB.costModelTableDef,   EB.encodeCostModelCopy (CostModelId 1) sampleCostModel)
  , (EB.potTransferTableDef, EB.encodePotTransferCopy samplePotTransfer)
  , (EB.treasuryTableDef,    EB.encodeTreasuryCopy sampleTreasury)
  , (EB.reserveTableDef,     EB.encodeReserveCopy sampleReserve)

    -- EpochSyncStats
  , (ESS.epochSyncStatsTableDef, ESS.encodeEpochSyncStatsCopy (EpochSyncStatsId 1) sampleEpochSyncStats)
  , (ESS.epochSyncTimeTableDef,  ESS.encodeEpochSyncTimeCopy (EpochSyncTimeId 1) sampleEpochSyncTime)

    -- EpochView
  , (EV.epochFinalizedTableDef, EV.encodeEpochFinalizedCopy (EpochId 1) sampleEpochFinalized)

    -- Governance
  , (Gov.drepHashTableDef,                Gov.encodeDrepHashCopy (DrepHashId 1) sampleDrepHash)
  , (Gov.drepRegistrationTableDef,        Gov.encodeDrepRegistrationCopy sampleDrepRegistration)
  , (Gov.drepDistrTableDef,               Gov.encodeDrepDistrCopy sampleDrepDistr)
  , (Gov.delegationVoteTableDef,          Gov.encodeDelegationVoteCopy sampleDelegationVote)
  , (Gov.votingProcedureTableDef,         Gov.encodeVotingProcedureCopy sampleVotingProcedure)
  , (Gov.votingAnchorTableDef,            Gov.encodeVotingAnchorCopy (VotingAnchorId 1) sampleVotingAnchor)
  , (Gov.constitutionTableDef,            Gov.encodeConstitutionCopy (ConstitutionId 1) sampleConstitution)
  , (Gov.committeeTableDef,               Gov.encodeCommitteeCopy (CommitteeId 1) sampleCommittee)
  , (Gov.committeeHashTableDef,           Gov.encodeCommitteeHashCopy (CommitteeHashId 1) sampleCommitteeHash)
  , (Gov.committeeMemberTableDef,         Gov.encodeCommitteeMemberCopy sampleCommitteeMember)
  , (Gov.committeeRegistrationTableDef,   Gov.encodeCommitteeRegistrationCopy sampleCommitteeRegistration)
  , (Gov.committeeDeRegistrationTableDef, Gov.encodeCommitteeDeRegistrationCopy sampleCommitteeDeRegistration)
  , (Gov.paramProposalTableDef,           Gov.encodeParamProposalCopy (ParamProposalId 1) sampleParamProposal)
  , (Gov.treasuryWithdrawalTableDef,      Gov.encodeTreasuryWithdrawalCopy sampleTreasuryWithdrawal)
  , (Gov.eventInfoTableDef,               Gov.encodeEventInfoCopy (EventInfoId 1) sampleEventInfo)

    -- Metadata
  , (Metadata.txMetadataTableDef, Metadata.encodeTxMetadataCopy sampleTxMetadata)

    -- MultiAsset
  , (MA.multiAssetTableDef, MA.encodeMultiAssetCopy (MultiAssetId 1) sampleMultiAsset)
  , (MA.maTxMintTableDef,   MA.encodeMaTxMintCopy sampleMaTxMint)
  , (MA.maTxOutTableDef,    MA.encodeMaTxOutCopy sampleMaTxOut)

    -- OffChainPool
  , (OCP.offChainPoolDataTableDef,       OCP.encodeOffChainPoolDataCopy sampleOffChainPoolData)
  , (OCP.offChainPoolFetchErrorTableDef, OCP.encodeOffChainPoolFetchErrorCopy sampleOffChainPoolFetchError)

    -- OffChainVote
  , (OCV.offChainVoteDataTableDef,           OCV.encodeOffChainVoteDataCopy sampleOffChainVoteData)
  , (OCV.offChainVoteGovActionDataTableDef,  OCV.encodeOffChainVoteGovActionDataCopy sampleOffChainVoteGovActionData)
  , (OCV.offChainVoteDrepDataTableDef,       OCV.encodeOffChainVoteDrepDataCopy sampleOffChainVoteDrepData)
  , (OCV.offChainVoteAuthorTableDef,         OCV.encodeOffChainVoteAuthorCopy sampleOffChainVoteAuthor)
  , (OCV.offChainVoteReferenceTableDef,      OCV.encodeOffChainVoteReferenceCopy sampleOffChainVoteReference)
  , (OCV.offChainVoteExternalUpdateTableDef, OCV.encodeOffChainVoteExternalUpdateCopy sampleOffChainVoteExternalUpdate)
  , (OCV.offChainVoteFetchErrorTableDef,     OCV.encodeOffChainVoteFetchErrorCopy sampleOffChainVoteFetchError)

    -- Pool
  , (Pool.poolUpdateTableDef,         Pool.encodePoolUpdateCopy (PoolUpdateId 1) samplePoolUpdate)
  , (Pool.poolMetadataRefTableDef,    Pool.encodePoolMetadataRefCopy (PoolMetadataRefId 1) samplePoolMetadataRef)
  , (Pool.poolOwnerTableDef,          Pool.encodePoolOwnerCopy samplePoolOwner)
  , (Pool.poolRetireTableDef,         Pool.encodePoolRetireCopy samplePoolRetire)
  , (Pool.poolRelayTableDef,          Pool.encodePoolRelayCopy samplePoolRelay)
  , (Pool.poolStatTableDef,           Pool.encodePoolStatCopy samplePoolStat)
  , (Pool.delistedPoolTableDef,       Pool.encodeDelistedPoolCopy sampleDelistedPool)
  , (Pool.reservedPoolTickerTableDef, Pool.encodeReservedPoolTickerCopy sampleReservedPoolTicker)

    -- ScriptsDatums
  , (SD.datumTableDef,           SD.encodeDatumCopy (DatumId 1) sampleDatum)
  , (SD.scriptTableDef,          SD.encodeScriptCopy (ScriptId 1) sampleScript)
  , (SD.redeemerTableDef,        SD.encodeRedeemerCopy (RedeemerId 1) sampleRedeemer)
  , (SD.redeemerDataTableDef,    SD.encodeRedeemerDataCopy (RedeemerDataId 1) sampleRedeemerData)
  , (SD.extraKeyWitnessTableDef, SD.encodeExtraKeyWitnessCopy sampleExtraKeyWitness)

    -- StakeDelegation
  , (SDel.stakeRegistrationTableDef,   SDel.encodeStakeRegistrationCopy sampleStakeRegistration)
  , (SDel.stakeDeregistrationTableDef, SDel.encodeStakeDeregistrationCopy sampleStakeDeregistration)
  , (SDel.delegationTableDef,          SDel.encodeDelegationCopy sampleDelegation)
  , (SDel.withdrawalTableDef,          SDel.encodeWithdrawalCopy sampleWithdrawal)
  , (SDel.rewardTableDef,              SDel.encodeRewardCopy sampleReward)
  , (SDel.potRewardTableDef,           SDel.encodePotRewardCopy samplePotReward)
  , (SDel.epochStakeTableDef,          SDel.encodeEpochStakeCopy sampleEpochStake)
  , (SDel.epochStakeProgressTableDef,  SDel.encodeEpochStakeProgressCopy sampleEpochStakeProgress)

    -- UTxO
  , (UTxO.txOutTableDef,           UTxO.encodeTxOutCopy (TxOutId 1) sampleTxOut)
  , (UTxO.txInTableDef,            UTxO.encodeTxInCopy sampleTxIn)
  , (UTxO.collateralTxInTableDef,  UTxO.encodeCollateralTxInCopy sampleCollateralTxIn)
  , (UTxO.referenceTxInTableDef,   UTxO.encodeReferenceTxInCopy sampleReferenceTxIn)
  , (UTxO.collateralTxOutTableDef, UTxO.encodeCollateralTxOutCopy (CollateralTxOutId 1) sampleCollateralTxOut)
  ]

-- ---------------------------------------------------------------------------
-- * Samples
-- ---------------------------------------------------------------------------

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

sampleBlock :: Core.Block
sampleBlock = Core.Block
  { Core.blockHash          = "\1"
  , Core.blockEpochNo       = Nothing
  , Core.blockSlotNo        = Nothing
  , Core.blockEpochSlotNo   = Nothing
  , Core.blockBlockNo       = Nothing
  , Core.blockPreviousId    = Nothing
  , Core.blockSlotLeaderId  = SlotLeaderId 1
  , Core.blockSize          = 1
  , Core.blockTime          = t0
  , Core.blockTxCount       = 0
  , Core.blockProtoMajor    = 9
  , Core.blockProtoMinor    = 0
  , Core.blockVrfKey        = Nothing
  , Core.blockOpCert        = Nothing
  , Core.blockOpCertCounter = Nothing
  }

sampleTx :: Core.Tx
sampleTx = Core.Tx
  { Core.txHash             = "\2"
  , Core.txBlockId          = BlockId 1
  , Core.txBlockIndex       = 0
  , Core.txOutSum           = DbLovelace 1
  , Core.txFee              = DbLovelace 1
  , Core.txDeposit          = Nothing
  , Core.txSize             = 1
  , Core.txInvalidBefore    = Nothing
  , Core.txInvalidHereafter = Nothing
  , Core.txValidContract    = True
  , Core.txScriptSize       = 0
  , Core.txTreasuryDonation = DbLovelace 0
  }

sampleSlotLeader :: Core.SlotLeader
sampleSlotLeader = Core.SlotLeader
  { Core.slotLeaderHash        = "\3"
  , Core.slotLeaderPoolHashId  = Nothing
  , Core.slotLeaderDescription = "sl"
  }

sampleStakeAddress :: Core.StakeAddress
sampleStakeAddress = Core.StakeAddress
  { Core.stakeAddressHashRaw    = "\4"
  , Core.stakeAddressView       = "stake1"
  , Core.stakeAddressScriptHash = Nothing
  }

samplePoolHash :: Core.PoolHash
samplePoolHash = Core.PoolHash
  { Core.poolHashHashRaw = "\5"
  , Core.poolHashView    = "pool1"
  }

sampleAddress :: Address.Address
sampleAddress = Address.Address
  { Address.addressAddress        = "addr1"
  , Address.addressRaw            = "\6"
  , Address.addressHasScript      = False
  , Address.addressPaymentCred    = Nothing
  , Address.addressStakeAddressId = Nothing
  }

sampleTxCbor :: CBOR.TxCbor
sampleTxCbor = CBOR.TxCbor
  { CBOR.txCborTxId  = TxId 1
  , CBOR.txCborBytes = "\7"
  }

sampleAdaPots :: AdaPots.AdaPots
sampleAdaPots = AdaPots.AdaPots
  { AdaPots.adaPotsSlotNo           = 1
  , AdaPots.adaPotsEpochNo          = 1
  , AdaPots.adaPotsTreasury         = DbLovelace 1
  , AdaPots.adaPotsReserves         = DbLovelace 1
  , AdaPots.adaPotsRewards          = DbLovelace 1
  , AdaPots.adaPotsUtxo             = DbLovelace 1
  , AdaPots.adaPotsDepositsStake    = DbLovelace 1
  , AdaPots.adaPotsFees             = DbLovelace 1
  , AdaPots.adaPotsBlockId          = BlockId 1
  , AdaPots.adaPotsDepositsDrep     = DbLovelace 0
  , AdaPots.adaPotsDepositsProposal = DbLovelace 0
  }

sampleEpochParam :: EB.EpochParam
sampleEpochParam = EB.EpochParam
  { EB.epochParamEpochNo                    = 1
  , EB.epochParamMinFeeA                    = 1
  , EB.epochParamMinFeeB                    = 1
  , EB.epochParamMaxBlockSize               = 1
  , EB.epochParamMaxTxSize                  = 1
  , EB.epochParamMaxBhSize                  = 1
  , EB.epochParamKeyDeposit                 = DbLovelace 1
  , EB.epochParamPoolDeposit                = DbLovelace 1
  , EB.epochParamMaxEpoch                   = 1
  , EB.epochParamOptimalPoolCount           = 1
  , EB.epochParamInfluence                  = 0.1
  , EB.epochParamMonetaryExpandRate         = 0.1
  , EB.epochParamTreasuryGrowthRate         = 0.1
  , EB.epochParamDecentralisation           = 0.1
  , EB.epochParamProtocolMajor              = 9
  , EB.epochParamProtocolMinor              = 0
  , EB.epochParamMinUtxoValue               = DbLovelace 1
  , EB.epochParamMinPoolCost                = DbLovelace 1
  , EB.epochParamNonce                      = Nothing
  , EB.epochParamCostModelId                = Nothing
  , EB.epochParamPriceMem                   = Nothing
  , EB.epochParamPriceStep                  = Nothing
  , EB.epochParamMaxTxExMem                 = Nothing
  , EB.epochParamMaxTxExSteps               = Nothing
  , EB.epochParamMaxBlockExMem              = Nothing
  , EB.epochParamMaxBlockExSteps            = Nothing
  , EB.epochParamMaxValSize                 = Nothing
  , EB.epochParamCollateralPercent          = Nothing
  , EB.epochParamMaxCollateralInputs        = Nothing
  , EB.epochParamBlockId                    = BlockId 1
  , EB.epochParamExtraEntropy               = Nothing
  , EB.epochParamCoinsPerUtxoSize           = Nothing
  , EB.epochParamPvtMotionNoConfidence      = Nothing
  , EB.epochParamPvtCommitteeNormal         = Nothing
  , EB.epochParamPvtCommitteeNoConfidence   = Nothing
  , EB.epochParamPvtHardForkInitiation      = Nothing
  , EB.epochParamDvtMotionNoConfidence      = Nothing
  , EB.epochParamDvtCommitteeNormal         = Nothing
  , EB.epochParamDvtCommitteeNoConfidence   = Nothing
  , EB.epochParamDvtUpdateToConstitution    = Nothing
  , EB.epochParamDvtHardForkInitiation      = Nothing
  , EB.epochParamDvtPPNetworkGroup          = Nothing
  , EB.epochParamDvtPPEconomicGroup         = Nothing
  , EB.epochParamDvtPPTechnicalGroup        = Nothing
  , EB.epochParamDvtPPGovGroup              = Nothing
  , EB.epochParamDvtTreasuryWithdrawal      = Nothing
  , EB.epochParamCommitteeMinSize           = Nothing
  , EB.epochParamCommitteeMaxTermLength     = Nothing
  , EB.epochParamGovActionLifetime          = Nothing
  , EB.epochParamGovActionDeposit           = Nothing
  , EB.epochParamDrepDeposit                = Nothing
  , EB.epochParamDrepActivity               = Nothing
  , EB.epochParamPvtppSecurityGroup         = Nothing
  , EB.epochParamMinFeeRefScriptCostPerByte = Nothing
  }

sampleEpochState :: EB.EpochState
sampleEpochState = EB.EpochState
  { EB.epochStateCommitteeId    = Nothing
  , EB.epochStateNoConfidenceId = Nothing
  , EB.epochStateConstitutionId = Nothing
  , EB.epochStateEpochNo        = 1
  }

sampleCostModel :: EB.CostModel
sampleCostModel = EB.CostModel
  { EB.costModelCosts = "{}"
  , EB.costModelHash  = "\8"
  }

samplePotTransfer :: EB.PotTransfer
samplePotTransfer = EB.PotTransfer
  { EB.potTransferCertIndex = 0
  , EB.potTransferTreasury  = toDbInt65 1
  , EB.potTransferReserves  = toDbInt65 (-1)
  , EB.potTransferTxId      = TxId 1
  }

sampleTreasury :: EB.Treasury
sampleTreasury = EB.Treasury
  { EB.treasuryAddrId    = StakeAddressId 1
  , EB.treasuryCertIndex = 0
  , EB.treasuryAmount    = toDbInt65 1
  , EB.treasuryTxId      = TxId 1
  }

sampleReserve :: EB.Reserve
sampleReserve = EB.Reserve
  { EB.reserveAddrId    = StakeAddressId 1
  , EB.reserveCertIndex = 0
  , EB.reserveAmount    = toDbInt65 1
  , EB.reserveTxId      = TxId 1
  }

sampleEpochSyncStats :: ESS.EpochSyncStats
sampleEpochSyncStats = ESS.EpochSyncStats
  { ESS.epochSyncStatsEpochNo         = 1
  , ESS.epochSyncStatsBlocksProcessed = 1
  , ESS.epochSyncStatsBlocksPerSec    = 1.0
  , ESS.epochSyncStatsElapsedSec      = 1.0
  , ESS.epochSyncStatsSyncedAt        = t0
  , ESS.epochSyncStatsPhase           = "ingest"
  }

sampleEpochSyncTime :: ESS.EpochSyncTime
sampleEpochSyncTime = ESS.EpochSyncTime
  { ESS.epochSyncTimeNo      = 1
  , ESS.epochSyncTimeSeconds = 1
  , ESS.epochSyncTimeState   = SyncLagging
  }

sampleEpochFinalized :: EV.EpochFinalized
sampleEpochFinalized = EV.EpochFinalized
  { EV.epochFinalizedOutSum    = 1
  , EV.epochFinalizedFees      = DbLovelace 1
  , EV.epochFinalizedTxCount   = 1
  , EV.epochFinalizedBlkCount  = 1
  , EV.epochFinalizedNo        = 1
  , EV.epochFinalizedStartTime = t0
  , EV.epochFinalizedEndTime   = t0
  }

sampleDrepHash :: Gov.DrepHash
sampleDrepHash = Gov.DrepHash
  { Gov.drepHashRaw       = Nothing
  , Gov.drepHashView      = "drep_always_abstain"
  , Gov.drepHashHasScript = False
  }

sampleDrepRegistration :: Gov.DrepRegistration
sampleDrepRegistration = Gov.DrepRegistration
  { Gov.drepRegistrationTxId           = TxId 1
  , Gov.drepRegistrationCertIndex      = 0
  , Gov.drepRegistrationDeposit        = Nothing
  , Gov.drepRegistrationDrepHashId     = DrepHashId 1
  , Gov.drepRegistrationVotingAnchorId = Nothing
  }

sampleDrepDistr :: Gov.DrepDistr
sampleDrepDistr = Gov.DrepDistr
  { Gov.drepDistrHashId      = DrepHashId 1
  , Gov.drepDistrAmount      = 1
  , Gov.drepDistrEpochNo     = 1
  , Gov.drepDistrActiveUntil = Nothing
  }

sampleDelegationVote :: Gov.DelegationVote
sampleDelegationVote = Gov.DelegationVote
  { Gov.delegationVoteAddrId     = StakeAddressId 1
  , Gov.delegationVoteCertIndex  = 0
  , Gov.delegationVoteDrepHashId = DrepHashId 1
  , Gov.delegationVoteTxId       = TxId 1
  , Gov.delegationVoteRedeemerId = Nothing
  }

sampleVotingProcedure :: Gov.VotingProcedure
sampleVotingProcedure = Gov.VotingProcedure
  { Gov.votingProcedureTxId                = TxId 1
  , Gov.votingProcedureIndex               = 0
  , Gov.votingProcedureGovActionProposalId = GovActionProposalId 1
  , Gov.votingProcedureVoterRole           = DRep
  , Gov.votingProcedureDrepVoter           = Nothing
  , Gov.votingProcedurePoolVoter           = Nothing
  , Gov.votingProcedureVote                = VoteYes
  , Gov.votingProcedureVotingAnchorId      = Nothing
  , Gov.votingProcedureCommitteeVoter      = Nothing
  , Gov.votingProcedureInvalid             = Nothing
  }

sampleVotingAnchor :: Gov.VotingAnchor
sampleVotingAnchor = Gov.VotingAnchor
  { Gov.votingAnchorUrl      = VoteUrl "https://example.com"
  , Gov.votingAnchorDataHash = "\9"
  , Gov.votingAnchorType     = GovActionAnchor
  , Gov.votingAnchorBlockId  = BlockId 1
  }

sampleConstitution :: Gov.Constitution
sampleConstitution = Gov.Constitution
  { Gov.constitutionGovActionProposalId = Nothing
  , Gov.constitutionVotingAnchorId      = VotingAnchorId 1
  , Gov.constitutionScriptHash          = Nothing
  }

sampleCommittee :: Gov.Committee
sampleCommittee = Gov.Committee
  { Gov.committeeGovActionProposalId = Nothing
  , Gov.committeeQuorumNumerator     = 1
  , Gov.committeeQuorumDenominator   = 2
  }

sampleCommitteeHash :: Gov.CommitteeHash
sampleCommitteeHash = Gov.CommitteeHash
  { Gov.committeeHashRaw       = "\10"
  , Gov.committeeHashHasScript = False
  }

sampleCommitteeMember :: Gov.CommitteeMember
sampleCommitteeMember = Gov.CommitteeMember
  { Gov.committeeMemberCommitteeId     = CommitteeId 1
  , Gov.committeeMemberCommitteeHashId = CommitteeHashId 1
  , Gov.committeeMemberExpirationEpoch = 1
  }

sampleCommitteeRegistration :: Gov.CommitteeRegistration
sampleCommitteeRegistration = Gov.CommitteeRegistration
  { Gov.committeeRegistrationTxId      = TxId 1
  , Gov.committeeRegistrationCertIndex = 0
  , Gov.committeeRegistrationColdKeyId = CommitteeHashId 1
  , Gov.committeeRegistrationHotKeyId  = CommitteeHashId 1
  }

sampleCommitteeDeRegistration :: Gov.CommitteeDeRegistration
sampleCommitteeDeRegistration = Gov.CommitteeDeRegistration
  { Gov.committeeDeRegistrationTxId           = TxId 1
  , Gov.committeeDeRegistrationCertIndex      = 0
  , Gov.committeeDeRegistrationVotingAnchorId = Nothing
  , Gov.committeeDeRegistrationColdKeyId      = CommitteeHashId 1
  }

sampleParamProposal :: Gov.ParamProposal
sampleParamProposal = Gov.ParamProposal
  { Gov.paramProposalEpochNo                    = Nothing
  , Gov.paramProposalKey                        = Nothing
  , Gov.paramProposalMinFeeA                    = Nothing
  , Gov.paramProposalMinFeeB                    = Nothing
  , Gov.paramProposalMaxBlockSize               = Nothing
  , Gov.paramProposalMaxTxSize                  = Nothing
  , Gov.paramProposalMaxBhSize                  = Nothing
  , Gov.paramProposalKeyDeposit                 = Nothing
  , Gov.paramProposalPoolDeposit                = Nothing
  , Gov.paramProposalMaxEpoch                   = Nothing
  , Gov.paramProposalOptimalPoolCount           = Nothing
  , Gov.paramProposalInfluence                  = Nothing
  , Gov.paramProposalMonetaryExpandRate         = Nothing
  , Gov.paramProposalTreasuryGrowthRate         = Nothing
  , Gov.paramProposalDecentralisation           = Nothing
  , Gov.paramProposalEntropy                    = Nothing
  , Gov.paramProposalProtocolMajor              = Nothing
  , Gov.paramProposalProtocolMinor              = Nothing
  , Gov.paramProposalMinUtxoValue               = Nothing
  , Gov.paramProposalMinPoolCost                = Nothing
  , Gov.paramProposalCostModelId                = Nothing
  , Gov.paramProposalPriceMem                   = Nothing
  , Gov.paramProposalPriceStep                  = Nothing
  , Gov.paramProposalMaxTxExMem                 = Nothing
  , Gov.paramProposalMaxTxExSteps               = Nothing
  , Gov.paramProposalMaxBlockExMem              = Nothing
  , Gov.paramProposalMaxBlockExSteps            = Nothing
  , Gov.paramProposalMaxValSize                 = Nothing
  , Gov.paramProposalCollateralPercent          = Nothing
  , Gov.paramProposalMaxCollateralInputs        = Nothing
  , Gov.paramProposalRegisteredTxId             = TxId 1
  , Gov.paramProposalCoinsPerUtxoSize           = Nothing
  , Gov.paramProposalPvtMotionNoConfidence      = Nothing
  , Gov.paramProposalPvtCommitteeNormal         = Nothing
  , Gov.paramProposalPvtCommitteeNoConfidence   = Nothing
  , Gov.paramProposalPvtHardForkInitiation      = Nothing
  , Gov.paramProposalPvtppSecurityGroup         = Nothing
  , Gov.paramProposalDvtMotionNoConfidence      = Nothing
  , Gov.paramProposalDvtCommitteeNormal         = Nothing
  , Gov.paramProposalDvtCommitteeNoConfidence   = Nothing
  , Gov.paramProposalDvtUpdateToConstitution    = Nothing
  , Gov.paramProposalDvtHardForkInitiation      = Nothing
  , Gov.paramProposalDvtPPNetworkGroup          = Nothing
  , Gov.paramProposalDvtPPEconomicGroup         = Nothing
  , Gov.paramProposalDvtPPTechnicalGroup        = Nothing
  , Gov.paramProposalDvtPPGovGroup              = Nothing
  , Gov.paramProposalDvtTreasuryWithdrawal      = Nothing
  , Gov.paramProposalCommitteeMinSize           = Nothing
  , Gov.paramProposalCommitteeMaxTermLength     = Nothing
  , Gov.paramProposalGovActionLifetime          = Nothing
  , Gov.paramProposalGovActionDeposit           = Nothing
  , Gov.paramProposalDrepDeposit                = Nothing
  , Gov.paramProposalDrepActivity               = Nothing
  , Gov.paramProposalMinFeeRefScriptCostPerByte = Nothing
  }

sampleTreasuryWithdrawal :: Gov.TreasuryWithdrawal
sampleTreasuryWithdrawal = Gov.TreasuryWithdrawal
  { Gov.treasuryWithdrawalGovActionProposalId = GovActionProposalId 1
  , Gov.treasuryWithdrawalStakeAddressId      = StakeAddressId 1
  , Gov.treasuryWithdrawalAmount              = DbLovelace 1
  }

sampleEventInfo :: Gov.EventInfo
sampleEventInfo = Gov.EventInfo
  { Gov.eventInfoTxId        = Nothing
  , Gov.eventInfoEpoch       = 1
  , Gov.eventInfoType        = "enacted"
  , Gov.eventInfoExplanation = Nothing
  }

sampleTxMetadata :: Metadata.TxMetadata
sampleTxMetadata = Metadata.TxMetadata
  { Metadata.txMetadataKey   = DbWord64 1
  , Metadata.txMetadataJson  = Nothing
  , Metadata.txMetadataBytes = "\11"
  , Metadata.txMetadataTxId  = TxId 1
  }

sampleMultiAsset :: MA.MultiAsset
sampleMultiAsset = MA.MultiAsset
  { MA.multiAssetPolicy      = "\12"
  , MA.multiAssetName        = "\13"
  , MA.multiAssetFingerprint = "asset1"
  }

sampleMaTxMint :: MA.MaTxMint
sampleMaTxMint = MA.MaTxMint
  { MA.maTxMintQuantity = 1
  , MA.maTxMintTxId     = TxId 1
  , MA.maTxMintIdent    = MultiAssetId 1
  }

sampleMaTxOut :: MA.MaTxOut
sampleMaTxOut = MA.MaTxOut
  { MA.maTxOutQuantity = DbWord64 1
  , MA.maTxOutTxOutId  = TxOutId 1
  , MA.maTxOutIdent    = MultiAssetId 1
  }

sampleOffChainPoolData :: OCP.OffChainPoolData
sampleOffChainPoolData = OCP.OffChainPoolData
  { OCP.offChainPoolDataPoolId     = PoolHashId 1
  , OCP.offChainPoolDataTickerName = "TICK"
  , OCP.offChainPoolDataHash       = "\14"
  , OCP.offChainPoolDataJson       = "{}"
  , OCP.offChainPoolDataBytes      = "\15"
  , OCP.offChainPoolDataPmrId      = PoolMetadataRefId 1
  }

sampleOffChainPoolFetchError :: OCP.OffChainPoolFetchError
sampleOffChainPoolFetchError = OCP.OffChainPoolFetchError
  { OCP.offChainPoolFetchErrorPoolId     = PoolHashId 1
  , OCP.offChainPoolFetchErrorFetchTime  = t0
  , OCP.offChainPoolFetchErrorPmrId      = PoolMetadataRefId 1
  , OCP.offChainPoolFetchErrorFetchError = "timeout"
  , OCP.offChainPoolFetchErrorRetryCount = 0
  }

sampleOffChainVoteData :: OCV.OffChainVoteData
sampleOffChainVoteData = OCV.OffChainVoteData
  { OCV.offChainVoteDataVotingAnchorId = VotingAnchorId 1
  , OCV.offChainVoteDataHash           = "\16"
  , OCV.offChainVoteDataJson           = "{}"
  , OCV.offChainVoteDataBytes          = "\17"
  , OCV.offChainVoteDataWarning        = Nothing
  , OCV.offChainVoteDataLanguage       = "en"
  , OCV.offChainVoteDataComment        = Nothing
  , OCV.offChainVoteDataIsValid        = Nothing
  }

sampleOffChainVoteGovActionData :: OCV.OffChainVoteGovActionData
sampleOffChainVoteGovActionData = OCV.OffChainVoteGovActionData
  { OCV.offChainVoteGovActionDataOffChainVoteDataId = OffChainVoteDataId 1
  , OCV.offChainVoteGovActionDataTitle              = "t"
  , OCV.offChainVoteGovActionDataAbstract           = "a"
  , OCV.offChainVoteGovActionDataMotivation         = "m"
  , OCV.offChainVoteGovActionDataRationale          = "r"
  }

sampleOffChainVoteDrepData :: OCV.OffChainVoteDrepData
sampleOffChainVoteDrepData = OCV.OffChainVoteDrepData
  { OCV.offChainVoteDrepDataOffChainVoteDataId = OffChainVoteDataId 1
  , OCV.offChainVoteDrepDataPaymentAddress     = Nothing
  , OCV.offChainVoteDrepDataGivenName          = "n"
  , OCV.offChainVoteDrepDataObjectives         = Nothing
  , OCV.offChainVoteDrepDataMotivations        = Nothing
  , OCV.offChainVoteDrepDataQualifications     = Nothing
  , OCV.offChainVoteDrepDataImageUrl           = Nothing
  , OCV.offChainVoteDrepDataImageHash          = Nothing
  }

sampleOffChainVoteAuthor :: OCV.OffChainVoteAuthor
sampleOffChainVoteAuthor = OCV.OffChainVoteAuthor
  { OCV.offChainVoteAuthorOffChainVoteDataId = OffChainVoteDataId 1
  , OCV.offChainVoteAuthorName               = Nothing
  , OCV.offChainVoteAuthorWitnessAlgorithm   = "ed25519"
  , OCV.offChainVoteAuthorPublicKey          = "pk"
  , OCV.offChainVoteAuthorSignature          = "sig"
  , OCV.offChainVoteAuthorWarning            = Nothing
  }

sampleOffChainVoteReference :: OCV.OffChainVoteReference
sampleOffChainVoteReference = OCV.OffChainVoteReference
  { OCV.offChainVoteReferenceOffChainVoteDataId = OffChainVoteDataId 1
  , OCV.offChainVoteReferenceLabel              = "l"
  , OCV.offChainVoteReferenceUri                = "u"
  , OCV.offChainVoteReferenceHashDigest         = Nothing
  , OCV.offChainVoteReferenceHashAlgorithm      = Nothing
  }

sampleOffChainVoteExternalUpdate :: OCV.OffChainVoteExternalUpdate
sampleOffChainVoteExternalUpdate = OCV.OffChainVoteExternalUpdate
  { OCV.offChainVoteExternalUpdateOffChainVoteDataId = OffChainVoteDataId 1
  , OCV.offChainVoteExternalUpdateTitle              = "t"
  , OCV.offChainVoteExternalUpdateUri                = "u"
  }

sampleOffChainVoteFetchError :: OCV.OffChainVoteFetchError
sampleOffChainVoteFetchError = OCV.OffChainVoteFetchError
  { OCV.offChainVoteFetchErrorVotingAnchorId = VotingAnchorId 1
  , OCV.offChainVoteFetchErrorFetchError     = "timeout"
  , OCV.offChainVoteFetchErrorFetchTime      = t0
  , OCV.offChainVoteFetchErrorRetryCount     = 0
  }

samplePoolUpdate :: Pool.PoolUpdate
samplePoolUpdate = Pool.PoolUpdate
  { Pool.poolUpdateHashId         = PoolHashId 1
  , Pool.poolUpdateCertIndex      = 0
  , Pool.poolUpdateVrfKeyHash     = "\18"
  , Pool.poolUpdatePledge         = DbLovelace 1
  , Pool.poolUpdateActiveEpochNo  = 1
  , Pool.poolUpdateMetaId         = Nothing
  , Pool.poolUpdateMargin         = 0.1
  , Pool.poolUpdateFixedCost      = DbLovelace 1
  , Pool.poolUpdateRegisteredTxId = TxId 1
  , Pool.poolUpdateRewardAddrId   = StakeAddressId 1
  , Pool.poolUpdateDeposit        = Nothing
  }

samplePoolMetadataRef :: Pool.PoolMetadataRef
samplePoolMetadataRef = Pool.PoolMetadataRef
  { Pool.poolMetadataRefPoolId         = PoolHashId 1
  , Pool.poolMetadataRefUrl            = "https://example.com"
  , Pool.poolMetadataRefHash           = "\19"
  , Pool.poolMetadataRefRegisteredTxId = TxId 1
  }

samplePoolOwner :: Pool.PoolOwner
samplePoolOwner = Pool.PoolOwner
  { Pool.poolOwnerAddrId       = StakeAddressId 1
  , Pool.poolOwnerPoolUpdateId = PoolUpdateId 1
  }

samplePoolRetire :: Pool.PoolRetire
samplePoolRetire = Pool.PoolRetire
  { Pool.poolRetireHashId        = PoolHashId 1
  , Pool.poolRetireCertIndex     = 0
  , Pool.poolRetireAnnouncedTxId = TxId 1
  , Pool.poolRetireRetiringEpoch = 1
  }

samplePoolRelay :: Pool.PoolRelay
samplePoolRelay = Pool.PoolRelay
  { Pool.poolRelayUpdateId   = PoolUpdateId 1
  , Pool.poolRelayIpv4       = Nothing
  , Pool.poolRelayIpv6       = Nothing
  , Pool.poolRelayDnsName    = Nothing
  , Pool.poolRelayDnsSrvName = Nothing
  , Pool.poolRelayPort       = Nothing
  }

samplePoolStat :: Pool.PoolStat
samplePoolStat = Pool.PoolStat
  { Pool.poolStatPoolHashId         = PoolHashId 1
  , Pool.poolStatEpochNo            = 1
  , Pool.poolStatNumberOfBlocks     = DbWord64 1
  , Pool.poolStatNumberOfDelegators = DbWord64 1
  , Pool.poolStatStake              = DbWord64 1
  , Pool.poolStatVotingPower        = Nothing
  }

sampleDelistedPool :: Pool.DelistedPool
sampleDelistedPool = Pool.DelistedPool
  { Pool.delistedPoolHashRaw = "\20"
  }

sampleReservedPoolTicker :: Pool.ReservedPoolTicker
sampleReservedPoolTicker = Pool.ReservedPoolTicker
  { Pool.reservedPoolTickerName     = "TICK"
  , Pool.reservedPoolTickerPoolHash = "\21"
  }

sampleDatum :: SD.Datum
sampleDatum = SD.Datum
  { SD.datumHash  = "\22"
  , SD.datumTxId  = TxId 1
  , SD.datumValue = Nothing
  , SD.datumBytes = "\23"
  }

sampleScript :: SD.Script
sampleScript = SD.Script
  { SD.scriptTxId           = TxId 1
  , SD.scriptHash           = "\24"
  , SD.scriptType           = PlutusV1
  , SD.scriptJson           = Nothing
  , SD.scriptBytes          = Nothing
  , SD.scriptSerialisedSize = Nothing
  }

sampleRedeemer :: SD.Redeemer
sampleRedeemer = SD.Redeemer
  { SD.redeemerTxId           = TxId 1
  , SD.redeemerUnitMem        = 1
  , SD.redeemerUnitSteps      = 1
  , SD.redeemerFee            = Nothing
  , SD.redeemerPurpose        = Spend
  , SD.redeemerIndex          = 0
  , SD.redeemerScriptHash     = Nothing
  , SD.redeemerRedeemerDataId = RedeemerDataId 1
  }

sampleRedeemerData :: SD.RedeemerData
sampleRedeemerData = SD.RedeemerData
  { SD.redeemerDataHash  = "\25"
  , SD.redeemerDataTxId  = TxId 1
  , SD.redeemerDataValue = Nothing
  , SD.redeemerDataBytes = "\26"
  }

sampleExtraKeyWitness :: SD.ExtraKeyWitness
sampleExtraKeyWitness = SD.ExtraKeyWitness
  { SD.extraKeyWitnessHash = "\27"
  , SD.extraKeyWitnessTxId = TxId 1
  }

sampleStakeRegistration :: SDel.StakeRegistration
sampleStakeRegistration = SDel.StakeRegistration
  { SDel.stakeRegistrationAddrId    = StakeAddressId 1
  , SDel.stakeRegistrationCertIndex = 0
  , SDel.stakeRegistrationEpochNo   = 1
  , SDel.stakeRegistrationTxId      = TxId 1
  , SDel.stakeRegistrationDeposit   = Nothing
  }

sampleStakeDeregistration :: SDel.StakeDeregistration
sampleStakeDeregistration = SDel.StakeDeregistration
  { SDel.stakeDeregistrationAddrId     = StakeAddressId 1
  , SDel.stakeDeregistrationCertIndex  = 0
  , SDel.stakeDeregistrationEpochNo    = 1
  , SDel.stakeDeregistrationTxId       = TxId 1
  , SDel.stakeDeregistrationRedeemerId = Nothing
  }

sampleDelegation :: SDel.Delegation
sampleDelegation = SDel.Delegation
  { SDel.delegationAddrId        = StakeAddressId 1
  , SDel.delegationCertIndex     = 0
  , SDel.delegationPoolHashId    = PoolHashId 1
  , SDel.delegationActiveEpochNo = 1
  , SDel.delegationTxId          = TxId 1
  , SDel.delegationSlotNo        = 1
  , SDel.delegationRedeemerId    = Nothing
  }

sampleWithdrawal :: SDel.Withdrawal
sampleWithdrawal = SDel.Withdrawal
  { SDel.withdrawalAddrId     = StakeAddressId 1
  , SDel.withdrawalTxId       = TxId 1
  , SDel.withdrawalAmount     = DbLovelace 1
  , SDel.withdrawalRedeemerId = Nothing
  }

sampleReward :: SDel.Reward
sampleReward = SDel.Reward
  { SDel.rewardAddrId         = StakeAddressId 1
  , SDel.rewardType           = RwdLeader
  , SDel.rewardAmount         = DbLovelace 1
  , SDel.rewardSpendableEpoch = 2
  , SDel.rewardPoolId         = PoolHashId 1
  , SDel.rewardEarnedEpoch    = 1
  }

samplePotReward :: SDel.PotReward
samplePotReward = SDel.PotReward
  { SDel.potRewardAddrId         = StakeAddressId 1
  , SDel.potRewardType           = RwdTreasury
  , SDel.potRewardAmount         = DbLovelace 1
  , SDel.potRewardSpendableEpoch = 2
  , SDel.potRewardEarnedEpoch    = 1
  }

sampleEpochStake :: SDel.EpochStake
sampleEpochStake = SDel.EpochStake
  { SDel.epochStakeAddrId  = StakeAddressId 1
  , SDel.epochStakePoolId  = PoolHashId 1
  , SDel.epochStakeAmount  = DbLovelace 1
  , SDel.epochStakeEpochNo = 1
  }

sampleEpochStakeProgress :: SDel.EpochStakeProgress
sampleEpochStakeProgress = SDel.EpochStakeProgress
  { SDel.epochStakeProgressEpochNo   = 1
  , SDel.epochStakeProgressCompleted = True
  }

sampleTxOut :: UTxO.TxOut
sampleTxOut = UTxO.TxOut
  { UTxO.txOutTxId              = TxId 1
  , UTxO.txOutIndex             = 0
  , UTxO.txOutAddressId         = Nothing
  , UTxO.txOutStakeAddressId    = Nothing
  , UTxO.txOutValue             = DbLovelace 1
  , UTxO.txOutDataHash          = Nothing
  , UTxO.txOutInlineDatumId     = Nothing
  , UTxO.txOutReferenceScriptId = Nothing
  , UTxO.txOutConsumedByTxId    = Nothing
  }

sampleTxIn :: UTxO.TxIn
sampleTxIn = UTxO.TxIn
  { UTxO.txInTxInId     = TxId 1
  , UTxO.txInTxOutId    = Nothing
  , UTxO.txInTxOutIndex = 0
  , UTxO.txInTxOutHash  = "\28"
  , UTxO.txInRedeemerId = Nothing
  }

sampleCollateralTxIn :: UTxO.CollateralTxIn
sampleCollateralTxIn = UTxO.CollateralTxIn
  { UTxO.collateralTxInTxInId     = TxId 1
  , UTxO.collateralTxInTxOutId    = Nothing
  , UTxO.collateralTxInTxOutIndex = 0
  , UTxO.collateralTxInTxOutHash  = "\29"
  }

sampleReferenceTxIn :: UTxO.ReferenceTxIn
sampleReferenceTxIn = UTxO.ReferenceTxIn
  { UTxO.referenceTxInTxInId     = TxId 1
  , UTxO.referenceTxInTxOutId    = Nothing
  , UTxO.referenceTxInTxOutIndex = 0
  , UTxO.referenceTxInTxOutHash  = "\30"
  }

sampleCollateralTxOut :: UTxO.CollateralTxOut
sampleCollateralTxOut = UTxO.CollateralTxOut
  { UTxO.collateralTxOutTxId              = TxId 1
  , UTxO.collateralTxOutIndex             = 0
  , UTxO.collateralTxOutAddressId         = Nothing
  , UTxO.collateralTxOutStakeAddressId    = Nothing
  , UTxO.collateralTxOutValue             = DbLovelace 1
  , UTxO.collateralTxOutDataHash          = Nothing
  , UTxO.collateralTxOutMultiAssetsDescr  = ""
  , UTxO.collateralTxOutInlineDatumId     = Nothing
  , UTxO.collateralTxOutReferenceScriptId = Nothing
  }
