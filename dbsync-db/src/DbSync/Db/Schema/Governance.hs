{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Schema types for Conway-era governance tables.
--
-- One module, one extractor (@governance@). All 16 tables here are
-- populated from block data once a Conway-era transaction carries a
-- governance certificate, vote, proposal, or anchor URL.
--
-- The FK graph inside this module is dense — voting procedures
-- reference proposals, anchors, drep hashes, pool hashes, committee
-- hashes, and event-info rows; proposals reference param proposals,
-- voting anchors, and other proposals; constitutions reference
-- proposals and anchors. Splitting these into separate extractors
-- would force a dance of NULL FKs and post-load resolution; keeping
-- them together lets us pre-assign every ID up-front during
-- @processBlock@ and write rows in any order.
--
-- @param_proposal.cost_model_id@ references the @cost_model@ table
-- which is owned by the @epoch_boundary@ extractor (commit 6); the
-- column stays as a nullable @BIGINT@ here so the schema compiles
-- in isolation.
module DbSync.Db.Schema.Governance
  ( -- * Schema types
    DrepHash (..)
  , DrepRegistration (..)
  , DrepDistr (..)
  , DelegationVote (..)
  , GovActionProposal (..)
  , VotingProcedure (..)
  , VotingAnchor (..)
  , Constitution (..)
  , Committee (..)
  , CommitteeHash (..)
  , CommitteeMember (..)
  , CommitteeRegistration (..)
  , CommitteeDeRegistration (..)
  , ParamProposal (..)
  , TreasuryWithdrawal (..)
  , EventInfo (..)

    -- * Table definitions
  , drepHashTableDef
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

    -- * COPY encoding
  , encodeDrepHashCopy
  , encodeDrepRegistrationCopy
  , encodeDrepDistrCopy
  , encodeDelegationVoteCopy
  , encodeGovActionProposalCopy
  , encodeVotingProcedureCopy
  , encodeVotingAnchorCopy
  , encodeConstitutionCopy
  , encodeCommitteeCopy
  , encodeCommitteeHashCopy
  , encodeCommitteeMemberCopy
  , encodeCommitteeRegistrationCopy
  , encodeCommitteeDeRegistrationCopy
  , encodeParamProposalCopy
  , encodeTreasuryWithdrawalCopy
  , encodeEventInfoCopy

    -- * Hasql encoders \/ decoders
  , drepHashEncoder, drepHashDecoder, entityDrepHashDecoder
  , drepRegistrationEncoder, drepRegistrationDecoder, entityDrepRegistrationDecoder
  , drepDistrEncoder, drepDistrDecoder, entityDrepDistrDecoder
  , delegationVoteEncoder, delegationVoteDecoder, entityDelegationVoteDecoder
  , govActionProposalEncoder, govActionProposalDecoder, entityGovActionProposalDecoder
  , votingProcedureEncoder, votingProcedureDecoder, entityVotingProcedureDecoder
  , votingAnchorEncoder, votingAnchorDecoder, entityVotingAnchorDecoder
  , constitutionEncoder, constitutionDecoder, entityConstitutionDecoder
  , committeeEncoder, committeeDecoder, entityCommitteeDecoder
  , committeeHashEncoder, committeeHashDecoder, entityCommitteeHashDecoder
  , committeeMemberEncoder, committeeMemberDecoder, entityCommitteeMemberDecoder
  , committeeRegistrationEncoder, committeeRegistrationDecoder, entityCommitteeRegistrationDecoder
  , committeeDeRegistrationEncoder, committeeDeRegistrationDecoder, entityCommitteeDeRegistrationDecoder
  , paramProposalEncoder, paramProposalDecoder, entityParamProposalDecoder
  , treasuryWithdrawalEncoder, treasuryWithdrawalDecoder, entityTreasuryWithdrawalDecoder
  , eventInfoEncoder, eventInfoDecoder, entityEventInfoDecoder
  ) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Text.Read as TR

import DbSync.Db.Schema.Entity (Key)
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.Types
import DbSync.Db.Types
  ( AnchorType
  , DbLovelace (..)
  , DbWord64 (..)
  , GovActionType
  , Vote
  , VoteUrl (..)
  , VoterRole
  , anchorTypeDecoder
  , anchorTypeEncoder
  , bAnchorType
  , bGovActionType
  , bVote
  , bVoterRole
  , dbLovelaceValueDecoder
  , dbLovelaceValueEncoder
  , govActionTypeDecoder
  , govActionTypeEncoder
  , maybeDbLovelaceDecoder
  , maybeDbLovelaceEncoder
  , maybeDbWord64Decoder
  , maybeDbWord64Encoder
  , voteDecoder
  , voteEncoder
  , voterRoleDecoder
  , voterRoleEncoder
  , voteUrlDecoder
  , voteUrlEncoder
  )
import DbSync.Db.Writer.Copy.Encoder
  ( buildCopyRow
  , bBool
  , bHex
  , bInt64
  , bText
  , bWord64
  )

-- ---------------------------------------------------------------------------
-- * Key type family instances
-- ---------------------------------------------------------------------------

type instance Key DrepHash = DrepHashId
type instance Key DrepRegistration = DrepRegistrationId
type instance Key DrepDistr = DrepDistrId
type instance Key DelegationVote = DelegationVoteId
type instance Key GovActionProposal = GovActionProposalId
type instance Key VotingProcedure = VotingProcedureId
type instance Key VotingAnchor = VotingAnchorId
type instance Key Constitution = ConstitutionId
type instance Key Committee = CommitteeId
type instance Key CommitteeHash = CommitteeHashId
type instance Key CommitteeMember = CommitteeMemberId
type instance Key CommitteeRegistration = CommitteeRegistrationId
type instance Key CommitteeDeRegistration = CommitteeDeRegistrationId
type instance Key ParamProposal = ParamProposalId
type instance Key TreasuryWithdrawal = TreasuryWithdrawalId
type instance Key EventInfo = EventInfoId

-- ---------------------------------------------------------------------------
-- * Schema types
-- ---------------------------------------------------------------------------

-- | The @drep_hash@ table (dedup table).
--
-- One row per DRep credential. @raw@ is nullable because the two
-- abstract DReps — @always_abstain@ and @always_no_confidence@ —
-- have no concrete hash; their @view@ string is the discriminator.
data DrepHash = DrepHash
  { drepHashRaw       :: !(Maybe ByteString) -- ^ 28-byte credential hash, or NULL for abstract DReps
  , drepHashView      :: !Text               -- ^ Bech32 form, or @drep_always_abstain@ / @drep_always_no_confidence@
  , drepHashHasScript :: !Bool               -- ^ True for script-based DReps
  }
  deriving stock (Eq, Show)

-- | The @drep_registration@ table.
--
-- @deposit@ is a signed @Int64@ rather than 'DbLovelace' because a
-- DRep deregistration row carries a negative refund amount.
data DrepRegistration = DrepRegistration
  { drepRegistrationTxId           :: !TxId
  , drepRegistrationCertIndex      :: !Word16
  , drepRegistrationDeposit        :: !(Maybe Int64)
  , drepRegistrationDrepHashId     :: !DrepHashId
  , drepRegistrationVotingAnchorId :: !(Maybe VotingAnchorId)
  }
  deriving stock (Eq, Show)

-- | The @drep_distr@ table. One row per (drep, epoch); written by the
-- ledger worker. Unique on @(hash_id, epoch_no)@.
data DrepDistr = DrepDistr
  { drepDistrHashId      :: !DrepHashId
  , drepDistrAmount      :: !Word64
  , drepDistrEpochNo     :: !Word64
  , drepDistrActiveUntil :: !(Maybe Word64)
  }
  deriving stock (Eq, Show)

-- | The @delegation_vote@ table — a stake address picks a DRep.
data DelegationVote = DelegationVote
  { delegationVoteAddrId     :: !StakeAddressId
  , delegationVoteCertIndex  :: !Word16
  , delegationVoteDrepHashId :: !DrepHashId
  , delegationVoteTxId       :: !TxId
  , delegationVoteRedeemerId :: !(Maybe RedeemerId)
  }
  deriving stock (Eq, Show)

-- | The @gov_action_proposal@ table.
--
-- @description@ is JSONB at the column level; we hand it to the COPY
-- writer as plain text and let PostgreSQL parse it on insert. The
-- self-FK @prev_gov_action_proposal@ links amendment chains.
data GovActionProposal = GovActionProposal
  { govActionProposalTxId                  :: !TxId
  , govActionProposalIndex                 :: !Word64
  , govActionProposalPrevGovActionProposal :: !(Maybe GovActionProposalId)
  , govActionProposalDeposit               :: !DbLovelace
  , govActionProposalReturnAddress         :: !StakeAddressId
  , govActionProposalExpiration            :: !(Maybe Word64)
  , govActionProposalVotingAnchorId        :: !(Maybe VotingAnchorId)
  , govActionProposalType                  :: !GovActionType
  , govActionProposalDescription           :: !Text
  , govActionProposalParamProposal         :: !(Maybe ParamProposalId)
  , govActionProposalRatifiedEpoch         :: !(Maybe Word64)
  , govActionProposalEnactedEpoch          :: !(Maybe Word64)
  , govActionProposalDroppedEpoch          :: !(Maybe Word64)
  , govActionProposalExpiredEpoch          :: !(Maybe Word64)
  }
  deriving stock (Eq, Show)

-- | The @voting_procedure@ table.
--
-- Three nullable voter ID columns — exactly one of @drep_voter@,
-- @pool_voter@, @committee_voter@ is non-NULL per row, picked by
-- @voter_role@. Ported as-is from the original; a future projection
-- variant could collapse them.
data VotingProcedure = VotingProcedure
  { votingProcedureTxId                :: !TxId
  , votingProcedureIndex               :: !Word16
  , votingProcedureGovActionProposalId :: !GovActionProposalId
  , votingProcedureVoterRole           :: !VoterRole
  , votingProcedureDrepVoter           :: !(Maybe DrepHashId)
  , votingProcedurePoolVoter           :: !(Maybe PoolHashId)
  , votingProcedureVote                :: !Vote
  , votingProcedureVotingAnchorId      :: !(Maybe VotingAnchorId)
  , votingProcedureCommitteeVoter      :: !(Maybe CommitteeHashId)
  , votingProcedureInvalid             :: !(Maybe EventInfoId)
  }
  deriving stock (Eq, Show)

-- | The @voting_anchor@ table — an off-chain document URL plus its
-- expected hash. Unique on @(data_hash, url, type)@.
data VotingAnchor = VotingAnchor
  { votingAnchorUrl      :: !VoteUrl
  , votingAnchorDataHash :: !ByteString
  , votingAnchorType     :: !AnchorType
  , votingAnchorBlockId  :: !BlockId
  }
  deriving stock (Eq, Show)

-- | The @constitution@ table. One row per constitution change.
data Constitution = Constitution
  { constitutionGovActionProposalId :: !(Maybe GovActionProposalId)
  , constitutionVotingAnchorId      :: !VotingAnchorId
  , constitutionScriptHash          :: !(Maybe ByteString)
  }
  deriving stock (Eq, Show)

-- | The @committee@ table — one row per committee membership change.
data Committee = Committee
  { committeeGovActionProposalId :: !(Maybe GovActionProposalId)
  , committeeQuorumNumerator     :: !Word64
  , committeeQuorumDenominator   :: !Word64
  }
  deriving stock (Eq, Show)

-- | The @committee_hash@ table (dedup table). Holds both cold and hot
-- committee key hashes; a single row can be referenced as either.
-- Unique on @(raw, has_script)@.
data CommitteeHash = CommitteeHash
  { committeeHashRaw       :: !ByteString
  , committeeHashHasScript :: !Bool
  }
  deriving stock (Eq, Show)

-- | The @committee_member@ table. Members are scoped by @committee_id@
-- so the same hash can appear under different committee snapshots.
data CommitteeMember = CommitteeMember
  { committeeMemberCommitteeId     :: !CommitteeId
  , committeeMemberCommitteeHashId :: !CommitteeHashId
  , committeeMemberExpirationEpoch :: !Word64
  }
  deriving stock (Eq, Show)

-- | The @committee_registration@ table. Each row pairs a cold key
-- (the on-chain identity) with a hot key (used for actual voting).
data CommitteeRegistration = CommitteeRegistration
  { committeeRegistrationTxId        :: !TxId
  , committeeRegistrationCertIndex   :: !Word16
  , committeeRegistrationColdKeyId   :: !CommitteeHashId
  , committeeRegistrationHotKeyId    :: !CommitteeHashId
  }
  deriving stock (Eq, Show)

-- | The @committee_de_registration@ table.
data CommitteeDeRegistration = CommitteeDeRegistration
  { committeeDeRegistrationTxId            :: !TxId
  , committeeDeRegistrationCertIndex       :: !Word16
  , committeeDeRegistrationVotingAnchorId  :: !(Maybe VotingAnchorId)
  , committeeDeRegistrationColdKeyId       :: !CommitteeHashId
  }
  deriving stock (Eq, Show)

-- | The @param_proposal@ table — 53 columns of optional parameter
-- overrides. Ported as-is per the AS-IS porting policy; a future
-- projection variant could fold the lot into a single JSONB.
--
-- Most columns are nullable — only those the proposer chose to
-- change are populated.
data ParamProposal = ParamProposal
  { paramProposalEpochNo                    :: !(Maybe Word64)
  , paramProposalKey                        :: !(Maybe ByteString)
  , paramProposalMinFeeA                    :: !(Maybe DbWord64)
  , paramProposalMinFeeB                    :: !(Maybe DbWord64)
  , paramProposalMaxBlockSize               :: !(Maybe DbWord64)
  , paramProposalMaxTxSize                  :: !(Maybe DbWord64)
  , paramProposalMaxBhSize                  :: !(Maybe DbWord64)
  , paramProposalKeyDeposit                 :: !(Maybe DbLovelace)
  , paramProposalPoolDeposit                :: !(Maybe DbLovelace)
  , paramProposalMaxEpoch                   :: !(Maybe DbWord64)
  , paramProposalOptimalPoolCount           :: !(Maybe DbWord64)
  , paramProposalInfluence                  :: !(Maybe Double)
  , paramProposalMonetaryExpandRate         :: !(Maybe Double)
  , paramProposalTreasuryGrowthRate         :: !(Maybe Double)
  , paramProposalDecentralisation           :: !(Maybe Double)
  , paramProposalEntropy                    :: !(Maybe ByteString)
  , paramProposalProtocolMajor              :: !(Maybe Word16)
  , paramProposalProtocolMinor              :: !(Maybe Word16)
  , paramProposalMinUtxoValue               :: !(Maybe DbLovelace)
  , paramProposalMinPoolCost                :: !(Maybe DbLovelace)
  , paramProposalCostModelId                :: !(Maybe CostModelId)
  , paramProposalPriceMem                   :: !(Maybe Double)
  , paramProposalPriceStep                  :: !(Maybe Double)
  , paramProposalMaxTxExMem                 :: !(Maybe DbWord64)
  , paramProposalMaxTxExSteps               :: !(Maybe DbWord64)
  , paramProposalMaxBlockExMem              :: !(Maybe DbWord64)
  , paramProposalMaxBlockExSteps            :: !(Maybe DbWord64)
  , paramProposalMaxValSize                 :: !(Maybe DbWord64)
  , paramProposalCollateralPercent          :: !(Maybe Word16)
  , paramProposalMaxCollateralInputs        :: !(Maybe Word16)
  , paramProposalRegisteredTxId             :: !TxId
  , paramProposalCoinsPerUtxoSize           :: !(Maybe DbLovelace)
  , paramProposalPvtMotionNoConfidence      :: !(Maybe Double)
  , paramProposalPvtCommitteeNormal         :: !(Maybe Double)
  , paramProposalPvtCommitteeNoConfidence   :: !(Maybe Double)
  , paramProposalPvtHardForkInitiation      :: !(Maybe Double)
  , paramProposalPvtppSecurityGroup         :: !(Maybe Double)
  , paramProposalDvtMotionNoConfidence      :: !(Maybe Double)
  , paramProposalDvtCommitteeNormal         :: !(Maybe Double)
  , paramProposalDvtCommitteeNoConfidence   :: !(Maybe Double)
  , paramProposalDvtUpdateToConstitution    :: !(Maybe Double)
  , paramProposalDvtHardForkInitiation      :: !(Maybe Double)
  , paramProposalDvtPPNetworkGroup          :: !(Maybe Double)
  , paramProposalDvtPPEconomicGroup         :: !(Maybe Double)
  , paramProposalDvtPPTechnicalGroup        :: !(Maybe Double)
  , paramProposalDvtPPGovGroup              :: !(Maybe Double)
  , paramProposalDvtTreasuryWithdrawal      :: !(Maybe Double)
  , paramProposalCommitteeMinSize           :: !(Maybe DbWord64)
  , paramProposalCommitteeMaxTermLength     :: !(Maybe DbWord64)
  , paramProposalGovActionLifetime          :: !(Maybe DbWord64)
  , paramProposalGovActionDeposit           :: !(Maybe DbWord64)
  , paramProposalDrepDeposit                :: !(Maybe DbWord64)
  , paramProposalDrepActivity               :: !(Maybe DbWord64)
  , paramProposalMinFeeRefScriptCostPerByte :: !(Maybe Double)
  }
  deriving stock (Eq, Show)

-- | The @treasury_withdrawal@ table — a join row between an enacted
-- gov_action_proposal and the stake address receiving the withdrawal.
data TreasuryWithdrawal = TreasuryWithdrawal
  { treasuryWithdrawalGovActionProposalId :: !GovActionProposalId
  , treasuryWithdrawalStakeAddressId      :: !StakeAddressId
  , treasuryWithdrawalAmount              :: !DbLovelace
  }
  deriving stock (Eq, Show)

-- | The @event_info@ table — a free-form audit record attached to
-- voting procedures whose evaluation produced a notable event.
-- @type@ is plain text in the original (not an enum).
data EventInfo = EventInfo
  { eventInfoTxId        :: !(Maybe TxId)
  , eventInfoEpoch       :: !Word64
  , eventInfoType        :: !Text
  , eventInfoExplanation :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definitions
-- ---------------------------------------------------------------------------

drepHashTableDef :: TableDef
drepHashTableDef = TableDef
  { tdName    = "drep_hash"
  , tdColumns =
      [ ColumnDef "id"         PgBigInt  False
      , ColumnDef "raw"        PgBytea   True
      , ColumnDef "view"       PgText    False
      , ColumnDef "has_script" PgBoolean False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["raw" :| ["has_script"]]
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

drepRegistrationTableDef :: TableDef
drepRegistrationTableDef = TableDef
  { tdName    = "drep_registration"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt False
      , ColumnDef "tx_id"            PgBigInt False
      , ColumnDef "cert_index"       PgBigInt False
      , ColumnDef "deposit"          PgBigInt True
      , ColumnDef "drep_hash_id"     PgBigInt False
      , ColumnDef "voting_anchor_id" PgBigInt True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

drepDistrTableDef :: TableDef
drepDistrTableDef = TableDef
  { tdName    = "drep_distr"
  , tdColumns =
      [ ColumnDef "id"           PgBigInt False
      , ColumnDef "hash_id"      PgBigInt False
      , ColumnDef "amount"       PgBigInt False
      , ColumnDef "epoch_no"     PgBigInt False
      , ColumnDef "active_until" PgBigInt True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["hash_id" :| ["epoch_no"]]
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

delegationVoteTableDef :: TableDef
delegationVoteTableDef = TableDef
  { tdName    = "delegation_vote"
  , tdColumns =
      [ ColumnDef "id"           PgBigInt False
      , ColumnDef "addr_id"      PgBigInt False
      , ColumnDef "cert_index"   PgBigInt False
      , ColumnDef "drep_hash_id" PgBigInt False
      , ColumnDef "tx_id"        PgBigInt False
      , ColumnDef "redeemer_id"  PgBigInt True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

govActionProposalTableDef :: TableDef
govActionProposalTableDef = TableDef
  { tdName    = "gov_action_proposal"
  , tdColumns =
      [ ColumnDef "id"                       PgBigInt  False
      , ColumnDef "tx_id"                    PgBigInt  False
      , ColumnDef "index"                    PgBigInt  False
      , ColumnDef "prev_gov_action_proposal" PgBigInt  True
      , ColumnDef "deposit"                  PgNumeric False
      , ColumnDef "return_address"           PgBigInt  False
      , ColumnDef "expiration"               PgBigInt  True
      , ColumnDef "voting_anchor_id"         PgBigInt  True
      , ColumnDef "type"                     PgText    False
      , ColumnDef "description"              PgJsonb   False
      , ColumnDef "param_proposal"           PgBigInt  True
      , ColumnDef "ratified_epoch"           PgBigInt  True
      , ColumnDef "enacted_epoch"            PgBigInt  True
      , ColumnDef "dropped_epoch"            PgBigInt  True
      , ColumnDef "expired_epoch"            PgBigInt  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

votingProcedureTableDef :: TableDef
votingProcedureTableDef = TableDef
  { tdName    = "voting_procedure"
  , tdColumns =
      [ ColumnDef "id"                     PgBigInt False
      , ColumnDef "tx_id"                  PgBigInt False
      , ColumnDef "index"                  PgBigInt False
      , ColumnDef "gov_action_proposal_id" PgBigInt False
      , ColumnDef "voter_role"             PgText   False
      , ColumnDef "drep_voter"             PgBigInt True
      , ColumnDef "pool_voter"             PgBigInt True
      , ColumnDef "vote"                   PgText   False
      , ColumnDef "voting_anchor_id"       PgBigInt True
      , ColumnDef "committee_voter"        PgBigInt True
      , ColumnDef "invalid"                PgBigInt True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

votingAnchorTableDef :: TableDef
votingAnchorTableDef = TableDef
  { tdName    = "voting_anchor"
  , tdColumns =
      [ ColumnDef "id"        PgBigInt False
      , ColumnDef "url"       PgText   False
      , ColumnDef "data_hash" PgBytea  False
      , ColumnDef "type"      PgText   False
      , ColumnDef "block_id"  PgBigInt False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["data_hash" :| ["url", "type"]]
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

constitutionTableDef :: TableDef
constitutionTableDef = TableDef
  { tdName    = "constitution"
  , tdColumns =
      [ ColumnDef "id"                     PgBigInt False
      , ColumnDef "gov_action_proposal_id" PgBigInt True
      , ColumnDef "voting_anchor_id"       PgBigInt False
      , ColumnDef "script_hash"            PgBytea  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

committeeTableDef :: TableDef
committeeTableDef = TableDef
  { tdName    = "committee"
  , tdColumns =
      [ ColumnDef "id"                     PgBigInt False
      , ColumnDef "gov_action_proposal_id" PgBigInt True
      , ColumnDef "quorum_numerator"       PgBigInt False
      , ColumnDef "quorum_denominator"     PgBigInt False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

committeeHashTableDef :: TableDef
committeeHashTableDef = TableDef
  { tdName    = "committee_hash"
  , tdColumns =
      [ ColumnDef "id"         PgBigInt  False
      , ColumnDef "raw"        PgBytea   False
      , ColumnDef "has_script" PgBoolean False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = ["raw" :| ["has_script"]]
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

committeeMemberTableDef :: TableDef
committeeMemberTableDef = TableDef
  { tdName    = "committee_member"
  , tdColumns =
      [ ColumnDef "id"                PgBigInt False
      , ColumnDef "committee_id"      PgBigInt False
      , ColumnDef "committee_hash_id" PgBigInt False
      , ColumnDef "expiration_epoch"  PgBigInt False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

committeeRegistrationTableDef :: TableDef
committeeRegistrationTableDef = TableDef
  { tdName    = "committee_registration"
  , tdColumns =
      [ ColumnDef "id"          PgBigInt False
      , ColumnDef "tx_id"       PgBigInt False
      , ColumnDef "cert_index"  PgBigInt False
      , ColumnDef "cold_key_id" PgBigInt False
      , ColumnDef "hot_key_id"  PgBigInt False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

committeeDeRegistrationTableDef :: TableDef
committeeDeRegistrationTableDef = TableDef
  { tdName    = "committee_de_registration"
  , tdColumns =
      [ ColumnDef "id"               PgBigInt False
      , ColumnDef "tx_id"            PgBigInt False
      , ColumnDef "cert_index"       PgBigInt False
      , ColumnDef "voting_anchor_id" PgBigInt True
      , ColumnDef "cold_key_id"      PgBigInt False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

-- | 53 column metadata for @param_proposal@. Doubles ride a TEXT
-- column matching the existing pattern in @pool_update.margin@ /
-- @epoch_sync_stats@; bumping these to PG @float8@ is a future
-- refactor. @committee_max_term_length@ has no @sqltype@ tag in
-- the original — keep the @numeric@ shape used by every other
-- @DbWord64@ column.
paramProposalTableDef :: TableDef
paramProposalTableDef = TableDef
  { tdName    = "param_proposal"
  , tdColumns =
      [ ColumnDef "id"                            PgBigInt   False
      , ColumnDef "epoch_no"                      PgBigInt   True
      , ColumnDef "key"                           PgBytea    True
      , ColumnDef "min_fee_a"                     PgNumeric  True
      , ColumnDef "min_fee_b"                     PgNumeric  True
      , ColumnDef "max_block_size"                PgNumeric  True
      , ColumnDef "max_tx_size"                   PgNumeric  True
      , ColumnDef "max_bh_size"                   PgNumeric  True
      , ColumnDef "key_deposit"                   PgNumeric  True
      , ColumnDef "pool_deposit"                  PgNumeric  True
      , ColumnDef "max_epoch"                     PgNumeric  True
      , ColumnDef "optimal_pool_count"            PgNumeric  True
      , ColumnDef "influence"                     PgText     True
      , ColumnDef "monetary_expand_rate"          PgText     True
      , ColumnDef "treasury_growth_rate"          PgText     True
      , ColumnDef "decentralisation"              PgText     True
      , ColumnDef "entropy"                       PgBytea    True
      , ColumnDef "protocol_major"                PgSmallInt True
      , ColumnDef "protocol_minor"                PgSmallInt True
      , ColumnDef "min_utxo_value"                PgNumeric  True
      , ColumnDef "min_pool_cost"                 PgNumeric  True
      , ColumnDef "cost_model_id"                 PgBigInt   True
      , ColumnDef "price_mem"                     PgText     True
      , ColumnDef "price_step"                    PgText     True
      , ColumnDef "max_tx_ex_mem"                 PgNumeric  True
      , ColumnDef "max_tx_ex_steps"               PgNumeric  True
      , ColumnDef "max_block_ex_mem"              PgNumeric  True
      , ColumnDef "max_block_ex_steps"            PgNumeric  True
      , ColumnDef "max_val_size"                  PgNumeric  True
      , ColumnDef "collateral_percent"            PgSmallInt True
      , ColumnDef "max_collateral_inputs"         PgSmallInt True
      , ColumnDef "registered_tx_id"              PgBigInt   False
      , ColumnDef "coins_per_utxo_size"           PgNumeric  True
      , ColumnDef "pvt_motion_no_confidence"      PgText     True
      , ColumnDef "pvt_committee_normal"          PgText     True
      , ColumnDef "pvt_committee_no_confidence"   PgText     True
      , ColumnDef "pvt_hard_fork_initiation"      PgText     True
      , ColumnDef "pvtpp_security_group"          PgText     True
      , ColumnDef "dvt_motion_no_confidence"      PgText     True
      , ColumnDef "dvt_committee_normal"          PgText     True
      , ColumnDef "dvt_committee_no_confidence"   PgText     True
      , ColumnDef "dvt_update_to_constitution"    PgText     True
      , ColumnDef "dvt_hard_fork_initiation"      PgText     True
      , ColumnDef "dvt_pp_network_group"          PgText     True
      , ColumnDef "dvt_pp_economic_group"         PgText     True
      , ColumnDef "dvt_pp_technical_group"        PgText     True
      , ColumnDef "dvt_pp_gov_group"              PgText     True
      , ColumnDef "dvt_treasury_withdrawal"       PgText     True
      , ColumnDef "committee_min_size"            PgNumeric  True
      , ColumnDef "committee_max_term_length"     PgNumeric  True
      , ColumnDef "gov_action_lifetime"           PgNumeric  True
      , ColumnDef "gov_action_deposit"            PgNumeric  True
      , ColumnDef "drep_deposit"                  PgNumeric  True
      , ColumnDef "drep_activity"                 PgNumeric  True
      , ColumnDef "min_fee_ref_script_cost_per_byte" PgText  True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

treasuryWithdrawalTableDef :: TableDef
treasuryWithdrawalTableDef = TableDef
  { tdName    = "treasury_withdrawal"
  , tdColumns =
      [ ColumnDef "id"                     PgBigInt  False
      , ColumnDef "gov_action_proposal_id" PgBigInt  False
      , ColumnDef "stake_address_id"       PgBigInt  False
      , ColumnDef "amount"                 PgNumeric False
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

eventInfoTableDef :: TableDef
eventInfoTableDef = TableDef
  { tdName    = "event_info"
  , tdColumns =
      [ ColumnDef "id"          PgBigInt False
      , ColumnDef "tx_id"       PgBigInt True
      , ColumnDef "epoch"       PgBigInt False
      , ColumnDef "type"        PgText   False
      , ColumnDef "explanation" PgText   True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }

-- ---------------------------------------------------------------------------
-- * COPY encoding
-- ---------------------------------------------------------------------------

encodeDrepHashCopy :: DrepHashId -> DrepHash -> ByteString
encodeDrepHashCopy (DrepHashId rid) dh =
  buildCopyRow
    [ Just $ bInt64 rid
    , bHex <$> drepHashRaw dh
    , Just $ bText (drepHashView dh)
    , Just $ bBool (drepHashHasScript dh)
    ]

encodeDrepRegistrationCopy :: DrepRegistrationId -> DrepRegistration -> ByteString
encodeDrepRegistrationCopy (DrepRegistrationId rid) dr =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getTxId $ drepRegistrationTxId dr)
    , Just $ bInt64 (fromIntegral $ drepRegistrationCertIndex dr)
    , bInt64 <$> drepRegistrationDeposit dr
    , Just $ bInt64 (getDrepHashId $ drepRegistrationDrepHashId dr)
    , bInt64 . getVotingAnchorId <$> drepRegistrationVotingAnchorId dr
    ]

encodeDrepDistrCopy :: DrepDistrId -> DrepDistr -> ByteString
encodeDrepDistrCopy (DrepDistrId rid) dd =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getDrepHashId $ drepDistrHashId dd)
    , Just $ bWord64 (drepDistrAmount dd)
    , Just $ bWord64 (drepDistrEpochNo dd)
    , bWord64 <$> drepDistrActiveUntil dd
    ]

encodeDelegationVoteCopy :: DelegationVoteId -> DelegationVote -> ByteString
encodeDelegationVoteCopy (DelegationVoteId rid) dv =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getStakeAddressId $ delegationVoteAddrId dv)
    , Just $ bInt64 (fromIntegral $ delegationVoteCertIndex dv)
    , Just $ bInt64 (getDrepHashId $ delegationVoteDrepHashId dv)
    , Just $ bInt64 (getTxId $ delegationVoteTxId dv)
    , bInt64 . getRedeemerId <$> delegationVoteRedeemerId dv
    ]

-- | @description@ is JSONB at the column level; we hand it to the
-- COPY writer as plain text and PostgreSQL parses it on insert.
encodeGovActionProposalCopy
  :: GovActionProposalId -> GovActionProposal -> ByteString
encodeGovActionProposalCopy (GovActionProposalId rid) gap =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getTxId $ govActionProposalTxId gap)
    , Just $ bWord64 (govActionProposalIndex gap)
    , bInt64 . getGovActionProposalId <$> govActionProposalPrevGovActionProposal gap
    , Just $ bWord64 (unDbLovelace $ govActionProposalDeposit gap)
    , Just $ bInt64 (getStakeAddressId $ govActionProposalReturnAddress gap)
    , bWord64 <$> govActionProposalExpiration gap
    , bInt64 . getVotingAnchorId <$> govActionProposalVotingAnchorId gap
    , Just $ bGovActionType (govActionProposalType gap)
    , Just $ bText (govActionProposalDescription gap)
    , bInt64 . getParamProposalId <$> govActionProposalParamProposal gap
    , bWord64 <$> govActionProposalRatifiedEpoch gap
    , bWord64 <$> govActionProposalEnactedEpoch gap
    , bWord64 <$> govActionProposalDroppedEpoch gap
    , bWord64 <$> govActionProposalExpiredEpoch gap
    ]

encodeVotingProcedureCopy :: VotingProcedureId -> VotingProcedure -> ByteString
encodeVotingProcedureCopy (VotingProcedureId rid) vp =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getTxId $ votingProcedureTxId vp)
    , Just $ bInt64 (fromIntegral $ votingProcedureIndex vp)
    , Just $ bInt64 (getGovActionProposalId $ votingProcedureGovActionProposalId vp)
    , Just $ bVoterRole (votingProcedureVoterRole vp)
    , bInt64 . getDrepHashId <$> votingProcedureDrepVoter vp
    , bInt64 . getPoolHashId <$> votingProcedurePoolVoter vp
    , Just $ bVote (votingProcedureVote vp)
    , bInt64 . getVotingAnchorId <$> votingProcedureVotingAnchorId vp
    , bInt64 . getCommitteeHashId <$> votingProcedureCommitteeVoter vp
    , bInt64 . getEventInfoId <$> votingProcedureInvalid vp
    ]

encodeVotingAnchorCopy :: VotingAnchorId -> VotingAnchor -> ByteString
encodeVotingAnchorCopy (VotingAnchorId rid) va =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bText (unVoteUrl $ votingAnchorUrl va)
    , Just $ bHex (votingAnchorDataHash va)
    , Just $ bAnchorType (votingAnchorType va)
    , Just $ bInt64 (getBlockId $ votingAnchorBlockId va)
    ]

encodeConstitutionCopy :: ConstitutionId -> Constitution -> ByteString
encodeConstitutionCopy (ConstitutionId rid) c =
  buildCopyRow
    [ Just $ bInt64 rid
    , bInt64 . getGovActionProposalId <$> constitutionGovActionProposalId c
    , Just $ bInt64 (getVotingAnchorId $ constitutionVotingAnchorId c)
    , bHex <$> constitutionScriptHash c
    ]

encodeCommitteeCopy :: CommitteeId -> Committee -> ByteString
encodeCommitteeCopy (CommitteeId rid) c =
  buildCopyRow
    [ Just $ bInt64 rid
    , bInt64 . getGovActionProposalId <$> committeeGovActionProposalId c
    , Just $ bWord64 (committeeQuorumNumerator c)
    , Just $ bWord64 (committeeQuorumDenominator c)
    ]

encodeCommitteeHashCopy :: CommitteeHashId -> CommitteeHash -> ByteString
encodeCommitteeHashCopy (CommitteeHashId rid) ch =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bHex (committeeHashRaw ch)
    , Just $ bBool (committeeHashHasScript ch)
    ]

encodeCommitteeMemberCopy :: CommitteeMemberId -> CommitteeMember -> ByteString
encodeCommitteeMemberCopy (CommitteeMemberId rid) cm =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getCommitteeId $ committeeMemberCommitteeId cm)
    , Just $ bInt64 (getCommitteeHashId $ committeeMemberCommitteeHashId cm)
    , Just $ bWord64 (committeeMemberExpirationEpoch cm)
    ]

encodeCommitteeRegistrationCopy
  :: CommitteeRegistrationId -> CommitteeRegistration -> ByteString
encodeCommitteeRegistrationCopy (CommitteeRegistrationId rid) cr =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getTxId $ committeeRegistrationTxId cr)
    , Just $ bInt64 (fromIntegral $ committeeRegistrationCertIndex cr)
    , Just $ bInt64 (getCommitteeHashId $ committeeRegistrationColdKeyId cr)
    , Just $ bInt64 (getCommitteeHashId $ committeeRegistrationHotKeyId cr)
    ]

encodeCommitteeDeRegistrationCopy
  :: CommitteeDeRegistrationId -> CommitteeDeRegistration -> ByteString
encodeCommitteeDeRegistrationCopy (CommitteeDeRegistrationId rid) cdr =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getTxId $ committeeDeRegistrationTxId cdr)
    , Just $ bInt64 (fromIntegral $ committeeDeRegistrationCertIndex cdr)
    , bInt64 . getVotingAnchorId <$> committeeDeRegistrationVotingAnchorId cdr
    , Just $ bInt64 (getCommitteeHashId $ committeeDeRegistrationColdKeyId cdr)
    ]

-- | 53 nullable parameter columns plus id and registered_tx_id.
-- Doubles serialise as ASCII via 'show'; the matching column type is
-- TEXT and the hasql codec round-trips through 'Read'.
encodeParamProposalCopy :: ParamProposalId -> ParamProposal -> ByteString
encodeParamProposalCopy (ParamProposalId rid) pp =
  buildCopyRow
    [ Just $ bInt64 rid
    , bWord64 <$> paramProposalEpochNo pp
    , bHex <$> paramProposalKey pp
    , bWord64 . unDbWord64 <$> paramProposalMinFeeA pp
    , bWord64 . unDbWord64 <$> paramProposalMinFeeB pp
    , bWord64 . unDbWord64 <$> paramProposalMaxBlockSize pp
    , bWord64 . unDbWord64 <$> paramProposalMaxTxSize pp
    , bWord64 . unDbWord64 <$> paramProposalMaxBhSize pp
    , bWord64 . unDbLovelace <$> paramProposalKeyDeposit pp
    , bWord64 . unDbLovelace <$> paramProposalPoolDeposit pp
    , bWord64 . unDbWord64 <$> paramProposalMaxEpoch pp
    , bWord64 . unDbWord64 <$> paramProposalOptimalPoolCount pp
    , bDouble <$> paramProposalInfluence pp
    , bDouble <$> paramProposalMonetaryExpandRate pp
    , bDouble <$> paramProposalTreasuryGrowthRate pp
    , bDouble <$> paramProposalDecentralisation pp
    , bHex <$> paramProposalEntropy pp
    , bInt64 . fromIntegral <$> paramProposalProtocolMajor pp
    , bInt64 . fromIntegral <$> paramProposalProtocolMinor pp
    , bWord64 . unDbLovelace <$> paramProposalMinUtxoValue pp
    , bWord64 . unDbLovelace <$> paramProposalMinPoolCost pp
    , bInt64 . getCostModelId <$> paramProposalCostModelId pp
    , bDouble <$> paramProposalPriceMem pp
    , bDouble <$> paramProposalPriceStep pp
    , bWord64 . unDbWord64 <$> paramProposalMaxTxExMem pp
    , bWord64 . unDbWord64 <$> paramProposalMaxTxExSteps pp
    , bWord64 . unDbWord64 <$> paramProposalMaxBlockExMem pp
    , bWord64 . unDbWord64 <$> paramProposalMaxBlockExSteps pp
    , bWord64 . unDbWord64 <$> paramProposalMaxValSize pp
    , bInt64 . fromIntegral <$> paramProposalCollateralPercent pp
    , bInt64 . fromIntegral <$> paramProposalMaxCollateralInputs pp
    , Just $ bInt64 (getTxId $ paramProposalRegisteredTxId pp)
    , bWord64 . unDbLovelace <$> paramProposalCoinsPerUtxoSize pp
    , bDouble <$> paramProposalPvtMotionNoConfidence pp
    , bDouble <$> paramProposalPvtCommitteeNormal pp
    , bDouble <$> paramProposalPvtCommitteeNoConfidence pp
    , bDouble <$> paramProposalPvtHardForkInitiation pp
    , bDouble <$> paramProposalPvtppSecurityGroup pp
    , bDouble <$> paramProposalDvtMotionNoConfidence pp
    , bDouble <$> paramProposalDvtCommitteeNormal pp
    , bDouble <$> paramProposalDvtCommitteeNoConfidence pp
    , bDouble <$> paramProposalDvtUpdateToConstitution pp
    , bDouble <$> paramProposalDvtHardForkInitiation pp
    , bDouble <$> paramProposalDvtPPNetworkGroup pp
    , bDouble <$> paramProposalDvtPPEconomicGroup pp
    , bDouble <$> paramProposalDvtPPTechnicalGroup pp
    , bDouble <$> paramProposalDvtPPGovGroup pp
    , bDouble <$> paramProposalDvtTreasuryWithdrawal pp
    , bWord64 . unDbWord64 <$> paramProposalCommitteeMinSize pp
    , bWord64 . unDbWord64 <$> paramProposalCommitteeMaxTermLength pp
    , bWord64 . unDbWord64 <$> paramProposalGovActionLifetime pp
    , bWord64 . unDbWord64 <$> paramProposalGovActionDeposit pp
    , bWord64 . unDbWord64 <$> paramProposalDrepDeposit pp
    , bWord64 . unDbWord64 <$> paramProposalDrepActivity pp
    , bDouble <$> paramProposalMinFeeRefScriptCostPerByte pp
    ]

encodeTreasuryWithdrawalCopy
  :: TreasuryWithdrawalId -> TreasuryWithdrawal -> ByteString
encodeTreasuryWithdrawalCopy (TreasuryWithdrawalId rid) tw =
  buildCopyRow
    [ Just $ bInt64 rid
    , Just $ bInt64 (getGovActionProposalId $ treasuryWithdrawalGovActionProposalId tw)
    , Just $ bInt64 (getStakeAddressId $ treasuryWithdrawalStakeAddressId tw)
    , Just $ bWord64 (unDbLovelace $ treasuryWithdrawalAmount tw)
    ]

encodeEventInfoCopy :: EventInfoId -> EventInfo -> ByteString
encodeEventInfoCopy (EventInfoId rid) ei =
  buildCopyRow
    [ Just $ bInt64 rid
    , bInt64 . getTxId <$> eventInfoTxId ei
    , Just $ bWord64 (eventInfoEpoch ei)
    , Just $ bText (eventInfoType ei)
    , bText <$> eventInfoExplanation ei
    ]

-- ---------------------------------------------------------------------------
-- * Hasql encoders / decoders
-- ---------------------------------------------------------------------------

-- DrepHash -----------------------------------------------------------------

drepHashEncoder :: E.Params DrepHash
drepHashEncoder = mconcat
  [ drepHashRaw       >$< E.param (E.nullable E.bytea)
  , drepHashView      >$< E.param (E.nonNullable E.text)
  , drepHashHasScript >$< E.param (E.nonNullable E.bool)
  ]

drepHashDecoder :: D.Row DrepHash
drepHashDecoder = DrepHash
  <$> D.column (D.nullable D.bytea)
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nonNullable D.bool)

entityDrepHashDecoder :: D.Row (DrepHashId, DrepHash)
entityDrepHashDecoder = (,)
  <$> idDecoder DrepHashId
  <*> drepHashDecoder

-- DrepRegistration ---------------------------------------------------------

drepRegistrationEncoder :: E.Params DrepRegistration
drepRegistrationEncoder = mconcat
  [ drepRegistrationTxId           >$< idEncoder getTxId
  , (fromIntegral :: Word16 -> Int64) . drepRegistrationCertIndex
                                   >$< E.param (E.nonNullable E.int8)
  , drepRegistrationDeposit        >$< E.param (E.nullable E.int8)
  , drepRegistrationDrepHashId     >$< idEncoder getDrepHashId
  , drepRegistrationVotingAnchorId >$< maybeIdEncoder getVotingAnchorId
  ]

drepRegistrationDecoder :: D.Row DrepRegistration
drepRegistrationDecoder = DrepRegistration
  <$> idDecoder TxId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nullable D.int8)
  <*> idDecoder DrepHashId
  <*> maybeIdDecoder VotingAnchorId

entityDrepRegistrationDecoder
  :: D.Row (DrepRegistrationId, DrepRegistration)
entityDrepRegistrationDecoder = (,)
  <$> idDecoder DrepRegistrationId
  <*> drepRegistrationDecoder

-- DrepDistr ----------------------------------------------------------------

drepDistrEncoder :: E.Params DrepDistr
drepDistrEncoder = mconcat
  [ drepDistrHashId      >$< idEncoder getDrepHashId
  , drepDistrAmount      >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , drepDistrEpochNo     >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , drepDistrActiveUntil >$< E.param (E.nullable $ fromIntegral >$< E.int8)
  ]

drepDistrDecoder :: D.Row DrepDistr
drepDistrDecoder = DrepDistr
  <$> idDecoder DrepHashId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))

entityDrepDistrDecoder :: D.Row (DrepDistrId, DrepDistr)
entityDrepDistrDecoder = (,)
  <$> idDecoder DrepDistrId
  <*> drepDistrDecoder

-- DelegationVote -----------------------------------------------------------

delegationVoteEncoder :: E.Params DelegationVote
delegationVoteEncoder = mconcat
  [ delegationVoteAddrId     >$< idEncoder getStakeAddressId
  , (fromIntegral :: Word16 -> Int64) . delegationVoteCertIndex
                             >$< E.param (E.nonNullable E.int8)
  , delegationVoteDrepHashId >$< idEncoder getDrepHashId
  , delegationVoteTxId       >$< idEncoder getTxId
  , delegationVoteRedeemerId >$< maybeIdEncoder getRedeemerId
  ]

delegationVoteDecoder :: D.Row DelegationVote
delegationVoteDecoder = DelegationVote
  <$> idDecoder StakeAddressId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> idDecoder DrepHashId
  <*> idDecoder TxId
  <*> maybeIdDecoder RedeemerId

entityDelegationVoteDecoder :: D.Row (DelegationVoteId, DelegationVote)
entityDelegationVoteDecoder = (,)
  <$> idDecoder DelegationVoteId
  <*> delegationVoteDecoder

-- GovActionProposal --------------------------------------------------------

govActionProposalEncoder :: E.Params GovActionProposal
govActionProposalEncoder = mconcat
  [ govActionProposalTxId                  >$< idEncoder getTxId
  , govActionProposalIndex                 >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , govActionProposalPrevGovActionProposal >$< maybeIdEncoder getGovActionProposalId
  , govActionProposalDeposit               >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  , govActionProposalReturnAddress         >$< idEncoder getStakeAddressId
  , govActionProposalExpiration            >$< E.param (E.nullable $ fromIntegral >$< E.int8)
  , govActionProposalVotingAnchorId        >$< maybeIdEncoder getVotingAnchorId
  , govActionProposalType                  >$< E.param (E.nonNullable govActionTypeEncoder)
  , govActionProposalDescription           >$< E.param (E.nonNullable E.text)
  , govActionProposalParamProposal         >$< maybeIdEncoder getParamProposalId
  , govActionProposalRatifiedEpoch         >$< E.param (E.nullable $ fromIntegral >$< E.int8)
  , govActionProposalEnactedEpoch          >$< E.param (E.nullable $ fromIntegral >$< E.int8)
  , govActionProposalDroppedEpoch          >$< E.param (E.nullable $ fromIntegral >$< E.int8)
  , govActionProposalExpiredEpoch          >$< E.param (E.nullable $ fromIntegral >$< E.int8)
  ]

govActionProposalDecoder :: D.Row GovActionProposal
govActionProposalDecoder = GovActionProposal
  <$> idDecoder TxId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> maybeIdDecoder GovActionProposalId
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)
  <*> idDecoder StakeAddressId
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))
  <*> maybeIdDecoder VotingAnchorId
  <*> D.column (D.nonNullable govActionTypeDecoder)
  <*> D.column (D.nonNullable D.text)
  <*> maybeIdDecoder ParamProposalId
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int8))

entityGovActionProposalDecoder
  :: D.Row (GovActionProposalId, GovActionProposal)
entityGovActionProposalDecoder = (,)
  <$> idDecoder GovActionProposalId
  <*> govActionProposalDecoder

-- VotingProcedure ----------------------------------------------------------

votingProcedureEncoder :: E.Params VotingProcedure
votingProcedureEncoder = mconcat
  [ votingProcedureTxId                >$< idEncoder getTxId
  , (fromIntegral :: Word16 -> Int64) . votingProcedureIndex
                                       >$< E.param (E.nonNullable E.int8)
  , votingProcedureGovActionProposalId >$< idEncoder getGovActionProposalId
  , votingProcedureVoterRole           >$< E.param (E.nonNullable voterRoleEncoder)
  , votingProcedureDrepVoter           >$< maybeIdEncoder getDrepHashId
  , votingProcedurePoolVoter           >$< maybeIdEncoder getPoolHashId
  , votingProcedureVote                >$< E.param (E.nonNullable voteEncoder)
  , votingProcedureVotingAnchorId      >$< maybeIdEncoder getVotingAnchorId
  , votingProcedureCommitteeVoter      >$< maybeIdEncoder getCommitteeHashId
  , votingProcedureInvalid             >$< maybeIdEncoder getEventInfoId
  ]

votingProcedureDecoder :: D.Row VotingProcedure
votingProcedureDecoder = VotingProcedure
  <$> idDecoder TxId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> idDecoder GovActionProposalId
  <*> D.column (D.nonNullable voterRoleDecoder)
  <*> maybeIdDecoder DrepHashId
  <*> maybeIdDecoder PoolHashId
  <*> D.column (D.nonNullable voteDecoder)
  <*> maybeIdDecoder VotingAnchorId
  <*> maybeIdDecoder CommitteeHashId
  <*> maybeIdDecoder EventInfoId

entityVotingProcedureDecoder
  :: D.Row (VotingProcedureId, VotingProcedure)
entityVotingProcedureDecoder = (,)
  <$> idDecoder VotingProcedureId
  <*> votingProcedureDecoder

-- VotingAnchor -------------------------------------------------------------

votingAnchorEncoder :: E.Params VotingAnchor
votingAnchorEncoder = mconcat
  [ votingAnchorUrl      >$< E.param (E.nonNullable voteUrlEncoder)
  , votingAnchorDataHash >$< E.param (E.nonNullable E.bytea)
  , votingAnchorType     >$< E.param (E.nonNullable anchorTypeEncoder)
  , votingAnchorBlockId  >$< idEncoder getBlockId
  ]

votingAnchorDecoder :: D.Row VotingAnchor
votingAnchorDecoder = VotingAnchor
  <$> D.column (D.nonNullable voteUrlDecoder)
  <*> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable anchorTypeDecoder)
  <*> idDecoder BlockId

entityVotingAnchorDecoder :: D.Row (VotingAnchorId, VotingAnchor)
entityVotingAnchorDecoder = (,)
  <$> idDecoder VotingAnchorId
  <*> votingAnchorDecoder

-- Constitution -------------------------------------------------------------

constitutionEncoder :: E.Params Constitution
constitutionEncoder = mconcat
  [ constitutionGovActionProposalId >$< maybeIdEncoder getGovActionProposalId
  , constitutionVotingAnchorId      >$< idEncoder getVotingAnchorId
  , constitutionScriptHash          >$< E.param (E.nullable E.bytea)
  ]

constitutionDecoder :: D.Row Constitution
constitutionDecoder = Constitution
  <$> maybeIdDecoder GovActionProposalId
  <*> idDecoder VotingAnchorId
  <*> D.column (D.nullable D.bytea)

entityConstitutionDecoder :: D.Row (ConstitutionId, Constitution)
entityConstitutionDecoder = (,)
  <$> idDecoder ConstitutionId
  <*> constitutionDecoder

-- Committee ----------------------------------------------------------------

committeeEncoder :: E.Params Committee
committeeEncoder = mconcat
  [ committeeGovActionProposalId >$< maybeIdEncoder getGovActionProposalId
  , committeeQuorumNumerator     >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , committeeQuorumDenominator   >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  ]

committeeDecoder :: D.Row Committee
committeeDecoder = Committee
  <$> maybeIdDecoder GovActionProposalId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityCommitteeDecoder :: D.Row (CommitteeId, Committee)
entityCommitteeDecoder = (,)
  <$> idDecoder CommitteeId
  <*> committeeDecoder

-- CommitteeHash ------------------------------------------------------------

committeeHashEncoder :: E.Params CommitteeHash
committeeHashEncoder = mconcat
  [ committeeHashRaw       >$< E.param (E.nonNullable E.bytea)
  , committeeHashHasScript >$< E.param (E.nonNullable E.bool)
  ]

committeeHashDecoder :: D.Row CommitteeHash
committeeHashDecoder = CommitteeHash
  <$> D.column (D.nonNullable D.bytea)
  <*> D.column (D.nonNullable D.bool)

entityCommitteeHashDecoder :: D.Row (CommitteeHashId, CommitteeHash)
entityCommitteeHashDecoder = (,)
  <$> idDecoder CommitteeHashId
  <*> committeeHashDecoder

-- CommitteeMember ----------------------------------------------------------

committeeMemberEncoder :: E.Params CommitteeMember
committeeMemberEncoder = mconcat
  [ committeeMemberCommitteeId     >$< idEncoder getCommitteeId
  , committeeMemberCommitteeHashId >$< idEncoder getCommitteeHashId
  , committeeMemberExpirationEpoch >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  ]

committeeMemberDecoder :: D.Row CommitteeMember
committeeMemberDecoder = CommitteeMember
  <$> idDecoder CommitteeId
  <*> idDecoder CommitteeHashId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))

entityCommitteeMemberDecoder
  :: D.Row (CommitteeMemberId, CommitteeMember)
entityCommitteeMemberDecoder = (,)
  <$> idDecoder CommitteeMemberId
  <*> committeeMemberDecoder

-- CommitteeRegistration ----------------------------------------------------

committeeRegistrationEncoder :: E.Params CommitteeRegistration
committeeRegistrationEncoder = mconcat
  [ committeeRegistrationTxId      >$< idEncoder getTxId
  , (fromIntegral :: Word16 -> Int64) . committeeRegistrationCertIndex
                                   >$< E.param (E.nonNullable E.int8)
  , committeeRegistrationColdKeyId >$< idEncoder getCommitteeHashId
  , committeeRegistrationHotKeyId  >$< idEncoder getCommitteeHashId
  ]

committeeRegistrationDecoder :: D.Row CommitteeRegistration
committeeRegistrationDecoder = CommitteeRegistration
  <$> idDecoder TxId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> idDecoder CommitteeHashId
  <*> idDecoder CommitteeHashId

entityCommitteeRegistrationDecoder
  :: D.Row (CommitteeRegistrationId, CommitteeRegistration)
entityCommitteeRegistrationDecoder = (,)
  <$> idDecoder CommitteeRegistrationId
  <*> committeeRegistrationDecoder

-- CommitteeDeRegistration --------------------------------------------------

committeeDeRegistrationEncoder :: E.Params CommitteeDeRegistration
committeeDeRegistrationEncoder = mconcat
  [ committeeDeRegistrationTxId           >$< idEncoder getTxId
  , (fromIntegral :: Word16 -> Int64) . committeeDeRegistrationCertIndex
                                          >$< E.param (E.nonNullable E.int8)
  , committeeDeRegistrationVotingAnchorId >$< maybeIdEncoder getVotingAnchorId
  , committeeDeRegistrationColdKeyId      >$< idEncoder getCommitteeHashId
  ]

committeeDeRegistrationDecoder :: D.Row CommitteeDeRegistration
committeeDeRegistrationDecoder = CommitteeDeRegistration
  <$> idDecoder TxId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> maybeIdDecoder VotingAnchorId
  <*> idDecoder CommitteeHashId

entityCommitteeDeRegistrationDecoder
  :: D.Row (CommitteeDeRegistrationId, CommitteeDeRegistration)
entityCommitteeDeRegistrationDecoder = (,)
  <$> idDecoder CommitteeDeRegistrationId
  <*> committeeDeRegistrationDecoder

-- ParamProposal ------------------------------------------------------------

paramProposalEncoder :: E.Params ParamProposal
paramProposalEncoder = mconcat
  [ paramProposalEpochNo                    >$< E.param (E.nullable $ fromIntegral >$< E.int8)
  , paramProposalKey                        >$< E.param (E.nullable E.bytea)
  , paramProposalMinFeeA                    >$< maybeDbWord64Encoder
  , paramProposalMinFeeB                    >$< maybeDbWord64Encoder
  , paramProposalMaxBlockSize               >$< maybeDbWord64Encoder
  , paramProposalMaxTxSize                  >$< maybeDbWord64Encoder
  , paramProposalMaxBhSize                  >$< maybeDbWord64Encoder
  , paramProposalKeyDeposit                 >$< maybeDbLovelaceEncoder
  , paramProposalPoolDeposit                >$< maybeDbLovelaceEncoder
  , paramProposalMaxEpoch                   >$< maybeDbWord64Encoder
  , paramProposalOptimalPoolCount           >$< maybeDbWord64Encoder
  , paramProposalInfluence                  >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalMonetaryExpandRate         >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalTreasuryGrowthRate         >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDecentralisation           >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalEntropy                    >$< E.param (E.nullable E.bytea)
  , (fmap fromIntegral :: Maybe Word16 -> Maybe Int16) . paramProposalProtocolMajor
                                            >$< E.param (E.nullable E.int2)
  , (fmap fromIntegral :: Maybe Word16 -> Maybe Int16) . paramProposalProtocolMinor
                                            >$< E.param (E.nullable E.int2)
  , paramProposalMinUtxoValue               >$< maybeDbLovelaceEncoder
  , paramProposalMinPoolCost                >$< maybeDbLovelaceEncoder
  , paramProposalCostModelId                >$< maybeIdEncoder getCostModelId
  , paramProposalPriceMem                   >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalPriceStep                  >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalMaxTxExMem                 >$< maybeDbWord64Encoder
  , paramProposalMaxTxExSteps               >$< maybeDbWord64Encoder
  , paramProposalMaxBlockExMem              >$< maybeDbWord64Encoder
  , paramProposalMaxBlockExSteps            >$< maybeDbWord64Encoder
  , paramProposalMaxValSize                 >$< maybeDbWord64Encoder
  , (fmap fromIntegral :: Maybe Word16 -> Maybe Int16) . paramProposalCollateralPercent
                                            >$< E.param (E.nullable E.int2)
  , (fmap fromIntegral :: Maybe Word16 -> Maybe Int16) . paramProposalMaxCollateralInputs
                                            >$< E.param (E.nullable E.int2)
  , paramProposalRegisteredTxId             >$< idEncoder getTxId
  , paramProposalCoinsPerUtxoSize           >$< maybeDbLovelaceEncoder
  , paramProposalPvtMotionNoConfidence      >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalPvtCommitteeNormal         >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalPvtCommitteeNoConfidence   >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalPvtHardForkInitiation      >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalPvtppSecurityGroup         >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtMotionNoConfidence      >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtCommitteeNormal         >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtCommitteeNoConfidence   >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtUpdateToConstitution    >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtHardForkInitiation      >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtPPNetworkGroup          >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtPPEconomicGroup         >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtPPTechnicalGroup        >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtPPGovGroup              >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalDvtTreasuryWithdrawal      >$< E.param (E.nullable doubleAsTextEncoder)
  , paramProposalCommitteeMinSize           >$< maybeDbWord64Encoder
  , paramProposalCommitteeMaxTermLength     >$< maybeDbWord64Encoder
  , paramProposalGovActionLifetime          >$< maybeDbWord64Encoder
  , paramProposalGovActionDeposit           >$< maybeDbWord64Encoder
  , paramProposalDrepDeposit                >$< maybeDbWord64Encoder
  , paramProposalDrepActivity               >$< maybeDbWord64Encoder
  , paramProposalMinFeeRefScriptCostPerByte >$< E.param (E.nullable doubleAsTextEncoder)
  ]

paramProposalDecoder :: D.Row ParamProposal
paramProposalDecoder = ParamProposal
  <$> (fmap fromIntegral <$> D.column (D.nullable D.int8))
  <*> D.column (D.nullable D.bytea)
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbLovelaceDecoder
  <*> maybeDbLovelaceDecoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable D.bytea)
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> maybeDbLovelaceDecoder
  <*> maybeDbLovelaceDecoder
  <*> maybeIdDecoder CostModelId
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> idDecoder TxId
  <*> maybeDbLovelaceDecoder
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> D.column (D.nullable doubleAsTextDecoder)
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> D.column (D.nullable doubleAsTextDecoder)

entityParamProposalDecoder :: D.Row (ParamProposalId, ParamProposal)
entityParamProposalDecoder = (,)
  <$> idDecoder ParamProposalId
  <*> paramProposalDecoder

-- TreasuryWithdrawal -------------------------------------------------------

treasuryWithdrawalEncoder :: E.Params TreasuryWithdrawal
treasuryWithdrawalEncoder = mconcat
  [ treasuryWithdrawalGovActionProposalId >$< idEncoder getGovActionProposalId
  , treasuryWithdrawalStakeAddressId      >$< idEncoder getStakeAddressId
  , treasuryWithdrawalAmount              >$< E.param (E.nonNullable dbLovelaceValueEncoder)
  ]

treasuryWithdrawalDecoder :: D.Row TreasuryWithdrawal
treasuryWithdrawalDecoder = TreasuryWithdrawal
  <$> idDecoder GovActionProposalId
  <*> idDecoder StakeAddressId
  <*> D.column (D.nonNullable dbLovelaceValueDecoder)

entityTreasuryWithdrawalDecoder
  :: D.Row (TreasuryWithdrawalId, TreasuryWithdrawal)
entityTreasuryWithdrawalDecoder = (,)
  <$> idDecoder TreasuryWithdrawalId
  <*> treasuryWithdrawalDecoder

-- EventInfo ----------------------------------------------------------------

eventInfoEncoder :: E.Params EventInfo
eventInfoEncoder = mconcat
  [ eventInfoTxId        >$< maybeIdEncoder getTxId
  , eventInfoEpoch       >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
  , eventInfoType        >$< E.param (E.nonNullable E.text)
  , eventInfoExplanation >$< E.param (E.nullable E.text)
  ]

eventInfoDecoder :: D.Row EventInfo
eventInfoDecoder = EventInfo
  <$> maybeIdDecoder TxId
  <*> (fromIntegral <$> D.column (D.nonNullable D.int8))
  <*> D.column (D.nonNullable D.text)
  <*> D.column (D.nullable D.text)

entityEventInfoDecoder :: D.Row (EventInfoId, EventInfo)
entityEventInfoDecoder = (,)
  <$> idDecoder EventInfoId
  <*> eventInfoDecoder

-- ---------------------------------------------------------------------------
-- * Internal: Double encoding via TEXT
-- ---------------------------------------------------------------------------
--
-- @param_proposal@ has 22 'Double' columns. The codebase already
-- carries the same TEXT-stored-double pattern in @pool_update.margin@
-- and @epoch_sync_stats@; replicating it here keeps the migration
-- path straightforward. A future cleanup can promote these helpers
-- to a shared module and switch the column type to PG @float8@.

bDouble :: Double -> Builder
bDouble = byteString . BS8.pack . show

doubleAsTextEncoder :: E.Value Double
doubleAsTextEncoder = T.pack . show >$< E.text

doubleAsTextDecoder :: D.Value Double
doubleAsTextDecoder = D.refine parseDouble D.text
  where
    parseDouble t = case TR.readMaybe (T.unpack t) of
      Just d  -> Right d
      Nothing -> Left $ "could not parse double: " <> t
