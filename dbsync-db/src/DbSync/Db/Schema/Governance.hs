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
-- which is owned by the @epoch_boundary@ extractor; the column stays
-- as a nullable @BIGINT@ here so the schema compiles in isolation.
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

    -- * Column records (compile-time-safe column references)
  , DrepHashCols (..), drepHashCols, drepHashColsList
  , DrepRegistrationCols (..), drepRegistrationCols, drepRegistrationColsList
  , DrepDistrCols (..), drepDistrCols, drepDistrColsList
  , DelegationVoteCols (..), delegationVoteCols, delegationVoteColsList
  , GovActionProposalCols (..), govActionProposalCols, govActionProposalColsList
  , VotingProcedureCols (..), votingProcedureCols, votingProcedureColsList
  , VotingAnchorCols (..), votingAnchorCols, votingAnchorColsList
  , ConstitutionCols (..), constitutionCols, constitutionColsList
  , CommitteeCols (..), committeeCols, committeeColsList
  , CommitteeHashCols (..), committeeHashCols, committeeHashColsList
  , CommitteeMemberCols (..), committeeMemberCols, committeeMemberColsList
  , CommitteeRegistrationCols (..), committeeRegistrationCols, committeeRegistrationColsList
  , CommitteeDeRegistrationCols (..), committeeDeRegistrationCols, committeeDeRegistrationColsList
  , ParamProposalCols (..), paramProposalCols, paramProposalColsList
  , TreasuryWithdrawalCols (..), treasuryWithdrawalCols, treasuryWithdrawalColsList
  , EventInfoCols (..), eventInfoCols, eventInfoColsList

    -- * Per-module column-record registry
  , governanceColumnRecords

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

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

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
  , bRational
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
  , rationalAsNumericDecoder
  , rationalAsNumericEncoder
  , voteDecoder
  , voteEncoder
  , voterRoleDecoder
  , voterRoleEncoder
  , voteUrlDecoder
  , voteUrlEncoder
  )
import DbSync.Db.Loader.Encoder
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
-- @voter_role@.
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
-- overrides. Most columns are nullable; only those the proposer chose
-- to change are populated.
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
  , paramProposalInfluence                  :: !(Maybe Rational)
  , paramProposalMonetaryExpandRate         :: !(Maybe Rational)
  , paramProposalTreasuryGrowthRate         :: !(Maybe Rational)
  , paramProposalDecentralisation           :: !(Maybe Rational)
  , paramProposalEntropy                    :: !(Maybe ByteString)
  , paramProposalProtocolMajor              :: !(Maybe Word16)
  , paramProposalProtocolMinor              :: !(Maybe Word16)
  , paramProposalMinUtxoValue               :: !(Maybe DbLovelace)
  , paramProposalMinPoolCost                :: !(Maybe DbLovelace)
  , paramProposalCostModelId                :: !(Maybe CostModelId)
  , paramProposalPriceMem                   :: !(Maybe Rational)
  , paramProposalPriceStep                  :: !(Maybe Rational)
  , paramProposalMaxTxExMem                 :: !(Maybe DbWord64)
  , paramProposalMaxTxExSteps               :: !(Maybe DbWord64)
  , paramProposalMaxBlockExMem              :: !(Maybe DbWord64)
  , paramProposalMaxBlockExSteps            :: !(Maybe DbWord64)
  , paramProposalMaxValSize                 :: !(Maybe DbWord64)
  , paramProposalCollateralPercent          :: !(Maybe Word16)
  , paramProposalMaxCollateralInputs        :: !(Maybe Word16)
  , paramProposalRegisteredTxId             :: !TxId
  , paramProposalCoinsPerUtxoSize           :: !(Maybe DbLovelace)
  , paramProposalPvtMotionNoConfidence      :: !(Maybe Rational)
  , paramProposalPvtCommitteeNormal         :: !(Maybe Rational)
  , paramProposalPvtCommitteeNoConfidence   :: !(Maybe Rational)
  , paramProposalPvtHardForkInitiation      :: !(Maybe Rational)
  , paramProposalPvtppSecurityGroup         :: !(Maybe Rational)
  , paramProposalDvtMotionNoConfidence      :: !(Maybe Rational)
  , paramProposalDvtCommitteeNormal         :: !(Maybe Rational)
  , paramProposalDvtCommitteeNoConfidence   :: !(Maybe Rational)
  , paramProposalDvtUpdateToConstitution    :: !(Maybe Rational)
  , paramProposalDvtHardForkInitiation      :: !(Maybe Rational)
  , paramProposalDvtPPNetworkGroup          :: !(Maybe Rational)
  , paramProposalDvtPPEconomicGroup         :: !(Maybe Rational)
  , paramProposalDvtPPTechnicalGroup        :: !(Maybe Rational)
  , paramProposalDvtPPGovGroup              :: !(Maybe Rational)
  , paramProposalDvtTreasuryWithdrawal      :: !(Maybe Rational)
  , paramProposalCommitteeMinSize           :: !(Maybe DbWord64)
  , paramProposalCommitteeMaxTermLength     :: !(Maybe DbWord64)
  , paramProposalGovActionLifetime          :: !(Maybe DbWord64)
  , paramProposalGovActionDeposit           :: !(Maybe DbWord64)
  , paramProposalDrepDeposit                :: !(Maybe DbWord64)
  , paramProposalDrepActivity               :: !(Maybe DbWord64)
  , paramProposalMinFeeRefScriptCostPerByte :: !(Maybe Rational)
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
-- @type@ is plain text, not an enum.
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
  , tdIdentityColumns = []
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
  , tdIdentityColumns = ["id"]
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
  , tdIdentityColumns = ["id"]
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
  , tdIdentityColumns = ["id"]
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
  , tdIdentityColumns = []
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
  , tdIdentityColumns = ["id"]
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
  , tdIdentityColumns = []
  , tdForeignKeys = []
  }

-- | FK-referenced by @epoch_state.constitution_id@, so 'constitution.id'
-- is allocated from an in-process counter and the row carries it explicitly.
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
  , tdIdentityColumns = []
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
  , tdIdentityColumns = []
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
  , tdIdentityColumns = []
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
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = [ForeignKey "committee_id" "committee" "id"]
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
  , tdIdentityColumns = ["id"]
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
  , tdIdentityColumns = ["id"]
  , tdForeignKeys = []
  }

-- | 53-column @param_proposal@. Rational parameters ride @numeric@
-- columns, matching @epoch_param@ / @pool_update.margin@.
-- @committee_max_term_length@ uses the @numeric@ shape shared by every
-- other @DbWord64@ column.
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
      , ColumnDef "influence"                     PgNumeric  True
      , ColumnDef "monetary_expand_rate"          PgNumeric  True
      , ColumnDef "treasury_growth_rate"          PgNumeric  True
      , ColumnDef "decentralisation"              PgNumeric  True
      , ColumnDef "entropy"                       PgBytea    True
      , ColumnDef "protocol_major"                PgSmallInt True
      , ColumnDef "protocol_minor"                PgSmallInt True
      , ColumnDef "min_utxo_value"                PgNumeric  True
      , ColumnDef "min_pool_cost"                 PgNumeric  True
      , ColumnDef "cost_model_id"                 PgBigInt   True
      , ColumnDef "price_mem"                     PgNumeric  True
      , ColumnDef "price_step"                    PgNumeric  True
      , ColumnDef "max_tx_ex_mem"                 PgNumeric  True
      , ColumnDef "max_tx_ex_steps"               PgNumeric  True
      , ColumnDef "max_block_ex_mem"              PgNumeric  True
      , ColumnDef "max_block_ex_steps"            PgNumeric  True
      , ColumnDef "max_val_size"                  PgNumeric  True
      , ColumnDef "collateral_percent"            PgSmallInt True
      , ColumnDef "max_collateral_inputs"         PgSmallInt True
      , ColumnDef "registered_tx_id"              PgBigInt   False
      , ColumnDef "coins_per_utxo_size"           PgNumeric  True
      , ColumnDef "pvt_motion_no_confidence"      PgNumeric  True
      , ColumnDef "pvt_committee_normal"          PgNumeric  True
      , ColumnDef "pvt_committee_no_confidence"   PgNumeric  True
      , ColumnDef "pvt_hard_fork_initiation"      PgNumeric  True
      , ColumnDef "pvtpp_security_group"          PgNumeric  True
      , ColumnDef "dvt_motion_no_confidence"      PgNumeric  True
      , ColumnDef "dvt_committee_normal"          PgNumeric  True
      , ColumnDef "dvt_committee_no_confidence"   PgNumeric  True
      , ColumnDef "dvt_update_to_constitution"    PgNumeric  True
      , ColumnDef "dvt_hard_fork_initiation"      PgNumeric  True
      , ColumnDef "dvt_pp_network_group"          PgNumeric  True
      , ColumnDef "dvt_pp_economic_group"         PgNumeric  True
      , ColumnDef "dvt_pp_technical_group"        PgNumeric  True
      , ColumnDef "dvt_pp_gov_group"              PgNumeric  True
      , ColumnDef "dvt_treasury_withdrawal"       PgNumeric  True
      , ColumnDef "committee_min_size"            PgNumeric  True
      , ColumnDef "committee_max_term_length"     PgNumeric  True
      , ColumnDef "gov_action_lifetime"           PgNumeric  True
      , ColumnDef "gov_action_deposit"            PgNumeric  True
      , ColumnDef "drep_deposit"                  PgNumeric  True
      , ColumnDef "drep_activity"                 PgNumeric  True
      , ColumnDef "min_fee_ref_script_cost_per_byte" PgNumeric True
      ]
  , tdMode = TableUnlogged
  , tdPrimaryKey        = Nothing
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdIdentityColumns = []
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
  , tdIdentityColumns = ["id"]
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
  , tdIdentityColumns = []
  , tdForeignKeys = []
  }

-- ---------------------------------------------------------------------------
-- * Column records
-- ---------------------------------------------------------------------------

data DrepHashCols = DrepHashCols
  { dhcId        :: !TableColumn
  , dhcRaw       :: !TableColumn
  , dhcView      :: !TableColumn
  , dhcHasScript :: !TableColumn
  }

drepHashCols :: DrepHashCols
drepHashCols =
  let c = TableColumn drepHashTableDef
  in DrepHashCols
       { dhcId        = c "id"
       , dhcRaw       = c "raw"
       , dhcView      = c "view"
       , dhcHasScript = c "has_script"
       }

drepHashColsList :: [TableColumn]
drepHashColsList =
  [ drepHashCols.dhcId
  , drepHashCols.dhcRaw
  , drepHashCols.dhcView
  , drepHashCols.dhcHasScript
  ]

data DrepRegistrationCols = DrepRegistrationCols
  { drcId             :: !TableColumn
  , drcTxId           :: !TableColumn
  , drcCertIndex      :: !TableColumn
  , drcDeposit        :: !TableColumn
  , drcDrepHashId     :: !TableColumn
  , drcVotingAnchorId :: !TableColumn
  }

drepRegistrationCols :: DrepRegistrationCols
drepRegistrationCols =
  let c = TableColumn drepRegistrationTableDef
  in DrepRegistrationCols
       { drcId             = c "id"
       , drcTxId           = c "tx_id"
       , drcCertIndex      = c "cert_index"
       , drcDeposit        = c "deposit"
       , drcDrepHashId     = c "drep_hash_id"
       , drcVotingAnchorId = c "voting_anchor_id"
       }

drepRegistrationColsList :: [TableColumn]
drepRegistrationColsList =
  [ drepRegistrationCols.drcId
  , drepRegistrationCols.drcTxId
  , drepRegistrationCols.drcCertIndex
  , drepRegistrationCols.drcDeposit
  , drepRegistrationCols.drcDrepHashId
  , drepRegistrationCols.drcVotingAnchorId
  ]

data DrepDistrCols = DrepDistrCols
  { ddcId          :: !TableColumn
  , ddcHashId      :: !TableColumn
  , ddcAmount      :: !TableColumn
  , ddcEpochNo     :: !TableColumn
  , ddcActiveUntil :: !TableColumn
  }

drepDistrCols :: DrepDistrCols
drepDistrCols =
  let c = TableColumn drepDistrTableDef
  in DrepDistrCols
       { ddcId          = c "id"
       , ddcHashId      = c "hash_id"
       , ddcAmount      = c "amount"
       , ddcEpochNo     = c "epoch_no"
       , ddcActiveUntil = c "active_until"
       }

drepDistrColsList :: [TableColumn]
drepDistrColsList =
  [ drepDistrCols.ddcId
  , drepDistrCols.ddcHashId
  , drepDistrCols.ddcAmount
  , drepDistrCols.ddcEpochNo
  , drepDistrCols.ddcActiveUntil
  ]

data DelegationVoteCols = DelegationVoteCols
  { dvcId         :: !TableColumn
  , dvcAddrId     :: !TableColumn
  , dvcCertIndex  :: !TableColumn
  , dvcDrepHashId :: !TableColumn
  , dvcTxId       :: !TableColumn
  , dvcRedeemerId :: !TableColumn
  }

delegationVoteCols :: DelegationVoteCols
delegationVoteCols =
  let c = TableColumn delegationVoteTableDef
  in DelegationVoteCols
       { dvcId         = c "id"
       , dvcAddrId     = c "addr_id"
       , dvcCertIndex  = c "cert_index"
       , dvcDrepHashId = c "drep_hash_id"
       , dvcTxId       = c "tx_id"
       , dvcRedeemerId = c "redeemer_id"
       }

delegationVoteColsList :: [TableColumn]
delegationVoteColsList =
  [ delegationVoteCols.dvcId
  , delegationVoteCols.dvcAddrId
  , delegationVoteCols.dvcCertIndex
  , delegationVoteCols.dvcDrepHashId
  , delegationVoteCols.dvcTxId
  , delegationVoteCols.dvcRedeemerId
  ]

data GovActionProposalCols = GovActionProposalCols
  { gapcId                     :: !TableColumn
  , gapcTxId                   :: !TableColumn
  , gapcIndex                  :: !TableColumn
  , gapcPrevGovActionProposal  :: !TableColumn
  , gapcDeposit                :: !TableColumn
  , gapcReturnAddress          :: !TableColumn
  , gapcExpiration             :: !TableColumn
  , gapcVotingAnchorId         :: !TableColumn
  , gapcType                   :: !TableColumn
  , gapcDescription            :: !TableColumn
  , gapcParamProposal          :: !TableColumn
  , gapcRatifiedEpoch          :: !TableColumn
  , gapcEnactedEpoch           :: !TableColumn
  , gapcDroppedEpoch           :: !TableColumn
  , gapcExpiredEpoch           :: !TableColumn
  }

govActionProposalCols :: GovActionProposalCols
govActionProposalCols =
  let c = TableColumn govActionProposalTableDef
  in GovActionProposalCols
       { gapcId                    = c "id"
       , gapcTxId                  = c "tx_id"
       , gapcIndex                 = c "index"
       , gapcPrevGovActionProposal = c "prev_gov_action_proposal"
       , gapcDeposit               = c "deposit"
       , gapcReturnAddress         = c "return_address"
       , gapcExpiration            = c "expiration"
       , gapcVotingAnchorId        = c "voting_anchor_id"
       , gapcType                  = c "type"
       , gapcDescription           = c "description"
       , gapcParamProposal         = c "param_proposal"
       , gapcRatifiedEpoch         = c "ratified_epoch"
       , gapcEnactedEpoch          = c "enacted_epoch"
       , gapcDroppedEpoch          = c "dropped_epoch"
       , gapcExpiredEpoch          = c "expired_epoch"
       }

govActionProposalColsList :: [TableColumn]
govActionProposalColsList =
  [ govActionProposalCols.gapcId
  , govActionProposalCols.gapcTxId
  , govActionProposalCols.gapcIndex
  , govActionProposalCols.gapcPrevGovActionProposal
  , govActionProposalCols.gapcDeposit
  , govActionProposalCols.gapcReturnAddress
  , govActionProposalCols.gapcExpiration
  , govActionProposalCols.gapcVotingAnchorId
  , govActionProposalCols.gapcType
  , govActionProposalCols.gapcDescription
  , govActionProposalCols.gapcParamProposal
  , govActionProposalCols.gapcRatifiedEpoch
  , govActionProposalCols.gapcEnactedEpoch
  , govActionProposalCols.gapcDroppedEpoch
  , govActionProposalCols.gapcExpiredEpoch
  ]

data VotingProcedureCols = VotingProcedureCols
  { vpcId                  :: !TableColumn
  , vpcTxId                :: !TableColumn
  , vpcIndex               :: !TableColumn
  , vpcGovActionProposalId :: !TableColumn
  , vpcVoterRole           :: !TableColumn
  , vpcDrepVoter           :: !TableColumn
  , vpcPoolVoter           :: !TableColumn
  , vpcVote                :: !TableColumn
  , vpcVotingAnchorId      :: !TableColumn
  , vpcCommitteeVoter      :: !TableColumn
  , vpcInvalid             :: !TableColumn
  }

votingProcedureCols :: VotingProcedureCols
votingProcedureCols =
  let c = TableColumn votingProcedureTableDef
  in VotingProcedureCols
       { vpcId                  = c "id"
       , vpcTxId                = c "tx_id"
       , vpcIndex               = c "index"
       , vpcGovActionProposalId = c "gov_action_proposal_id"
       , vpcVoterRole           = c "voter_role"
       , vpcDrepVoter           = c "drep_voter"
       , vpcPoolVoter           = c "pool_voter"
       , vpcVote                = c "vote"
       , vpcVotingAnchorId      = c "voting_anchor_id"
       , vpcCommitteeVoter      = c "committee_voter"
       , vpcInvalid             = c "invalid"
       }

votingProcedureColsList :: [TableColumn]
votingProcedureColsList =
  [ votingProcedureCols.vpcId
  , votingProcedureCols.vpcTxId
  , votingProcedureCols.vpcIndex
  , votingProcedureCols.vpcGovActionProposalId
  , votingProcedureCols.vpcVoterRole
  , votingProcedureCols.vpcDrepVoter
  , votingProcedureCols.vpcPoolVoter
  , votingProcedureCols.vpcVote
  , votingProcedureCols.vpcVotingAnchorId
  , votingProcedureCols.vpcCommitteeVoter
  , votingProcedureCols.vpcInvalid
  ]

data VotingAnchorCols = VotingAnchorCols
  { vacId       :: !TableColumn
  , vacUrl      :: !TableColumn
  , vacDataHash :: !TableColumn
  , vacType     :: !TableColumn
  , vacBlockId  :: !TableColumn
  }

votingAnchorCols :: VotingAnchorCols
votingAnchorCols =
  let c = TableColumn votingAnchorTableDef
  in VotingAnchorCols
       { vacId       = c "id"
       , vacUrl      = c "url"
       , vacDataHash = c "data_hash"
       , vacType     = c "type"
       , vacBlockId  = c "block_id"
       }

votingAnchorColsList :: [TableColumn]
votingAnchorColsList =
  [ votingAnchorCols.vacId
  , votingAnchorCols.vacUrl
  , votingAnchorCols.vacDataHash
  , votingAnchorCols.vacType
  , votingAnchorCols.vacBlockId
  ]

data ConstitutionCols = ConstitutionCols
  { cccId                  :: !TableColumn
  , cccGovActionProposalId :: !TableColumn
  , cccVotingAnchorId      :: !TableColumn
  , cccScriptHash          :: !TableColumn
  }

constitutionCols :: ConstitutionCols
constitutionCols =
  let c = TableColumn constitutionTableDef
  in ConstitutionCols
       { cccId                  = c "id"
       , cccGovActionProposalId = c "gov_action_proposal_id"
       , cccVotingAnchorId      = c "voting_anchor_id"
       , cccScriptHash          = c "script_hash"
       }

constitutionColsList :: [TableColumn]
constitutionColsList =
  [ constitutionCols.cccId
  , constitutionCols.cccGovActionProposalId
  , constitutionCols.cccVotingAnchorId
  , constitutionCols.cccScriptHash
  ]

data CommitteeCols = CommitteeCols
  { cmtcId                  :: !TableColumn
  , cmtcGovActionProposalId :: !TableColumn
  , cmtcQuorumNumerator     :: !TableColumn
  , cmtcQuorumDenominator   :: !TableColumn
  }

committeeCols :: CommitteeCols
committeeCols =
  let c = TableColumn committeeTableDef
  in CommitteeCols
       { cmtcId                  = c "id"
       , cmtcGovActionProposalId = c "gov_action_proposal_id"
       , cmtcQuorumNumerator     = c "quorum_numerator"
       , cmtcQuorumDenominator   = c "quorum_denominator"
       }

committeeColsList :: [TableColumn]
committeeColsList =
  [ committeeCols.cmtcId
  , committeeCols.cmtcGovActionProposalId
  , committeeCols.cmtcQuorumNumerator
  , committeeCols.cmtcQuorumDenominator
  ]

data CommitteeHashCols = CommitteeHashCols
  { chcId        :: !TableColumn
  , chcRaw       :: !TableColumn
  , chcHasScript :: !TableColumn
  }

committeeHashCols :: CommitteeHashCols
committeeHashCols =
  let c = TableColumn committeeHashTableDef
  in CommitteeHashCols
       { chcId        = c "id"
       , chcRaw       = c "raw"
       , chcHasScript = c "has_script"
       }

committeeHashColsList :: [TableColumn]
committeeHashColsList =
  [ committeeHashCols.chcId
  , committeeHashCols.chcRaw
  , committeeHashCols.chcHasScript
  ]

data CommitteeMemberCols = CommitteeMemberCols
  { cmemcId              :: !TableColumn
  , cmemcCommitteeId     :: !TableColumn
  , cmemcCommitteeHashId :: !TableColumn
  , cmemcExpirationEpoch :: !TableColumn
  }

committeeMemberCols :: CommitteeMemberCols
committeeMemberCols =
  let c = TableColumn committeeMemberTableDef
  in CommitteeMemberCols
       { cmemcId              = c "id"
       , cmemcCommitteeId     = c "committee_id"
       , cmemcCommitteeHashId = c "committee_hash_id"
       , cmemcExpirationEpoch = c "expiration_epoch"
       }

committeeMemberColsList :: [TableColumn]
committeeMemberColsList =
  [ committeeMemberCols.cmemcId
  , committeeMemberCols.cmemcCommitteeId
  , committeeMemberCols.cmemcCommitteeHashId
  , committeeMemberCols.cmemcExpirationEpoch
  ]

data CommitteeRegistrationCols = CommitteeRegistrationCols
  { crcId        :: !TableColumn
  , crcTxId      :: !TableColumn
  , crcCertIndex :: !TableColumn
  , crcColdKeyId :: !TableColumn
  , crcHotKeyId  :: !TableColumn
  }

committeeRegistrationCols :: CommitteeRegistrationCols
committeeRegistrationCols =
  let c = TableColumn committeeRegistrationTableDef
  in CommitteeRegistrationCols
       { crcId        = c "id"
       , crcTxId      = c "tx_id"
       , crcCertIndex = c "cert_index"
       , crcColdKeyId = c "cold_key_id"
       , crcHotKeyId  = c "hot_key_id"
       }

committeeRegistrationColsList :: [TableColumn]
committeeRegistrationColsList =
  [ committeeRegistrationCols.crcId
  , committeeRegistrationCols.crcTxId
  , committeeRegistrationCols.crcCertIndex
  , committeeRegistrationCols.crcColdKeyId
  , committeeRegistrationCols.crcHotKeyId
  ]

data CommitteeDeRegistrationCols = CommitteeDeRegistrationCols
  { cdrcId             :: !TableColumn
  , cdrcTxId           :: !TableColumn
  , cdrcCertIndex      :: !TableColumn
  , cdrcVotingAnchorId :: !TableColumn
  , cdrcColdKeyId      :: !TableColumn
  }

committeeDeRegistrationCols :: CommitteeDeRegistrationCols
committeeDeRegistrationCols =
  let c = TableColumn committeeDeRegistrationTableDef
  in CommitteeDeRegistrationCols
       { cdrcId             = c "id"
       , cdrcTxId           = c "tx_id"
       , cdrcCertIndex      = c "cert_index"
       , cdrcVotingAnchorId = c "voting_anchor_id"
       , cdrcColdKeyId      = c "cold_key_id"
       }

committeeDeRegistrationColsList :: [TableColumn]
committeeDeRegistrationColsList =
  [ committeeDeRegistrationCols.cdrcId
  , committeeDeRegistrationCols.cdrcTxId
  , committeeDeRegistrationCols.cdrcCertIndex
  , committeeDeRegistrationCols.cdrcVotingAnchorId
  , committeeDeRegistrationCols.cdrcColdKeyId
  ]

data ParamProposalCols = ParamProposalCols
  { ppcId                          :: !TableColumn
  , ppcEpochNo                     :: !TableColumn
  , ppcKey                         :: !TableColumn
  , ppcMinFeeA                     :: !TableColumn
  , ppcMinFeeB                     :: !TableColumn
  , ppcMaxBlockSize                :: !TableColumn
  , ppcMaxTxSize                   :: !TableColumn
  , ppcMaxBhSize                   :: !TableColumn
  , ppcKeyDeposit                  :: !TableColumn
  , ppcPoolDeposit                 :: !TableColumn
  , ppcMaxEpoch                    :: !TableColumn
  , ppcOptimalPoolCount            :: !TableColumn
  , ppcInfluence                   :: !TableColumn
  , ppcMonetaryExpandRate          :: !TableColumn
  , ppcTreasuryGrowthRate          :: !TableColumn
  , ppcDecentralisation            :: !TableColumn
  , ppcEntropy                     :: !TableColumn
  , ppcProtocolMajor               :: !TableColumn
  , ppcProtocolMinor               :: !TableColumn
  , ppcMinUtxoValue                :: !TableColumn
  , ppcMinPoolCost                 :: !TableColumn
  , ppcCostModelId                 :: !TableColumn
  , ppcPriceMem                    :: !TableColumn
  , ppcPriceStep                   :: !TableColumn
  , ppcMaxTxExMem                  :: !TableColumn
  , ppcMaxTxExSteps                :: !TableColumn
  , ppcMaxBlockExMem               :: !TableColumn
  , ppcMaxBlockExSteps             :: !TableColumn
  , ppcMaxValSize                  :: !TableColumn
  , ppcCollateralPercent           :: !TableColumn
  , ppcMaxCollateralInputs         :: !TableColumn
  , ppcRegisteredTxId              :: !TableColumn
  , ppcCoinsPerUtxoSize            :: !TableColumn
  , ppcPvtMotionNoConfidence       :: !TableColumn
  , ppcPvtCommitteeNormal          :: !TableColumn
  , ppcPvtCommitteeNoConfidence    :: !TableColumn
  , ppcPvtHardForkInitiation       :: !TableColumn
  , ppcPvtppSecurityGroup          :: !TableColumn
  , ppcDvtMotionNoConfidence       :: !TableColumn
  , ppcDvtCommitteeNormal          :: !TableColumn
  , ppcDvtCommitteeNoConfidence    :: !TableColumn
  , ppcDvtUpdateToConstitution     :: !TableColumn
  , ppcDvtHardForkInitiation       :: !TableColumn
  , ppcDvtPPNetworkGroup           :: !TableColumn
  , ppcDvtPPEconomicGroup          :: !TableColumn
  , ppcDvtPPTechnicalGroup         :: !TableColumn
  , ppcDvtPPGovGroup               :: !TableColumn
  , ppcDvtTreasuryWithdrawal       :: !TableColumn
  , ppcCommitteeMinSize            :: !TableColumn
  , ppcCommitteeMaxTermLength      :: !TableColumn
  , ppcGovActionLifetime           :: !TableColumn
  , ppcGovActionDeposit            :: !TableColumn
  , ppcDrepDeposit                 :: !TableColumn
  , ppcDrepActivity                :: !TableColumn
  , ppcMinFeeRefScriptCostPerByte  :: !TableColumn
  }

paramProposalCols :: ParamProposalCols
paramProposalCols =
  let c = TableColumn paramProposalTableDef
  in ParamProposalCols
       { ppcId                         = c "id"
       , ppcEpochNo                    = c "epoch_no"
       , ppcKey                        = c "key"
       , ppcMinFeeA                    = c "min_fee_a"
       , ppcMinFeeB                    = c "min_fee_b"
       , ppcMaxBlockSize               = c "max_block_size"
       , ppcMaxTxSize                  = c "max_tx_size"
       , ppcMaxBhSize                  = c "max_bh_size"
       , ppcKeyDeposit                 = c "key_deposit"
       , ppcPoolDeposit                = c "pool_deposit"
       , ppcMaxEpoch                   = c "max_epoch"
       , ppcOptimalPoolCount           = c "optimal_pool_count"
       , ppcInfluence                  = c "influence"
       , ppcMonetaryExpandRate         = c "monetary_expand_rate"
       , ppcTreasuryGrowthRate         = c "treasury_growth_rate"
       , ppcDecentralisation           = c "decentralisation"
       , ppcEntropy                    = c "entropy"
       , ppcProtocolMajor              = c "protocol_major"
       , ppcProtocolMinor              = c "protocol_minor"
       , ppcMinUtxoValue               = c "min_utxo_value"
       , ppcMinPoolCost                = c "min_pool_cost"
       , ppcCostModelId                = c "cost_model_id"
       , ppcPriceMem                   = c "price_mem"
       , ppcPriceStep                  = c "price_step"
       , ppcMaxTxExMem                 = c "max_tx_ex_mem"
       , ppcMaxTxExSteps               = c "max_tx_ex_steps"
       , ppcMaxBlockExMem              = c "max_block_ex_mem"
       , ppcMaxBlockExSteps            = c "max_block_ex_steps"
       , ppcMaxValSize                 = c "max_val_size"
       , ppcCollateralPercent          = c "collateral_percent"
       , ppcMaxCollateralInputs        = c "max_collateral_inputs"
       , ppcRegisteredTxId             = c "registered_tx_id"
       , ppcCoinsPerUtxoSize           = c "coins_per_utxo_size"
       , ppcPvtMotionNoConfidence      = c "pvt_motion_no_confidence"
       , ppcPvtCommitteeNormal         = c "pvt_committee_normal"
       , ppcPvtCommitteeNoConfidence   = c "pvt_committee_no_confidence"
       , ppcPvtHardForkInitiation      = c "pvt_hard_fork_initiation"
       , ppcPvtppSecurityGroup         = c "pvtpp_security_group"
       , ppcDvtMotionNoConfidence      = c "dvt_motion_no_confidence"
       , ppcDvtCommitteeNormal         = c "dvt_committee_normal"
       , ppcDvtCommitteeNoConfidence   = c "dvt_committee_no_confidence"
       , ppcDvtUpdateToConstitution    = c "dvt_update_to_constitution"
       , ppcDvtHardForkInitiation      = c "dvt_hard_fork_initiation"
       , ppcDvtPPNetworkGroup          = c "dvt_pp_network_group"
       , ppcDvtPPEconomicGroup         = c "dvt_pp_economic_group"
       , ppcDvtPPTechnicalGroup        = c "dvt_pp_technical_group"
       , ppcDvtPPGovGroup              = c "dvt_pp_gov_group"
       , ppcDvtTreasuryWithdrawal      = c "dvt_treasury_withdrawal"
       , ppcCommitteeMinSize           = c "committee_min_size"
       , ppcCommitteeMaxTermLength     = c "committee_max_term_length"
       , ppcGovActionLifetime          = c "gov_action_lifetime"
       , ppcGovActionDeposit           = c "gov_action_deposit"
       , ppcDrepDeposit                = c "drep_deposit"
       , ppcDrepActivity               = c "drep_activity"
       , ppcMinFeeRefScriptCostPerByte = c "min_fee_ref_script_cost_per_byte"
       }

paramProposalColsList :: [TableColumn]
paramProposalColsList =
  [ paramProposalCols.ppcId
  , paramProposalCols.ppcEpochNo
  , paramProposalCols.ppcKey
  , paramProposalCols.ppcMinFeeA
  , paramProposalCols.ppcMinFeeB
  , paramProposalCols.ppcMaxBlockSize
  , paramProposalCols.ppcMaxTxSize
  , paramProposalCols.ppcMaxBhSize
  , paramProposalCols.ppcKeyDeposit
  , paramProposalCols.ppcPoolDeposit
  , paramProposalCols.ppcMaxEpoch
  , paramProposalCols.ppcOptimalPoolCount
  , paramProposalCols.ppcInfluence
  , paramProposalCols.ppcMonetaryExpandRate
  , paramProposalCols.ppcTreasuryGrowthRate
  , paramProposalCols.ppcDecentralisation
  , paramProposalCols.ppcEntropy
  , paramProposalCols.ppcProtocolMajor
  , paramProposalCols.ppcProtocolMinor
  , paramProposalCols.ppcMinUtxoValue
  , paramProposalCols.ppcMinPoolCost
  , paramProposalCols.ppcCostModelId
  , paramProposalCols.ppcPriceMem
  , paramProposalCols.ppcPriceStep
  , paramProposalCols.ppcMaxTxExMem
  , paramProposalCols.ppcMaxTxExSteps
  , paramProposalCols.ppcMaxBlockExMem
  , paramProposalCols.ppcMaxBlockExSteps
  , paramProposalCols.ppcMaxValSize
  , paramProposalCols.ppcCollateralPercent
  , paramProposalCols.ppcMaxCollateralInputs
  , paramProposalCols.ppcRegisteredTxId
  , paramProposalCols.ppcCoinsPerUtxoSize
  , paramProposalCols.ppcPvtMotionNoConfidence
  , paramProposalCols.ppcPvtCommitteeNormal
  , paramProposalCols.ppcPvtCommitteeNoConfidence
  , paramProposalCols.ppcPvtHardForkInitiation
  , paramProposalCols.ppcPvtppSecurityGroup
  , paramProposalCols.ppcDvtMotionNoConfidence
  , paramProposalCols.ppcDvtCommitteeNormal
  , paramProposalCols.ppcDvtCommitteeNoConfidence
  , paramProposalCols.ppcDvtUpdateToConstitution
  , paramProposalCols.ppcDvtHardForkInitiation
  , paramProposalCols.ppcDvtPPNetworkGroup
  , paramProposalCols.ppcDvtPPEconomicGroup
  , paramProposalCols.ppcDvtPPTechnicalGroup
  , paramProposalCols.ppcDvtPPGovGroup
  , paramProposalCols.ppcDvtTreasuryWithdrawal
  , paramProposalCols.ppcCommitteeMinSize
  , paramProposalCols.ppcCommitteeMaxTermLength
  , paramProposalCols.ppcGovActionLifetime
  , paramProposalCols.ppcGovActionDeposit
  , paramProposalCols.ppcDrepDeposit
  , paramProposalCols.ppcDrepActivity
  , paramProposalCols.ppcMinFeeRefScriptCostPerByte
  ]

data TreasuryWithdrawalCols = TreasuryWithdrawalCols
  { twcId                  :: !TableColumn
  , twcGovActionProposalId :: !TableColumn
  , twcStakeAddressId      :: !TableColumn
  , twcAmount              :: !TableColumn
  }

treasuryWithdrawalCols :: TreasuryWithdrawalCols
treasuryWithdrawalCols =
  let c = TableColumn treasuryWithdrawalTableDef
  in TreasuryWithdrawalCols
       { twcId                  = c "id"
       , twcGovActionProposalId = c "gov_action_proposal_id"
       , twcStakeAddressId      = c "stake_address_id"
       , twcAmount              = c "amount"
       }

treasuryWithdrawalColsList :: [TableColumn]
treasuryWithdrawalColsList =
  [ treasuryWithdrawalCols.twcId
  , treasuryWithdrawalCols.twcGovActionProposalId
  , treasuryWithdrawalCols.twcStakeAddressId
  , treasuryWithdrawalCols.twcAmount
  ]

data EventInfoCols = EventInfoCols
  { eicId          :: !TableColumn
  , eicTxId        :: !TableColumn
  , eicEpoch       :: !TableColumn
  , eicType        :: !TableColumn
  , eicExplanation :: !TableColumn
  }

eventInfoCols :: EventInfoCols
eventInfoCols =
  let c = TableColumn eventInfoTableDef
  in EventInfoCols
       { eicId          = c "id"
       , eicTxId        = c "tx_id"
       , eicEpoch       = c "epoch"
       , eicType        = c "type"
       , eicExplanation = c "explanation"
       }

eventInfoColsList :: [TableColumn]
eventInfoColsList =
  [ eventInfoCols.eicId
  , eventInfoCols.eicTxId
  , eventInfoCols.eicEpoch
  , eventInfoCols.eicType
  , eventInfoCols.eicExplanation
  ]

-- ---------------------------------------------------------------------------
-- * Per-module column-record registry
-- ---------------------------------------------------------------------------

governanceColumnRecords :: [(TableDef, [TableColumn])]
governanceColumnRecords =
  [ (drepHashTableDef,                drepHashColsList)
  , (drepRegistrationTableDef,        drepRegistrationColsList)
  , (drepDistrTableDef,               drepDistrColsList)
  , (delegationVoteTableDef,          delegationVoteColsList)
  , (govActionProposalTableDef,       govActionProposalColsList)
  , (votingProcedureTableDef,         votingProcedureColsList)
  , (votingAnchorTableDef,            votingAnchorColsList)
  , (constitutionTableDef,            constitutionColsList)
  , (committeeTableDef,               committeeColsList)
  , (committeeHashTableDef,           committeeHashColsList)
  , (committeeMemberTableDef,         committeeMemberColsList)
  , (committeeRegistrationTableDef,   committeeRegistrationColsList)
  , (committeeDeRegistrationTableDef, committeeDeRegistrationColsList)
  , (paramProposalTableDef,           paramProposalColsList)
  , (treasuryWithdrawalTableDef,      treasuryWithdrawalColsList)
  , (eventInfoTableDef,               eventInfoColsList)
  ]

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

encodeDrepRegistrationCopy :: DrepRegistration -> ByteString
encodeDrepRegistrationCopy dr =
  buildCopyRow
    [ Just $ bInt64 (getTxId $ drepRegistrationTxId dr)
    , Just $ bInt64 (fromIntegral $ drepRegistrationCertIndex dr)
    , bInt64 <$> drepRegistrationDeposit dr
    , Just $ bInt64 (getDrepHashId $ drepRegistrationDrepHashId dr)
    , bInt64 . getVotingAnchorId <$> drepRegistrationVotingAnchorId dr
    ]

encodeDrepDistrCopy :: DrepDistr -> ByteString
encodeDrepDistrCopy dd =
  buildCopyRow
    [ Just $ bInt64 (getDrepHashId $ drepDistrHashId dd)
    , Just $ bWord64 (drepDistrAmount dd)
    , Just $ bWord64 (drepDistrEpochNo dd)
    , bWord64 <$> drepDistrActiveUntil dd
    ]

encodeDelegationVoteCopy :: DelegationVote -> ByteString
encodeDelegationVoteCopy dv =
  buildCopyRow
    [ Just $ bInt64 (getStakeAddressId $ delegationVoteAddrId dv)
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

encodeVotingProcedureCopy :: VotingProcedure -> ByteString
encodeVotingProcedureCopy vp =
  buildCopyRow
    [ Just $ bInt64 (getTxId $ votingProcedureTxId vp)
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

encodeCommitteeMemberCopy :: CommitteeMember -> ByteString
encodeCommitteeMemberCopy cm =
  buildCopyRow
    [ Just $ bInt64 (getCommitteeId $ committeeMemberCommitteeId cm)
    , Just $ bInt64 (getCommitteeHashId $ committeeMemberCommitteeHashId cm)
    , Just $ bWord64 (committeeMemberExpirationEpoch cm)
    ]

encodeCommitteeRegistrationCopy :: CommitteeRegistration -> ByteString
encodeCommitteeRegistrationCopy cr =
  buildCopyRow
    [ Just $ bInt64 (getTxId $ committeeRegistrationTxId cr)
    , Just $ bInt64 (fromIntegral $ committeeRegistrationCertIndex cr)
    , Just $ bInt64 (getCommitteeHashId $ committeeRegistrationColdKeyId cr)
    , Just $ bInt64 (getCommitteeHashId $ committeeRegistrationHotKeyId cr)
    ]

encodeCommitteeDeRegistrationCopy :: CommitteeDeRegistration -> ByteString
encodeCommitteeDeRegistrationCopy cdr =
  buildCopyRow
    [ Just $ bInt64 (getTxId $ committeeDeRegistrationTxId cdr)
    , Just $ bInt64 (fromIntegral $ committeeDeRegistrationCertIndex cdr)
    , bInt64 . getVotingAnchorId <$> committeeDeRegistrationVotingAnchorId cdr
    , Just $ bInt64 (getCommitteeHashId $ committeeDeRegistrationColdKeyId cdr)
    ]

-- | 53 nullable parameter columns plus id and registered_tx_id.
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
    , bRational <$> paramProposalInfluence pp
    , bRational <$> paramProposalMonetaryExpandRate pp
    , bRational <$> paramProposalTreasuryGrowthRate pp
    , bRational <$> paramProposalDecentralisation pp
    , bHex <$> paramProposalEntropy pp
    , bInt64 . fromIntegral <$> paramProposalProtocolMajor pp
    , bInt64 . fromIntegral <$> paramProposalProtocolMinor pp
    , bWord64 . unDbLovelace <$> paramProposalMinUtxoValue pp
    , bWord64 . unDbLovelace <$> paramProposalMinPoolCost pp
    , bInt64 . getCostModelId <$> paramProposalCostModelId pp
    , bRational <$> paramProposalPriceMem pp
    , bRational <$> paramProposalPriceStep pp
    , bWord64 . unDbWord64 <$> paramProposalMaxTxExMem pp
    , bWord64 . unDbWord64 <$> paramProposalMaxTxExSteps pp
    , bWord64 . unDbWord64 <$> paramProposalMaxBlockExMem pp
    , bWord64 . unDbWord64 <$> paramProposalMaxBlockExSteps pp
    , bWord64 . unDbWord64 <$> paramProposalMaxValSize pp
    , bInt64 . fromIntegral <$> paramProposalCollateralPercent pp
    , bInt64 . fromIntegral <$> paramProposalMaxCollateralInputs pp
    , Just $ bInt64 (getTxId $ paramProposalRegisteredTxId pp)
    , bWord64 . unDbLovelace <$> paramProposalCoinsPerUtxoSize pp
    , bRational <$> paramProposalPvtMotionNoConfidence pp
    , bRational <$> paramProposalPvtCommitteeNormal pp
    , bRational <$> paramProposalPvtCommitteeNoConfidence pp
    , bRational <$> paramProposalPvtHardForkInitiation pp
    , bRational <$> paramProposalPvtppSecurityGroup pp
    , bRational <$> paramProposalDvtMotionNoConfidence pp
    , bRational <$> paramProposalDvtCommitteeNormal pp
    , bRational <$> paramProposalDvtCommitteeNoConfidence pp
    , bRational <$> paramProposalDvtUpdateToConstitution pp
    , bRational <$> paramProposalDvtHardForkInitiation pp
    , bRational <$> paramProposalDvtPPNetworkGroup pp
    , bRational <$> paramProposalDvtPPEconomicGroup pp
    , bRational <$> paramProposalDvtPPTechnicalGroup pp
    , bRational <$> paramProposalDvtPPGovGroup pp
    , bRational <$> paramProposalDvtTreasuryWithdrawal pp
    , bWord64 . unDbWord64 <$> paramProposalCommitteeMinSize pp
    , bWord64 . unDbWord64 <$> paramProposalCommitteeMaxTermLength pp
    , bWord64 . unDbWord64 <$> paramProposalGovActionLifetime pp
    , bWord64 . unDbWord64 <$> paramProposalGovActionDeposit pp
    , bWord64 . unDbWord64 <$> paramProposalDrepDeposit pp
    , bWord64 . unDbWord64 <$> paramProposalDrepActivity pp
    , bRational <$> paramProposalMinFeeRefScriptCostPerByte pp
    ]

encodeTreasuryWithdrawalCopy :: TreasuryWithdrawal -> ByteString
encodeTreasuryWithdrawalCopy tw =
  buildCopyRow
    [ Just $ bInt64 (getGovActionProposalId $ treasuryWithdrawalGovActionProposalId tw)
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
  , paramProposalInfluence                  >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalMonetaryExpandRate         >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalTreasuryGrowthRate         >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDecentralisation           >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalEntropy                    >$< E.param (E.nullable E.bytea)
  , (fmap fromIntegral :: Maybe Word16 -> Maybe Int16) . paramProposalProtocolMajor
                                            >$< E.param (E.nullable E.int2)
  , (fmap fromIntegral :: Maybe Word16 -> Maybe Int16) . paramProposalProtocolMinor
                                            >$< E.param (E.nullable E.int2)
  , paramProposalMinUtxoValue               >$< maybeDbLovelaceEncoder
  , paramProposalMinPoolCost                >$< maybeDbLovelaceEncoder
  , paramProposalCostModelId                >$< maybeIdEncoder getCostModelId
  , paramProposalPriceMem                   >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalPriceStep                  >$< E.param (E.nullable rationalAsNumericEncoder)
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
  , paramProposalPvtMotionNoConfidence      >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalPvtCommitteeNormal         >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalPvtCommitteeNoConfidence   >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalPvtHardForkInitiation      >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalPvtppSecurityGroup         >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtMotionNoConfidence      >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtCommitteeNormal         >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtCommitteeNoConfidence   >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtUpdateToConstitution    >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtHardForkInitiation      >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtPPNetworkGroup          >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtPPEconomicGroup         >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtPPTechnicalGroup        >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtPPGovGroup              >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalDvtTreasuryWithdrawal      >$< E.param (E.nullable rationalAsNumericEncoder)
  , paramProposalCommitteeMinSize           >$< maybeDbWord64Encoder
  , paramProposalCommitteeMaxTermLength     >$< maybeDbWord64Encoder
  , paramProposalGovActionLifetime          >$< maybeDbWord64Encoder
  , paramProposalGovActionDeposit           >$< maybeDbWord64Encoder
  , paramProposalDrepDeposit                >$< maybeDbWord64Encoder
  , paramProposalDrepActivity               >$< maybeDbWord64Encoder
  , paramProposalMinFeeRefScriptCostPerByte >$< E.param (E.nullable rationalAsNumericEncoder)
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
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable D.bytea)
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> maybeDbLovelaceDecoder
  <*> maybeDbLovelaceDecoder
  <*> maybeIdDecoder CostModelId
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> (fmap fromIntegral <$> D.column (D.nullable D.int2))
  <*> idDecoder TxId
  <*> maybeDbLovelaceDecoder
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> D.column (D.nullable rationalAsNumericDecoder)
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> maybeDbWord64Decoder
  <*> D.column (D.nullable rationalAsNumericDecoder)

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
