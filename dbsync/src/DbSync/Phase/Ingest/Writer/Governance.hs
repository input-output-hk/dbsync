-- | COPY writers for tables owned by the @governance@ extractor.
--
-- Five FK-referenced tables carry their assigned id (allocated by the
-- resolver) into the encoder; eight leaf tables ride the
-- IDENTITY-managed @id@ column and the encoder omits the field. Three
-- dedup-backed tables share the FK-style id-passing shape.
module DbSync.Phase.Ingest.Writer.Governance
  ( -- FK-referenced
    writeGovActionProposalCopy
  , writeParamProposalCopy
  , writeCommitteeCopy
  , writeConstitutionCopy
  , writeEventInfoCopy

    -- Dedup-backed
  , writeDrepHashCopy
  , writeCommitteeHashCopy
  , writeVotingAnchorCopy

    -- IDENTITY leaves
  , writeDrepRegistrationCopy
  , writeDrepDistrCopy
  , writeDelegationVoteCopy
  , writeVotingProcedureCopy
  , writeTreasuryWithdrawalCopy
  , writeCommitteeMemberCopy
  , writeCommitteeRegistrationCopy
  , writeCommitteeDeRegistrationCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Governance
  ( Committee
  , CommitteeDeRegistration
  , CommitteeHash
  , CommitteeMember
  , CommitteeRegistration
  , Constitution
  , DelegationVote
  , DrepDistr
  , DrepHash
  , DrepRegistration
  , EventInfo
  , GovActionProposal
  , ParamProposal
  , TreasuryWithdrawal
  , VotingAnchor
  , VotingProcedure
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
  , encodeCommitteeCopy
  , encodeCommitteeDeRegistrationCopy
  , encodeCommitteeHashCopy
  , encodeCommitteeMemberCopy
  , encodeCommitteeRegistrationCopy
  , encodeConstitutionCopy
  , encodeDelegationVoteCopy
  , encodeDrepDistrCopy
  , encodeDrepHashCopy
  , encodeDrepRegistrationCopy
  , encodeEventInfoCopy
  , encodeGovActionProposalCopy
  , encodeParamProposalCopy
  , encodeTreasuryWithdrawalCopy
  , encodeVotingAnchorCopy
  , encodeVotingProcedureCopy
  , eventInfoTableDef
  , govActionProposalTableDef
  , paramProposalTableDef
  , treasuryWithdrawalTableDef
  , votingAnchorTableDef
  , votingProcedureTableDef
  )
import DbSync.Db.Schema.Ids
  ( CommitteeHashId
  , CommitteeId
  , ConstitutionId
  , DrepHashId
  , EventInfoId
  , GovActionProposalId
  , ParamProposalId
  , VotingAnchorId
  )
import DbSync.Db.Schema.Types (TableDef (..))

-- ---------------------------------------------------------------------------
-- * FK-referenced
-- ---------------------------------------------------------------------------

writeGovActionProposalCopy
  :: LoaderStream -> GovActionProposalId -> GovActionProposal -> IO ()
writeGovActionProposalCopy ls gid g =
  lsWriteRow ls (tdName govActionProposalTableDef) (encodeGovActionProposalCopy gid g)

writeParamProposalCopy :: LoaderStream -> ParamProposalId -> ParamProposal -> IO ()
writeParamProposalCopy ls pid p =
  lsWriteRow ls (tdName paramProposalTableDef) (encodeParamProposalCopy pid p)

writeCommitteeCopy :: LoaderStream -> CommitteeId -> Committee -> IO ()
writeCommitteeCopy ls cid c =
  lsWriteRow ls (tdName committeeTableDef) (encodeCommitteeCopy cid c)

writeConstitutionCopy :: LoaderStream -> ConstitutionId -> Constitution -> IO ()
writeConstitutionCopy ls cid c =
  lsWriteRow ls (tdName constitutionTableDef) (encodeConstitutionCopy cid c)

writeEventInfoCopy :: LoaderStream -> EventInfoId -> EventInfo -> IO ()
writeEventInfoCopy ls eid e =
  lsWriteRow ls (tdName eventInfoTableDef) (encodeEventInfoCopy eid e)

-- ---------------------------------------------------------------------------
-- * Dedup-backed
-- ---------------------------------------------------------------------------

writeDrepHashCopy :: LoaderStream -> DrepHashId -> DrepHash -> IO ()
writeDrepHashCopy ls did d =
  lsWriteRow ls (tdName drepHashTableDef) (encodeDrepHashCopy did d)

writeCommitteeHashCopy :: LoaderStream -> CommitteeHashId -> CommitteeHash -> IO ()
writeCommitteeHashCopy ls cid c =
  lsWriteRow ls (tdName committeeHashTableDef) (encodeCommitteeHashCopy cid c)

writeVotingAnchorCopy :: LoaderStream -> VotingAnchorId -> VotingAnchor -> IO ()
writeVotingAnchorCopy ls vid v =
  lsWriteRow ls (tdName votingAnchorTableDef) (encodeVotingAnchorCopy vid v)

-- ---------------------------------------------------------------------------
-- * IDENTITY leaves
-- ---------------------------------------------------------------------------

writeDrepRegistrationCopy :: LoaderStream -> DrepRegistration -> IO ()
writeDrepRegistrationCopy ls dr =
  lsWriteRow ls (tdName drepRegistrationTableDef) (encodeDrepRegistrationCopy dr)

writeDrepDistrCopy :: LoaderStream -> DrepDistr -> IO ()
writeDrepDistrCopy ls dd =
  lsWriteRow ls (tdName drepDistrTableDef) (encodeDrepDistrCopy dd)

writeDelegationVoteCopy :: LoaderStream -> DelegationVote -> IO ()
writeDelegationVoteCopy ls dv =
  lsWriteRow ls (tdName delegationVoteTableDef) (encodeDelegationVoteCopy dv)

writeVotingProcedureCopy :: LoaderStream -> VotingProcedure -> IO ()
writeVotingProcedureCopy ls vp =
  lsWriteRow ls (tdName votingProcedureTableDef) (encodeVotingProcedureCopy vp)

writeTreasuryWithdrawalCopy :: LoaderStream -> TreasuryWithdrawal -> IO ()
writeTreasuryWithdrawalCopy ls tw =
  lsWriteRow ls (tdName treasuryWithdrawalTableDef) (encodeTreasuryWithdrawalCopy tw)

writeCommitteeMemberCopy :: LoaderStream -> CommitteeMember -> IO ()
writeCommitteeMemberCopy ls cm =
  lsWriteRow ls (tdName committeeMemberTableDef) (encodeCommitteeMemberCopy cm)

writeCommitteeRegistrationCopy :: LoaderStream -> CommitteeRegistration -> IO ()
writeCommitteeRegistrationCopy ls cr =
  lsWriteRow ls (tdName committeeRegistrationTableDef) (encodeCommitteeRegistrationCopy cr)

writeCommitteeDeRegistrationCopy :: LoaderStream -> CommitteeDeRegistration -> IO ()
writeCommitteeDeRegistrationCopy ls cdr =
  lsWriteRow ls (tdName committeeDeRegistrationTableDef) (encodeCommitteeDeRegistrationCopy cdr)
