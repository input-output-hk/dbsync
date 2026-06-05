-- | hasql writers for tables owned by the @governance@ extractor.
--
-- All flavours panic via 'todoWrite' until the per-table INSERT
-- statements are wired. Both the direct ('*Conn') and buffered
-- ('*Buf') variants share the same stub.
module DbSync.Phase.Following.Writer.Governance
  ( -- FK-referenced
    writeGovActionProposalConn,        writeGovActionProposalBuf
  , writeParamProposalConn,            writeParamProposalBuf
  , writeCommitteeConn,                writeCommitteeBuf
  , writeConstitutionConn,             writeConstitutionBuf
  , writeEventInfoConn,                writeEventInfoBuf

    -- Dedup-backed
  , writeDrepHashConn,                 writeDrepHashBuf
  , writeCommitteeHashConn,            writeCommitteeHashBuf
  , writeVotingAnchorConn,             writeVotingAnchorBuf

    -- IDENTITY leaves
  , writeDrepRegistrationConn,         writeDrepRegistrationBuf
  , writeDrepDistrConn,                writeDrepDistrBuf
  , writeDelegationVoteConn,           writeDelegationVoteBuf
  , writeVotingProcedureConn,          writeVotingProcedureBuf
  , writeTreasuryWithdrawalConn,       writeTreasuryWithdrawalBuf
  , writeCommitteeMemberConn,          writeCommitteeMemberBuf
  , writeCommitteeRegistrationConn,    writeCommitteeRegistrationBuf
  , writeCommitteeDeRegistrationConn,  writeCommitteeDeRegistrationBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

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
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (todoWrite, todoWriteLeaf)

-- ---------------------------------------------------------------------------
-- * FK-referenced
-- ---------------------------------------------------------------------------

writeGovActionProposalConn
  :: Conn.Connection -> GovActionProposalId -> GovActionProposal -> IO ()
writeGovActionProposalConn _ = todoWrite "writeGovActionProposal"

writeGovActionProposalBuf
  :: WriteBuffer -> GovActionProposalId -> GovActionProposal -> IO ()
writeGovActionProposalBuf _ = todoWrite "writeGovActionProposal"

writeParamProposalConn :: Conn.Connection -> ParamProposalId -> ParamProposal -> IO ()
writeParamProposalConn _ = todoWrite "writeParamProposal"

writeParamProposalBuf :: WriteBuffer -> ParamProposalId -> ParamProposal -> IO ()
writeParamProposalBuf _ = todoWrite "writeParamProposal"

writeCommitteeConn :: Conn.Connection -> CommitteeId -> Committee -> IO ()
writeCommitteeConn _ = todoWrite "writeCommittee"

writeCommitteeBuf :: WriteBuffer -> CommitteeId -> Committee -> IO ()
writeCommitteeBuf _ = todoWrite "writeCommittee"

writeConstitutionConn :: Conn.Connection -> ConstitutionId -> Constitution -> IO ()
writeConstitutionConn _ = todoWrite "writeConstitution"

writeConstitutionBuf :: WriteBuffer -> ConstitutionId -> Constitution -> IO ()
writeConstitutionBuf _ = todoWrite "writeConstitution"

writeEventInfoConn :: Conn.Connection -> EventInfoId -> EventInfo -> IO ()
writeEventInfoConn _ = todoWrite "writeEventInfo"

writeEventInfoBuf :: WriteBuffer -> EventInfoId -> EventInfo -> IO ()
writeEventInfoBuf _ = todoWrite "writeEventInfo"

-- ---------------------------------------------------------------------------
-- * Dedup-backed
-- ---------------------------------------------------------------------------

writeDrepHashConn :: Conn.Connection -> DrepHashId -> DrepHash -> IO ()
writeDrepHashConn _ = todoWrite "writeDrepHash"

writeDrepHashBuf :: WriteBuffer -> DrepHashId -> DrepHash -> IO ()
writeDrepHashBuf _ = todoWrite "writeDrepHash"

writeCommitteeHashConn :: Conn.Connection -> CommitteeHashId -> CommitteeHash -> IO ()
writeCommitteeHashConn _ = todoWrite "writeCommitteeHash"

writeCommitteeHashBuf :: WriteBuffer -> CommitteeHashId -> CommitteeHash -> IO ()
writeCommitteeHashBuf _ = todoWrite "writeCommitteeHash"

writeVotingAnchorConn :: Conn.Connection -> VotingAnchorId -> VotingAnchor -> IO ()
writeVotingAnchorConn _ = todoWrite "writeVotingAnchor"

writeVotingAnchorBuf :: WriteBuffer -> VotingAnchorId -> VotingAnchor -> IO ()
writeVotingAnchorBuf _ = todoWrite "writeVotingAnchor"

-- ---------------------------------------------------------------------------
-- * IDENTITY leaves
-- ---------------------------------------------------------------------------

writeDrepRegistrationConn :: Conn.Connection -> DrepRegistration -> IO ()
writeDrepRegistrationConn _ = todoWriteLeaf "writeDrepRegistration"

writeDrepRegistrationBuf :: WriteBuffer -> DrepRegistration -> IO ()
writeDrepRegistrationBuf _ = todoWriteLeaf "writeDrepRegistration"

writeDrepDistrConn :: Conn.Connection -> DrepDistr -> IO ()
writeDrepDistrConn _ = todoWriteLeaf "writeDrepDistr"

writeDrepDistrBuf :: WriteBuffer -> DrepDistr -> IO ()
writeDrepDistrBuf _ = todoWriteLeaf "writeDrepDistr"

writeDelegationVoteConn :: Conn.Connection -> DelegationVote -> IO ()
writeDelegationVoteConn _ = todoWriteLeaf "writeDelegationVote"

writeDelegationVoteBuf :: WriteBuffer -> DelegationVote -> IO ()
writeDelegationVoteBuf _ = todoWriteLeaf "writeDelegationVote"

writeVotingProcedureConn :: Conn.Connection -> VotingProcedure -> IO ()
writeVotingProcedureConn _ = todoWriteLeaf "writeVotingProcedure"

writeVotingProcedureBuf :: WriteBuffer -> VotingProcedure -> IO ()
writeVotingProcedureBuf _ = todoWriteLeaf "writeVotingProcedure"

writeTreasuryWithdrawalConn :: Conn.Connection -> TreasuryWithdrawal -> IO ()
writeTreasuryWithdrawalConn _ = todoWriteLeaf "writeTreasuryWithdrawal"

writeTreasuryWithdrawalBuf :: WriteBuffer -> TreasuryWithdrawal -> IO ()
writeTreasuryWithdrawalBuf _ = todoWriteLeaf "writeTreasuryWithdrawal"

writeCommitteeMemberConn :: Conn.Connection -> CommitteeMember -> IO ()
writeCommitteeMemberConn _ = todoWriteLeaf "writeCommitteeMember"

writeCommitteeMemberBuf :: WriteBuffer -> CommitteeMember -> IO ()
writeCommitteeMemberBuf _ = todoWriteLeaf "writeCommitteeMember"

writeCommitteeRegistrationConn :: Conn.Connection -> CommitteeRegistration -> IO ()
writeCommitteeRegistrationConn _ = todoWriteLeaf "writeCommitteeRegistration"

writeCommitteeRegistrationBuf :: WriteBuffer -> CommitteeRegistration -> IO ()
writeCommitteeRegistrationBuf _ = todoWriteLeaf "writeCommitteeRegistration"

writeCommitteeDeRegistrationConn :: Conn.Connection -> CommitteeDeRegistration -> IO ()
writeCommitteeDeRegistrationConn _ = todoWriteLeaf "writeCommitteeDeRegistration"

writeCommitteeDeRegistrationBuf :: WriteBuffer -> CommitteeDeRegistration -> IO ()
writeCommitteeDeRegistrationBuf _ = todoWriteLeaf "writeCommitteeDeRegistration"
