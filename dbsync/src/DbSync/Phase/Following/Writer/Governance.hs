-- | hasql writers for tables owned by the @governance@ extractor.
--
-- Five FK-referenced tables take a caller-allocated id alongside the
-- row; three dedup-backed tables share the same shape; eight leaves
-- ride PG's IDENTITY column and the writer takes just the row.
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
import DbSync.Db.Statement.Governance (insertCommitteeRowStmt)
import DbSync.Db.Statement.Governance
  ( insertCommitteeDeRegistrationRowStmt
  )
import DbSync.Db.Statement.Governance (insertCommitteeHashRowStmt)
import DbSync.Db.Statement.Governance (insertCommitteeMemberRowStmt)
import DbSync.Db.Statement.Governance
  ( insertCommitteeRegistrationRowStmt
  )
import DbSync.Db.Statement.Governance (insertConstitutionRowStmt)
import DbSync.Db.Statement.Governance (insertDelegationVoteRowStmt)
import DbSync.Db.Statement.Governance (insertDrepDistrRowStmt)
import DbSync.Db.Statement.Governance (insertDrepHashRowStmt)
import DbSync.Db.Statement.Governance (insertDrepRegistrationRowStmt)
import DbSync.Db.Statement.Governance (insertEventInfoRowStmt)
import DbSync.Db.Statement.Governance (insertGovActionProposalRowStmt)
import DbSync.Db.Statement.Governance (insertParamProposalRowStmt)
import DbSync.Db.Statement.Governance (insertTreasuryWithdrawalRowStmt)
import DbSync.Db.Statement.Governance (insertVotingAnchorRowStmt)
import DbSync.Db.Statement.Governance (insertVotingProcedureRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

-- ---------------------------------------------------------------------------
-- * FK-referenced
-- ---------------------------------------------------------------------------

writeGovActionProposalConn
  :: Conn.Connection -> GovActionProposalId -> GovActionProposal -> IO ()
writeGovActionProposalConn conn gid g =
  runConn conn (gid, g) insertGovActionProposalRowStmt

writeGovActionProposalBuf
  :: WriteBuffer -> GovActionProposalId -> GovActionProposal -> IO ()
writeGovActionProposalBuf buf gid g =
  queueBuf buf (gid, g) insertGovActionProposalRowStmt

writeParamProposalConn
  :: Conn.Connection -> ParamProposalId -> ParamProposal -> IO ()
writeParamProposalConn conn pid p =
  runConn conn (pid, p) insertParamProposalRowStmt

writeParamProposalBuf
  :: WriteBuffer -> ParamProposalId -> ParamProposal -> IO ()
writeParamProposalBuf buf pid p =
  queueBuf buf (pid, p) insertParamProposalRowStmt

writeCommitteeConn :: Conn.Connection -> CommitteeId -> Committee -> IO ()
writeCommitteeConn conn cid c = runConn conn (cid, c) insertCommitteeRowStmt

writeCommitteeBuf :: WriteBuffer -> CommitteeId -> Committee -> IO ()
writeCommitteeBuf buf cid c = queueBuf buf (cid, c) insertCommitteeRowStmt

writeConstitutionConn
  :: Conn.Connection -> ConstitutionId -> Constitution -> IO ()
writeConstitutionConn conn cid c =
  runConn conn (cid, c) insertConstitutionRowStmt

writeConstitutionBuf :: WriteBuffer -> ConstitutionId -> Constitution -> IO ()
writeConstitutionBuf buf cid c = queueBuf buf (cid, c) insertConstitutionRowStmt

writeEventInfoConn :: Conn.Connection -> EventInfoId -> EventInfo -> IO ()
writeEventInfoConn conn eid e = runConn conn (eid, e) insertEventInfoRowStmt

writeEventInfoBuf :: WriteBuffer -> EventInfoId -> EventInfo -> IO ()
writeEventInfoBuf buf eid e = queueBuf buf (eid, e) insertEventInfoRowStmt

-- ---------------------------------------------------------------------------
-- * Dedup-backed
-- ---------------------------------------------------------------------------

writeDrepHashConn :: Conn.Connection -> DrepHashId -> DrepHash -> IO ()
writeDrepHashConn conn did d = runConn conn (did, d) insertDrepHashRowStmt

writeDrepHashBuf :: WriteBuffer -> DrepHashId -> DrepHash -> IO ()
writeDrepHashBuf buf did d = queueBuf buf (did, d) insertDrepHashRowStmt

writeCommitteeHashConn
  :: Conn.Connection -> CommitteeHashId -> CommitteeHash -> IO ()
writeCommitteeHashConn conn cid c = runConn conn (cid, c) insertCommitteeHashRowStmt

writeCommitteeHashBuf :: WriteBuffer -> CommitteeHashId -> CommitteeHash -> IO ()
writeCommitteeHashBuf buf cid c = queueBuf buf (cid, c) insertCommitteeHashRowStmt

writeVotingAnchorConn
  :: Conn.Connection -> VotingAnchorId -> VotingAnchor -> IO ()
writeVotingAnchorConn conn vid v = runConn conn (vid, v) insertVotingAnchorRowStmt

writeVotingAnchorBuf :: WriteBuffer -> VotingAnchorId -> VotingAnchor -> IO ()
writeVotingAnchorBuf buf vid v = queueBuf buf (vid, v) insertVotingAnchorRowStmt

-- ---------------------------------------------------------------------------
-- * IDENTITY leaves
-- ---------------------------------------------------------------------------

writeDrepRegistrationConn :: Conn.Connection -> DrepRegistration -> IO ()
writeDrepRegistrationConn conn dr = runConn conn dr insertDrepRegistrationRowStmt

writeDrepRegistrationBuf :: WriteBuffer -> DrepRegistration -> IO ()
writeDrepRegistrationBuf buf dr = queueBuf buf dr insertDrepRegistrationRowStmt

writeDrepDistrConn :: Conn.Connection -> DrepDistr -> IO ()
writeDrepDistrConn conn dd = runConn conn dd insertDrepDistrRowStmt

writeDrepDistrBuf :: WriteBuffer -> DrepDistr -> IO ()
writeDrepDistrBuf buf dd = queueBuf buf dd insertDrepDistrRowStmt

writeDelegationVoteConn :: Conn.Connection -> DelegationVote -> IO ()
writeDelegationVoteConn conn dv = runConn conn dv insertDelegationVoteRowStmt

writeDelegationVoteBuf :: WriteBuffer -> DelegationVote -> IO ()
writeDelegationVoteBuf buf dv = queueBuf buf dv insertDelegationVoteRowStmt

writeVotingProcedureConn :: Conn.Connection -> VotingProcedure -> IO ()
writeVotingProcedureConn conn vp = runConn conn vp insertVotingProcedureRowStmt

writeVotingProcedureBuf :: WriteBuffer -> VotingProcedure -> IO ()
writeVotingProcedureBuf buf vp = queueBuf buf vp insertVotingProcedureRowStmt

writeTreasuryWithdrawalConn :: Conn.Connection -> TreasuryWithdrawal -> IO ()
writeTreasuryWithdrawalConn conn tw =
  runConn conn tw insertTreasuryWithdrawalRowStmt

writeTreasuryWithdrawalBuf :: WriteBuffer -> TreasuryWithdrawal -> IO ()
writeTreasuryWithdrawalBuf buf tw =
  queueBuf buf tw insertTreasuryWithdrawalRowStmt

writeCommitteeMemberConn :: Conn.Connection -> CommitteeMember -> IO ()
writeCommitteeMemberConn conn cm =
  runConn conn cm insertCommitteeMemberRowStmt

writeCommitteeMemberBuf :: WriteBuffer -> CommitteeMember -> IO ()
writeCommitteeMemberBuf buf cm =
  queueBuf buf cm insertCommitteeMemberRowStmt

writeCommitteeRegistrationConn
  :: Conn.Connection -> CommitteeRegistration -> IO ()
writeCommitteeRegistrationConn conn cr =
  runConn conn cr insertCommitteeRegistrationRowStmt

writeCommitteeRegistrationBuf :: WriteBuffer -> CommitteeRegistration -> IO ()
writeCommitteeRegistrationBuf buf cr =
  queueBuf buf cr insertCommitteeRegistrationRowStmt

writeCommitteeDeRegistrationConn
  :: Conn.Connection -> CommitteeDeRegistration -> IO ()
writeCommitteeDeRegistrationConn conn cdr =
  runConn conn cdr insertCommitteeDeRegistrationRowStmt

writeCommitteeDeRegistrationBuf
  :: WriteBuffer -> CommitteeDeRegistration -> IO ()
writeCommitteeDeRegistrationBuf buf cdr =
  queueBuf buf cdr insertCommitteeDeRegistrationRowStmt
