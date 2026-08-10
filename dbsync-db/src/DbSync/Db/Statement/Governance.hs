{-# LANGUAGE OverloadedStrings #-}

-- | Hasql 'Statement' bindings for the @governance@ extractor tables.
--
-- Three flavours appear here:
--
--   * Dedup tables (@drep_hash@, @voting_anchor@, @committee_hash@) —
--     look up via the @query…IdStmt@; on a miss allocate from the
--     matching @next…IdStmt@.
--   * Counter-managed FK targets (@gov_action_proposal@,
--     @constitution@, @committee@, @param_proposal@, @event_info@) —
--     the resolver allocates a fresh id per row.
--   * IDENTITY leaves (everything else) — PostgreSQL fills @id@.
module DbSync.Db.Statement.Governance
  ( -- * drep_hash
    insertDrepHashRowStmt
  , nextDrepHashIdStmt
  , queryDrepHashIdStmt

    -- * drep_registration
  , insertDrepRegistrationRowStmt

    -- * drep_distr
  , insertDrepDistrRowStmt

    -- * delegation_vote
  , insertDelegationVoteRowStmt

    -- * gov_action_proposal
  , insertGovActionProposalRowStmt
  , nextGovActionProposalIdStmt
  , queryGovActionProposalByTxHashStmt

    -- * voting_procedure
  , insertVotingProcedureRowStmt

    -- * voting_anchor
  , insertVotingAnchorRowStmt
  , nextVotingAnchorIdStmt
  , queryVotingAnchorIdStmt

    -- * constitution
  , insertConstitutionRowStmt
  , nextConstitutionIdStmt

    -- * committee
  , insertCommitteeRowStmt
  , nextCommitteeIdStmt

    -- * committee_hash
  , insertCommitteeHashRowStmt
  , nextCommitteeHashIdStmt
  , queryCommitteeHashIdStmt

    -- * committee_member
  , insertCommitteeMemberRowStmt

    -- * committee_registration
  , insertCommitteeRegistrationRowStmt

    -- * committee_de_registration
  , insertCommitteeDeRegistrationRowStmt

    -- * param_proposal
  , insertParamProposalRowStmt
  , nextParamProposalIdStmt

    -- * treasury_withdrawal
  , insertTreasuryWithdrawalRowStmt

    -- * event_info
  , insertEventInfoRowStmt
  , nextEventInfoIdStmt
  ) where

import Cardano.Prelude

import Data.Functor.Contravariant ((>$<))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Core (TxCols (..), txCols, txTableDef)
import DbSync.Db.Schema.Governance
  ( Committee
  , CommitteeDeRegistration
  , CommitteeHash
  , CommitteeHashCols (..)
  , CommitteeMember
  , CommitteeRegistration
  , Constitution
  , DelegationVote
  , DrepDistr
  , DrepHash
  , DrepHashCols (..)
  , DrepRegistration
  , EventInfo
  , GovActionProposal
  , GovActionProposalCols (..)
  , ParamProposal
  , TreasuryWithdrawal
  , VotingAnchor
  , VotingAnchorCols (..)
  , VotingProcedure
  , committeeDeRegistrationEncoder
  , committeeDeRegistrationTableDef
  , committeeEncoder
  , committeeHashCols
  , committeeHashEncoder
  , committeeHashTableDef
  , committeeMemberEncoder
  , committeeMemberTableDef
  , committeeRegistrationEncoder
  , committeeRegistrationTableDef
  , committeeTableDef
  , constitutionEncoder
  , constitutionTableDef
  , delegationVoteEncoder
  , delegationVoteTableDef
  , drepDistrEncoder
  , drepDistrTableDef
  , drepHashCols
  , drepHashEncoder
  , drepHashTableDef
  , drepRegistrationEncoder
  , drepRegistrationTableDef
  , eventInfoEncoder
  , eventInfoTableDef
  , govActionProposalCols
  , govActionProposalEncoder
  , govActionProposalTableDef
  , paramProposalEncoder
  , paramProposalTableDef
  , treasuryWithdrawalEncoder
  , treasuryWithdrawalTableDef
  , votingAnchorCols
  , votingAnchorEncoder
  , votingAnchorTableDef
  , votingProcedureEncoder
  , votingProcedureTableDef
  )
import DbSync.Db.Schema.Ids
  ( CommitteeHashId (..)
  , CommitteeId (..)
  , ConstitutionId (..)
  , DrepHashId (..)
  , EventInfoId (..)
  , GovActionProposalId (..)
  , ParamProposalId (..)
  , VotingAnchorId (..)
  , idDecoder
  , idEncoder
  )
import DbSync.Db.Sql.Refs (col, qcol, table)
import DbSync.Db.Statement.Common (insertRowSql, nextIdStmt, upsertRowSql)
import DbSync.Db.Types (AnchorType, anchorTypeEncoder)

-- ---------------------------------------------------------------------------
-- * drep_hash
-- ---------------------------------------------------------------------------

insertDrepHashRowStmt :: Stmt.Statement (DrepHashId, DrepHash) ()
insertDrepHashRowStmt =
  Stmt.preparable (insertRowSql drepHashTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getDrepHashId)
           <> (snd >$< drepHashEncoder)

nextDrepHashIdStmt :: Stmt.Statement () DrepHashId
nextDrepHashIdStmt = nextIdStmt drepHashTableDef DrepHashId

-- | @view@ is required to disambiguate the two abstract DReps, which
-- both have @raw=NULL@ and @has_script=FALSE@.
queryDrepHashIdStmt :: Stmt.Statement (Maybe ByteString, Bool, Text) (Maybe DrepHashId)
queryDrepHashIdStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder DrepHashId))
  where
    sql = mconcat
      [ "SELECT ", col drepHashCols.dhcId, " FROM ", table drepHashTableDef
      , " WHERE ", col drepHashCols.dhcRaw, " IS NOT DISTINCT FROM $1"
      , " AND ", col drepHashCols.dhcHasScript, " = $2"
      , " AND ", col drepHashCols.dhcView, " = $3"
      ]
    encoder = ((\(r, _, _) -> r) >$< E.param (E.nullable E.bytea))
           <> ((\(_, s, _) -> s) >$< E.param (E.nonNullable E.bool))
           <> ((\(_, _, v) -> v) >$< E.param (E.nonNullable E.text))

-- ---------------------------------------------------------------------------
-- * drep_registration
-- ---------------------------------------------------------------------------

insertDrepRegistrationRowStmt :: Stmt.Statement DrepRegistration ()
insertDrepRegistrationRowStmt =
  Stmt.preparable
    (insertRowSql drepRegistrationTableDef)
    drepRegistrationEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * drep_distr
-- ---------------------------------------------------------------------------

-- | Written by 'runGovernanceBoundary' from the pulsing snapshot at
-- each epoch boundary. Upserts on @(hash_id, epoch_no)@ so a
-- rollback that re-crosses the boundary refreshes the rows.
insertDrepDistrRowStmt :: Stmt.Statement DrepDistr ()
insertDrepDistrRowStmt =
  Stmt.preparable (upsertRowSql drepDistrTableDef) drepDistrEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * delegation_vote
-- ---------------------------------------------------------------------------

insertDelegationVoteRowStmt :: Stmt.Statement DelegationVote ()
insertDelegationVoteRowStmt =
  Stmt.preparable
    (insertRowSql delegationVoteTableDef)
    delegationVoteEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * gov_action_proposal
-- ---------------------------------------------------------------------------

insertGovActionProposalRowStmt
  :: Stmt.Statement (GovActionProposalId, GovActionProposal) ()
insertGovActionProposalRowStmt =
  Stmt.preparable (insertRowSql govActionProposalTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getGovActionProposalId)
           <> (snd >$< govActionProposalEncoder)

nextGovActionProposalIdStmt :: Stmt.Statement () GovActionProposalId
nextGovActionProposalIdStmt =
  nextIdStmt govActionProposalTableDef GovActionProposalId

-- | SELECT-on-PG fallback for the Follow cross-block proposal cache;
-- resolves a vote's @GovActionId@ when the in-process cache misses.
queryGovActionProposalByTxHashStmt
  :: Stmt.Statement (ByteString, Word64) (Maybe GovActionProposalId)
queryGovActionProposalByTxHashStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder GovActionProposalId))
  where
    sql = mconcat
      [ "SELECT ", qcol "g" govActionProposalCols.gapcId
      , " FROM ", table govActionProposalTableDef, " g"
      , " JOIN ", table txTableDef, " t"
      ,   " ON ", qcol "t" txCols.tcId, " = ", qcol "g" govActionProposalCols.gapcTxId
      , " WHERE ", qcol "t" txCols.tcHash, " = $1"
      , " AND ", qcol "g" govActionProposalCols.gapcIndex, " = $2"
      ]
    encoder =
         (fst >$< E.param (E.nonNullable E.bytea))
      <> ((fromIntegral . snd :: (ByteString, Word64) -> Int64)
           >$< E.param (E.nonNullable E.int8))

-- ---------------------------------------------------------------------------
-- * voting_procedure
-- ---------------------------------------------------------------------------

insertVotingProcedureRowStmt :: Stmt.Statement VotingProcedure ()
insertVotingProcedureRowStmt =
  Stmt.preparable
    (insertRowSql votingProcedureTableDef)
    votingProcedureEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * voting_anchor
-- ---------------------------------------------------------------------------

insertVotingAnchorRowStmt :: Stmt.Statement (VotingAnchorId, VotingAnchor) ()
insertVotingAnchorRowStmt =
  Stmt.preparable (insertRowSql votingAnchorTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getVotingAnchorId)
           <> (snd >$< votingAnchorEncoder)

nextVotingAnchorIdStmt :: Stmt.Statement () VotingAnchorId
nextVotingAnchorIdStmt = nextIdStmt votingAnchorTableDef VotingAnchorId

-- | Matches the @(url, data_hash, type)@ unique constraint.
queryVotingAnchorIdStmt
  :: Stmt.Statement (Text, ByteString, AnchorType) (Maybe VotingAnchorId)
queryVotingAnchorIdStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder VotingAnchorId))
  where
    sql = mconcat
      [ "SELECT ", col votingAnchorCols.vacId
      , " FROM ", table votingAnchorTableDef
      , " WHERE ", col votingAnchorCols.vacUrl, " = $1"
      , " AND ", col votingAnchorCols.vacDataHash, " = $2"
      , " AND ", col votingAnchorCols.vacType, " = $3"
      ]
    encoder =
         ((\(u, _, _) -> u) >$< E.param (E.nonNullable E.text))
      <> ((\(_, h, _) -> h) >$< E.param (E.nonNullable E.bytea))
      <> ((\(_, _, t) -> t) >$< E.param (E.nonNullable anchorTypeEncoder))

-- ---------------------------------------------------------------------------
-- * constitution
-- ---------------------------------------------------------------------------

insertConstitutionRowStmt :: Stmt.Statement (ConstitutionId, Constitution) ()
insertConstitutionRowStmt =
  Stmt.preparable (insertRowSql constitutionTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getConstitutionId)
           <> (snd >$< constitutionEncoder)

nextConstitutionIdStmt :: Stmt.Statement () ConstitutionId
nextConstitutionIdStmt = nextIdStmt constitutionTableDef ConstitutionId

-- ---------------------------------------------------------------------------
-- * committee
-- ---------------------------------------------------------------------------

insertCommitteeRowStmt :: Stmt.Statement (CommitteeId, Committee) ()
insertCommitteeRowStmt =
  Stmt.preparable (insertRowSql committeeTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getCommitteeId)
           <> (snd >$< committeeEncoder)

nextCommitteeIdStmt :: Stmt.Statement () CommitteeId
nextCommitteeIdStmt = nextIdStmt committeeTableDef CommitteeId

-- ---------------------------------------------------------------------------
-- * committee_hash
-- ---------------------------------------------------------------------------

insertCommitteeHashRowStmt :: Stmt.Statement (CommitteeHashId, CommitteeHash) ()
insertCommitteeHashRowStmt =
  Stmt.preparable (insertRowSql committeeHashTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getCommitteeHashId)
           <> (snd >$< committeeHashEncoder)

nextCommitteeHashIdStmt :: Stmt.Statement () CommitteeHashId
nextCommitteeHashIdStmt = nextIdStmt committeeHashTableDef CommitteeHashId

queryCommitteeHashIdStmt :: Stmt.Statement (ByteString, Bool) (Maybe CommitteeHashId)
queryCommitteeHashIdStmt =
  Stmt.preparable sql encoder (D.rowMaybe (idDecoder CommitteeHashId))
  where
    sql = mconcat
      [ "SELECT ", col committeeHashCols.chcId
      , " FROM ", table committeeHashTableDef
      , " WHERE ", col committeeHashCols.chcRaw, " = $1"
      , " AND ", col committeeHashCols.chcHasScript, " = $2"
      ]
    encoder = (fst >$< E.param (E.nonNullable E.bytea))
           <> (snd >$< E.param (E.nonNullable E.bool))

-- ---------------------------------------------------------------------------
-- * committee_member
-- ---------------------------------------------------------------------------

insertCommitteeMemberRowStmt :: Stmt.Statement CommitteeMember ()
insertCommitteeMemberRowStmt =
  Stmt.preparable
    (insertRowSql committeeMemberTableDef)
    committeeMemberEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * committee_registration
-- ---------------------------------------------------------------------------

insertCommitteeRegistrationRowStmt :: Stmt.Statement CommitteeRegistration ()
insertCommitteeRegistrationRowStmt =
  Stmt.preparable
    (insertRowSql committeeRegistrationTableDef)
    committeeRegistrationEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * committee_de_registration
-- ---------------------------------------------------------------------------

insertCommitteeDeRegistrationRowStmt :: Stmt.Statement CommitteeDeRegistration ()
insertCommitteeDeRegistrationRowStmt =
  Stmt.preparable
    (insertRowSql committeeDeRegistrationTableDef)
    committeeDeRegistrationEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * param_proposal
-- ---------------------------------------------------------------------------

insertParamProposalRowStmt :: Stmt.Statement (ParamProposalId, ParamProposal) ()
insertParamProposalRowStmt =
  Stmt.preparable (insertRowSql paramProposalTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getParamProposalId)
           <> (snd >$< paramProposalEncoder)

nextParamProposalIdStmt :: Stmt.Statement () ParamProposalId
nextParamProposalIdStmt = nextIdStmt paramProposalTableDef ParamProposalId

-- ---------------------------------------------------------------------------
-- * treasury_withdrawal
-- ---------------------------------------------------------------------------

insertTreasuryWithdrawalRowStmt :: Stmt.Statement TreasuryWithdrawal ()
insertTreasuryWithdrawalRowStmt =
  Stmt.preparable
    (insertRowSql treasuryWithdrawalTableDef)
    treasuryWithdrawalEncoder
    D.noResult

-- ---------------------------------------------------------------------------
-- * event_info
-- ---------------------------------------------------------------------------

-- | No extractor populates @event_info@, so nothing calls this statement.
insertEventInfoRowStmt :: Stmt.Statement (EventInfoId, EventInfo) ()
insertEventInfoRowStmt =
  Stmt.preparable (insertRowSql eventInfoTableDef) encoder D.noResult
  where
    encoder = (fst >$< idEncoder getEventInfoId)
           <> (snd >$< eventInfoEncoder)

nextEventInfoIdStmt :: Stmt.Statement () EventInfoId
nextEventInfoIdStmt = nextIdStmt eventInfoTableDef EventInfoId
