-- | Follow 'IdResolver' fragments for the @governance@ extractor.
--
-- Follow-phase plumbing is not landed for any of these IDs; both
-- direct and buffered flavours use the same stubs.
module DbSync.Phase.Following.Resolver.Governance
  ( -- Counter / nextval-style FKs
    assignGovActionProposalIdStub
  , assignParamProposalIdStub
  , assignCommitteeIdStub
  , assignConstitutionIdStub
  , assignEventInfoIdStub

    -- Dedup-style SELECT-then-INSERT
  , resolveDrepHashStub
  , resolveCommitteeHashStub
  , resolveVotingAnchorStub

    -- Cross-block scratchpads (no Follow plumbing yet)
  , lookupGovActionProposalIdStub
  , recordGovActionProposalIdStub
  , readEnactedEpochStateIdsStub
  , writeEnactedEpochStateIdsStub
  , readGovExpiresAfterStub
  , writeGovExpiresAfterStub
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Governance (CommitteeHash, DrepHash, VotingAnchor)
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
import DbSync.Db.Types (AnchorType)
import DbSync.Phase.Following.Resolver.Internal (todoResolve)

assignGovActionProposalIdStub :: IO GovActionProposalId
assignGovActionProposalIdStub = todoResolve "assignGovActionProposalId"

assignParamProposalIdStub :: IO ParamProposalId
assignParamProposalIdStub = todoResolve "assignParamProposalId"

assignCommitteeIdStub :: IO CommitteeId
assignCommitteeIdStub = todoResolve "assignCommitteeId"

assignConstitutionIdStub :: IO ConstitutionId
assignConstitutionIdStub = todoResolve "assignConstitutionId"

assignEventInfoIdStub :: IO EventInfoId
assignEventInfoIdStub = todoResolve "assignEventInfoId"

resolveDrepHashStub :: ByteString -> DrepHash -> IO (DrepHashId, Bool)
resolveDrepHashStub _ _ = todoResolve "resolveDrepHash"

resolveCommitteeHashStub
  :: ByteString -> CommitteeHash -> IO (CommitteeHashId, Bool)
resolveCommitteeHashStub _ _ = todoResolve "resolveCommitteeHash"

resolveVotingAnchorStub
  :: ByteString -> AnchorType -> VotingAnchor -> IO (VotingAnchorId, Bool)
resolveVotingAnchorStub _ _ _ = todoResolve "resolveVotingAnchor"

lookupGovActionProposalIdStub
  :: ByteString -> Word64 -> IO (Maybe GovActionProposalId)
lookupGovActionProposalIdStub _ _ = todoResolve "lookupGovActionProposalId"

recordGovActionProposalIdStub
  :: ByteString -> Word64 -> GovActionProposalId -> IO ()
recordGovActionProposalIdStub _ _ _ = todoResolve "recordGovActionProposalId"

readEnactedEpochStateIdsStub :: IO (Maybe Int64, Maybe Int64, Maybe Int64)
readEnactedEpochStateIdsStub = todoResolve "readEnactedEpochStateIds"

writeEnactedEpochStateIdsStub
  :: (Maybe Int64, Maybe Int64, Maybe Int64) -> IO ()
writeEnactedEpochStateIdsStub _ = todoResolve "writeEnactedEpochStateIds"

readGovExpiresAfterStub :: IO (Maybe Word64)
readGovExpiresAfterStub = todoResolve "readGovExpiresAfter"

writeGovExpiresAfterStub :: Maybe Word64 -> IO ()
writeGovExpiresAfterStub _ = todoResolve "writeGovExpiresAfter"
