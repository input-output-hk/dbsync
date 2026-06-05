{-# LANGUAGE BangPatterns #-}

-- | Follow 'IdResolver' fragments for the @governance@ extractor.
--
-- Per-block governance writes are not yet plumbed for Follow; the
-- dedup resolvers and assigners panic. The cross-block scratchpad
-- ops are backed by per-session 'IORef's so 'runEpochBoundary' can
-- read 'readEnactedEpochStateIds' without hitting a stub: with
-- governance disabled in Follow they stay at their defaults
-- (@(Nothing, Nothing, Nothing)@ and @Nothing@) and 'epoch_state'
-- writes NULLs into the three governance FK columns.
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

    -- Cross-block scratchpads — IORef-backed
  , GovScratchpad
  , newGovScratchpad
  , lookupGovActionProposalIdRef
  , recordGovActionProposalIdRef
  , readEnactedEpochStateIdsRef
  , writeEnactedEpochStateIdsRef
  , readGovExpiresAfterRef
  , writeGovExpiresAfterRef
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map

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

-- ---------------------------------------------------------------------------
-- * Per-block / counter stubs (unchanged — governance Follow not wired)
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- * Cross-block scratchpads
-- ---------------------------------------------------------------------------

-- | Per-session governance scratchpad. Mirrors the Ingest fields on
-- 'ExtractState' but lives in process-local IORefs because Follow
-- has no equivalent state record.
data GovScratchpad = GovScratchpad
  { gsProposalCache       :: !(IORef (Map (ByteString, Word64) GovActionProposalId))
  , gsEnactedEpochStateIds :: !(IORef (Maybe Int64, Maybe Int64, Maybe Int64))
  , gsExpiresAfter        :: !(IORef (Maybe Word64))
  }

newGovScratchpad :: IO GovScratchpad
newGovScratchpad =
  GovScratchpad
    <$> newIORef Map.empty
    <*> newIORef (Nothing, Nothing, Nothing)
    <*> newIORef Nothing

lookupGovActionProposalIdRef
  :: GovScratchpad -> ByteString -> Word64 -> IO (Maybe GovActionProposalId)
lookupGovActionProposalIdRef gs txHash idx = do
  m <- readIORef (gsProposalCache gs)
  pure (Map.lookup (txHash, idx) m)

recordGovActionProposalIdRef
  :: GovScratchpad -> ByteString -> Word64 -> GovActionProposalId -> IO ()
recordGovActionProposalIdRef gs txHash idx gid =
  atomicModifyIORef' (gsProposalCache gs) $ \m ->
    (Map.insert (txHash, idx) gid m, ())

readEnactedEpochStateIdsRef
  :: GovScratchpad -> IO (Maybe Int64, Maybe Int64, Maybe Int64)
readEnactedEpochStateIdsRef = readIORef . gsEnactedEpochStateIds

writeEnactedEpochStateIdsRef
  :: GovScratchpad -> (Maybe Int64, Maybe Int64, Maybe Int64) -> IO ()
writeEnactedEpochStateIdsRef gs !triple =
  atomicModifyIORef' (gsEnactedEpochStateIds gs) $ \_ -> (triple, ())

readGovExpiresAfterRef :: GovScratchpad -> IO (Maybe Word64)
readGovExpiresAfterRef = readIORef . gsExpiresAfter

writeGovExpiresAfterRef :: GovScratchpad -> Maybe Word64 -> IO ()
writeGovExpiresAfterRef gs !mv =
  atomicModifyIORef' (gsExpiresAfter gs) $ \_ -> (mv, ())
