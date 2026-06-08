{-# LANGUAGE BangPatterns #-}

-- | Follow 'IdResolver' fragments for the @governance@ extractor.
--
-- Five counter-managed FK tables get @assign*Id@ helpers that pop
-- from per-block pre-allocated queues (Buf) or @nextval@ over the
-- connection (Conn).
--
-- Three dedup-backed tables get @resolve*@ helpers. The cache key is
-- the encoded ByteString that matches the Ingest LSM layout; the
-- SELECT keys on the row's structured columns.
--
-- Cross-block scratchpads — the proposal cache becomes a SELECT-on-PG
-- + per-block 'BlockDedupCache' shadow; the enacted-state triple and
-- gov-action lifetime remain IORef-backed because no PG mirror exists.
module DbSync.Phase.Following.Resolver.Governance
  ( -- * Counter / nextval-style FKs
    assignGovActionProposalIdConn
  , assignGovActionProposalIdBuf
  , assignParamProposalIdConn
  , assignParamProposalIdBuf
  , assignCommitteeIdConn
  , assignCommitteeIdBuf
  , assignConstitutionIdConn
  , assignConstitutionIdBuf
  , assignEventInfoIdConn
  , assignEventInfoIdBuf

    -- * Dedup-style SELECT-then-INSERT
  , resolveDrepHashConn
  , resolveDrepHashBuf
  , resolveCommitteeHashConn
  , resolveCommitteeHashBuf
  , resolveVotingAnchorConn
  , resolveVotingAnchorBuf

    -- * Cross-block proposal cache
  , lookupGovActionProposalIdConn
  , lookupGovActionProposalIdBuf
  , recordGovActionProposalIdConn
  , recordGovActionProposalIdBuf

    -- * Cross-block scratchpads (IORef-backed)
  , GovScratchpad
  , newGovScratchpad
  , readEnactedEpochStateIdsRef
  , writeEnactedEpochStateIdsRef
  , readGovExpiresAfterRef
  , writeGovExpiresAfterRef
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Governance (CommitteeHash (..), DrepHash (..), VotingAnchor (..))
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
import DbSync.Db.Statement.Committee (nextCommitteeIdStmt)
import DbSync.Db.Statement.CommitteeHash
  ( nextCommitteeHashIdStmt
  , queryCommitteeHashIdStmt
  )
import DbSync.Db.Statement.Constitution (nextConstitutionIdStmt)
import DbSync.Db.Statement.DrepHash (nextDrepHashIdStmt, queryDrepHashIdStmt)
import DbSync.Db.Statement.EventInfo (nextEventInfoIdStmt)
import DbSync.Db.Statement.GovActionProposal
  ( nextGovActionProposalIdStmt
  , queryGovActionProposalByTxHashStmt
  )
import DbSync.Db.Statement.ParamProposal (nextParamProposalIdStmt)
import DbSync.Db.Statement.VotingAnchor
  ( nextVotingAnchorIdStmt
  , queryVotingAnchorIdStmt
  )
import DbSync.Db.Types (AnchorType, VoteUrl (..))
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , cacheInsert
  , resolveDedupWith
  , runStmt
  )

-- ---------------------------------------------------------------------------
-- * Counter / nextval-style FKs
-- ---------------------------------------------------------------------------

assignGovActionProposalIdConn :: Conn.Connection -> IO GovActionProposalId
assignGovActionProposalIdConn conn = runStmt conn () nextGovActionProposalIdStmt

assignGovActionProposalIdBuf :: PreAllocatedIds -> IO GovActionProposalId
assignGovActionProposalIdBuf preAlloc =
  popHead "assignGovActionProposalId" (paiGovActionProposalIds preAlloc)

assignParamProposalIdConn :: Conn.Connection -> IO ParamProposalId
assignParamProposalIdConn conn = runStmt conn () nextParamProposalIdStmt

assignParamProposalIdBuf :: PreAllocatedIds -> IO ParamProposalId
assignParamProposalIdBuf preAlloc =
  popHead "assignParamProposalId" (paiParamProposalIds preAlloc)

assignCommitteeIdConn :: Conn.Connection -> IO CommitteeId
assignCommitteeIdConn conn = runStmt conn () nextCommitteeIdStmt

assignCommitteeIdBuf :: PreAllocatedIds -> IO CommitteeId
assignCommitteeIdBuf preAlloc =
  popHead "assignCommitteeId" (paiCommitteeIds preAlloc)

assignConstitutionIdConn :: Conn.Connection -> IO ConstitutionId
assignConstitutionIdConn conn = runStmt conn () nextConstitutionIdStmt

assignConstitutionIdBuf :: PreAllocatedIds -> IO ConstitutionId
assignConstitutionIdBuf preAlloc =
  popHead "assignConstitutionId" (paiConstitutionIds preAlloc)

-- | @event_info@ has no pre-allocation lane in 'PreAllocatedIds' —
-- the table is never written today. The Buf flavour falls through
-- to 'nextval' via the connection so a future writer doesn't need
-- to wait for a pipeline upgrade.
assignEventInfoIdConn :: Conn.Connection -> IO EventInfoId
assignEventInfoIdConn conn = runStmt conn () nextEventInfoIdStmt

assignEventInfoIdBuf :: Conn.Connection -> IO EventInfoId
assignEventInfoIdBuf conn = runStmt conn () nextEventInfoIdStmt

-- ---------------------------------------------------------------------------
-- * Dedup-style SELECT-then-INSERT
-- ---------------------------------------------------------------------------

-- | Resolve a @drep_hash@ row. The cache key is the encoded dedup
-- key supplied by 'SharedDedup.resolveAndWriteDrepHash'; the SELECT
-- keys on @(raw, has_script, view)@ — @view@ distinguishes the two
-- abstract DReps, which share @(NULL, FALSE)@.
resolveDrepHashConn
  :: Conn.Connection -> ByteString -> DrepHash -> IO (DrepHashId, Bool)
resolveDrepHashConn conn _key row = do
  mId <- runStmt conn (drepHashSelectKey row) queryDrepHashIdStmt
  case mId of
    Just did -> pure (did, False)
    Nothing  -> do
      did <- runStmt conn () nextDrepHashIdStmt
      pure (did, True)

resolveDrepHashBuf
  :: Conn.Connection
  -> BlockDedupCache
  -> ByteString
  -> DrepHash
  -> IO (DrepHashId, Bool)
resolveDrepHashBuf conn cache key row =
  resolveDedupWith
    conn
    key
    (drepHashSelectKey row)
    (bdcDrepHash cache)
    queryDrepHashIdStmt
    nextDrepHashIdStmt

drepHashSelectKey :: DrepHash -> (Maybe ByteString, Bool, Text)
drepHashSelectKey row =
  (drepHashRaw row, drepHashHasScript row, drepHashView row)

resolveCommitteeHashConn
  :: Conn.Connection -> ByteString -> CommitteeHash -> IO (CommitteeHashId, Bool)
resolveCommitteeHashConn conn _key row = do
  mId <- runStmt conn (committeeHashRaw row, committeeHashHasScript row) queryCommitteeHashIdStmt
  case mId of
    Just cid -> pure (cid, False)
    Nothing  -> do
      cid <- runStmt conn () nextCommitteeHashIdStmt
      pure (cid, True)

resolveCommitteeHashBuf
  :: Conn.Connection
  -> BlockDedupCache
  -> ByteString
  -> CommitteeHash
  -> IO (CommitteeHashId, Bool)
resolveCommitteeHashBuf conn cache key row =
  resolveDedupWith
    conn
    key
    (committeeHashRaw row, committeeHashHasScript row)
    (bdcCommitteeHash cache)
    queryCommitteeHashIdStmt
    nextCommitteeHashIdStmt

resolveVotingAnchorConn
  :: Conn.Connection
  -> ByteString
  -> AnchorType
  -> VotingAnchor
  -> IO (VotingAnchorId, Bool)
resolveVotingAnchorConn conn _key _anchorType row = do
  mId <- runStmt conn (selectKey row) queryVotingAnchorIdStmt
  case mId of
    Just vid -> pure (vid, False)
    Nothing  -> do
      vid <- runStmt conn () nextVotingAnchorIdStmt
      pure (vid, True)

resolveVotingAnchorBuf
  :: Conn.Connection
  -> BlockDedupCache
  -> ByteString
  -> AnchorType
  -> VotingAnchor
  -> IO (VotingAnchorId, Bool)
resolveVotingAnchorBuf conn cache key _anchorType row =
  resolveDedupWith
    conn
    key
    (selectKey row)
    (bdcVotingAnchor cache)
    queryVotingAnchorIdStmt
    nextVotingAnchorIdStmt

-- | Project a @voting_anchor@ row into the @(url, data_hash, type)@
-- triple used by 'queryVotingAnchorIdStmt'.
selectKey :: VotingAnchor -> (Text, ByteString, AnchorType)
selectKey row =
  ( unVoteUrl (votingAnchorUrl row)
  , votingAnchorDataHash row
  , votingAnchorType row
  )

-- ---------------------------------------------------------------------------
-- * Cross-block proposal cache
-- ---------------------------------------------------------------------------

-- | Direct lookup: SELECT against PG. No in-process cache because each
-- proposal row lands immediately when the proposing tx commits.
lookupGovActionProposalIdConn
  :: Conn.Connection -> ByteString -> Word64 -> IO (Maybe GovActionProposalId)
lookupGovActionProposalIdConn conn txHash idx =
  runStmt conn (txHash, idx) queryGovActionProposalByTxHashStmt

-- | Buffered lookup: check the per-block shadow first; on miss
-- SELECT against PG. The shadow covers same-block proposal/vote
-- pairs whose proposal write hasn't flushed yet.
lookupGovActionProposalIdBuf
  :: Conn.Connection
  -> BlockDedupCache
  -> ByteString
  -> Word64
  -> IO (Maybe GovActionProposalId)
lookupGovActionProposalIdBuf conn cache txHash idx = do
  m <- readIORef (bdcGovActionProposal cache)
  case Map.lookup (txHash, idx) m of
    Just gid -> pure (Just gid)
    Nothing  -> runStmt conn (txHash, idx) queryGovActionProposalByTxHashStmt

-- | No-op for the direct flavour: the row lands via @writeGovActionProposal@
-- and a follow-up @lookupGovActionProposalIdConn@ finds it via SELECT.
recordGovActionProposalIdConn
  :: ByteString -> Word64 -> GovActionProposalId -> IO ()
recordGovActionProposalIdConn _ _ _ = pure ()

-- | Buffered flavour: stash the freshly-allocated id in the per-block
-- shadow so a same-block vote pass can resolve it without waiting for
-- the buffer flush.
recordGovActionProposalIdBuf
  :: BlockDedupCache
  -> ByteString
  -> Word64
  -> GovActionProposalId
  -> IO ()
recordGovActionProposalIdBuf cache txHash idx gid =
  cacheInsert (bdcGovActionProposal cache) (txHash, idx) gid

-- ---------------------------------------------------------------------------
-- * Cross-block scratchpads (IORef-backed)
-- ---------------------------------------------------------------------------

-- | Per-session governance scratchpad for state that survives a block
-- transition but has no direct PG mirror (the enacted-state triple
-- and the gov-action lifetime).
data GovScratchpad = GovScratchpad
  { gsEnactedEpochStateIds :: !(IORef (Maybe Int64, Maybe Int64, Maybe Int64))
  , gsExpiresAfter         :: !(IORef (Maybe Word64))
  }

newGovScratchpad :: IO GovScratchpad
newGovScratchpad =
  GovScratchpad
    <$> newIORef (Nothing, Nothing, Nothing)
    <*> newIORef Nothing

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
