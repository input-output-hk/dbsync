{-# LANGUAGE OverloadedStrings #-}

-- | Ingest 'IdResolver' fragments for the @governance@ extractor.
--
-- Five counter-managed FK tables (gov_action_proposal, param_proposal,
-- committee, constitution, event_info) each get an @assign*Id@
-- function that pulls from 'IdCounters' on 'ExtractState'.
--
-- Three dedup-backed tables (drep_hash, committee_hash, voting_anchor)
-- get a @resolve*@ function backed by an LSM dedup store. The voting
-- anchor's natural key is @(url, data_hash, type)@; the caller encodes
-- the triple into the lookup key.
module DbSync.Phase.Ingest.Resolver.Governance
  ( assignGovActionProposalIdIngest
  , assignParamProposalIdIngest
  , assignCommitteeIdIngest
  , assignConstitutionIdIngest
  , assignEventInfoIdIngest

  , resolveDrepHashIngest
  , resolveCommitteeHashIngest
  , resolveVotingAnchorIngest

  , lookupGovActionProposalIdIngest
  , recordGovActionProposalIdIngest
  , readEnactedEpochStateIdsIngest
  , writeEnactedEpochStateIdsIngest
  , readGovExpiresAfterIngest
  , writeGovExpiresAfterIngest
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import qualified Data.Map.Strict as Map

import DbSync.Db.Schema.Governance (CommitteeHash, DrepHash, VotingAnchor)
import DbSync.Db.Schema.Ids
  ( CommitteeHashId (..)
  , CommitteeId (..)
  , ConstitutionId (..)
  , DrepHashId (..)
  , EventInfoId (..)
  , GovActionProposalId (..)
  , ParamProposalId (..)
  , VotingAnchorId (..)
  , getGovActionProposalId
  )
import DbSync.Db.Types (AnchorType)
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..))
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert)
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)

-- ---------------------------------------------------------------------------
-- * Counter-managed FK tables
-- ---------------------------------------------------------------------------

assignGovActionProposalIdIngest :: IORef ExtractState -> IO GovActionProposalId
assignGovActionProposalIdIngest extractStateRef =
  allocateNextId extractStateRef icGovActionProposalId
    (\cs c -> cs { icGovActionProposalId = c }) GovActionProposalId

assignParamProposalIdIngest :: IORef ExtractState -> IO ParamProposalId
assignParamProposalIdIngest extractStateRef =
  allocateNextId extractStateRef icParamProposalId
    (\cs c -> cs { icParamProposalId = c }) ParamProposalId

assignCommitteeIdIngest :: IORef ExtractState -> IO CommitteeId
assignCommitteeIdIngest extractStateRef =
  allocateNextId extractStateRef icCommitteeId
    (\cs c -> cs { icCommitteeId = c }) CommitteeId

assignConstitutionIdIngest :: IORef ExtractState -> IO ConstitutionId
assignConstitutionIdIngest extractStateRef =
  allocateNextId extractStateRef icConstitutionId
    (\cs c -> cs { icConstitutionId = c }) ConstitutionId

assignEventInfoIdIngest :: IORef ExtractState -> IO EventInfoId
assignEventInfoIdIngest extractStateRef =
  allocateNextId extractStateRef icEventInfoId
    (\cs c -> cs { icEventInfoId = c }) EventInfoId

-- ---------------------------------------------------------------------------
-- * Dedup-backed tables
-- ---------------------------------------------------------------------------

resolveDrepHashIngest
  :: DedupStores -> ByteString -> DrepHash -> IO (DrepHashId, Bool)
resolveDrepHashIngest stores key _row = do
  (rawId, isNew) <- lookupOrInsert (SBS.toShort key) (dstDrepHash stores)
  pure (DrepHashId rawId, isNew)

resolveCommitteeHashIngest
  :: DedupStores -> ByteString -> CommitteeHash -> IO (CommitteeHashId, Bool)
resolveCommitteeHashIngest stores key _row = do
  (rawId, isNew) <- lookupOrInsert (SBS.toShort key) (dstCommitteeHash stores)
  pure (CommitteeHashId rawId, isNew)

-- | Caller passes 'AnchorType' so the signature matches the Follow
-- flavour; Ingest folds it into the dedup key upstream.
resolveVotingAnchorIngest
  :: DedupStores
  -> ByteString
  -> AnchorType
  -> VotingAnchor
  -> IO (VotingAnchorId, Bool)
resolveVotingAnchorIngest stores key _anchorType _row = do
  (rawId, isNew) <- lookupOrInsert (SBS.toShort key) (dstVotingAnchor stores)
  pure (VotingAnchorId rawId, isNew)

-- ---------------------------------------------------------------------------
-- * ExtractState-backed governance scratchpads
-- ---------------------------------------------------------------------------

lookupGovActionProposalIdIngest
  :: IORef ExtractState
  -> ByteString
  -> Word64
  -> IO (Maybe GovActionProposalId)
lookupGovActionProposalIdIngest ref txHash idx = do
  st <- readIORef ref
  pure $ GovActionProposalId <$> Map.lookup (txHash, idx) (esGovActionProposalCache st)

recordGovActionProposalIdIngest
  :: IORef ExtractState
  -> ByteString
  -> Word64
  -> GovActionProposalId
  -> IO ()
recordGovActionProposalIdIngest ref txHash idx gid =
  atomicModifyIORef' ref $ \st ->
    ( st { esGovActionProposalCache =
             Map.insert (txHash, idx) (getGovActionProposalId gid)
               (esGovActionProposalCache st)
         }
    , ()
    )

readEnactedEpochStateIdsIngest
  :: IORef ExtractState -> IO (Maybe Int64, Maybe Int64, Maybe Int64)
readEnactedEpochStateIdsIngest ref = do
  st <- readIORef ref
  pure (esCurrentCommitteeId st, esCurrentNoConfidenceId st, esCurrentConstitutionId st)

writeEnactedEpochStateIdsIngest
  :: IORef ExtractState
  -> (Maybe Int64, Maybe Int64, Maybe Int64)
  -> IO ()
writeEnactedEpochStateIdsIngest ref (cid, ncid, conid) =
  atomicModifyIORef' ref $ \st ->
    ( st { esCurrentCommitteeId    = cid
         , esCurrentNoConfidenceId = ncid
         , esCurrentConstitutionId = conid
         }
    , ()
    )

readGovExpiresAfterIngest :: IORef ExtractState -> IO (Maybe Word64)
readGovExpiresAfterIngest ref = esGovExpiresAfter <$> readIORef ref

writeGovExpiresAfterIngest :: IORef ExtractState -> Maybe Word64 -> IO ()
writeGovExpiresAfterIngest ref mv =
  atomicModifyIORef' ref $ \st -> (st { esGovExpiresAfter = mv }, ())
