{-# LANGUAGE OverloadedStrings #-}

-- | Owns the seven @off_chain_vote_*@ tables. The per-block pass only
-- reports the governance anchors on proposals, votes, DRep
-- registrations, committee resignations and constitution updates,
-- through 'enqueueVoteMetaFetch'. 'DbSync.Worker.OffChain.Vote' writes
-- the result rows.
module DbSync.Extractor.OffChainVotes
  ( offChainVotesExtractor
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.OffChainVote
  ( offChainVoteAuthorTableDef
  , offChainVoteDataTableDef
  , offChainVoteDrepDataTableDef
  , offChainVoteExternalUpdateTableDef
  , offChainVoteFetchErrorTableDef
  , offChainVoteGovActionDataTableDef
  , offChainVoteReferenceTableDef
  )
import DbSync.Db.Types (AnchorType (..))
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , ProcessBlockFn
  , TxContext (..)
  )
import DbSync.Parser.Types
  ( AnchorData (..)
  , CertAction (..)
  , GenericGovAction (..)
  , GenericGovActionProposal (..)
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericVotingProcedure (..)
  )
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Worker.OffChain.Types (VotingAnchorRef (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

offChainVotesExtractor :: ExtractorDef
offChainVotesExtractor = ExtractorDef
  { pdName    = "off_chain_votes"
  , pdTables  =
      [ offChainVoteDataTableDef
      , offChainVoteGovActionDataTableDef
      , offChainVoteDrepDataTableDef
      , offChainVoteAuthorTableDef
      , offChainVoteReferenceTableDef
      , offChainVoteExternalUpdateTableDef
      , offChainVoteFetchErrorTableDef
      ]
  , pdProcess = processOffChainVotes
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processOffChainVotes :: ProcessBlockFn
processOffChainVotes ctx = do
  resolver <- asks getResolver
  forM_ (bcTxs ctx) $ \tc -> when (txValidContract (tcGenTx tc)) $ do
    let tx = tcGenTx tc
    forM_ (txProposals tx)        (enqueueProposalAnchors resolver)
    forM_ (txVotingProcedures tx) (enqueueVoteAnchor      resolver)
    forM_ (txCertificates tx)     (enqueueCertAnchor      resolver)

-- | Enqueue the proposal's main anchor and, for a 'GovNewConstitution'
-- action, the embedded constitution anchor.
enqueueProposalAnchors :: MonadIO m => IdResolver IO -> GenericGovActionProposal -> m ()
enqueueProposalAnchors resolver prop = do
  enqueue resolver GovActionAnchor (ggapAnchor prop)
  case ggapAction prop of
    GovNewConstitution _ ca _ -> enqueue resolver ConstitutionAnchor ca
    _                         -> pure ()

enqueueVoteAnchor :: MonadIO m => IdResolver IO -> GenericVotingProcedure -> m ()
enqueueVoteAnchor resolver vp =
  forM_ (gvpAnchor vp) (enqueue resolver VoteAnchor)

enqueueCertAnchor :: MonadIO m => IdResolver IO -> GenericTxCertificate -> m ()
enqueueCertAnchor resolver cert = case txCertAction cert of
  CertDRepRegistration _ _ (Just a) -> enqueue resolver DrepAnchor           a
  CertDRepUpdate       _   (Just a) -> enqueue resolver DrepAnchor           a
  CertCommitteeResign  _   (Just a) -> enqueue resolver CommitteeDeRegAnchor a
  _                                 -> pure ()

enqueue :: MonadIO m => IdResolver IO -> AnchorType -> AnchorData -> m ()
enqueue resolver anchorType a =
  liftIO $ enqueueVoteMetaFetch resolver $ VotingAnchorRef
    { varUrl        = adUrl a
    , varMetaHash   = adHash a
    , varAnchorType = anchorType
    }
