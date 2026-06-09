-- | Ingest 'IdResolver' fragments for the @off_chain_votes@
-- extractor.
--
-- The worker independently polls PG for anchors that lack a result,
-- so this hook is a no-op in production. The test resolver records
-- the call so the per-block extractor pass can be asserted on.
module DbSync.Phase.Ingest.Resolver.OffChainVotes
  ( enqueueVoteMetaFetchIngest
  ) where

import Cardano.Prelude

import DbSync.Worker.OffChain.Types (VotingAnchorRef)

enqueueVoteMetaFetchIngest :: VotingAnchorRef -> IO ()
enqueueVoteMetaFetchIngest _ = pure ()
