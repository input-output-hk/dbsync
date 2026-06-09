-- | Follow 'IdResolver' fragments for the @off_chain_votes@
-- extractor.
--
-- Same shape as the Ingest fragment: a no-op in production because
-- the worker polls PG. The test resolver overrides this to capture
-- the call.
module DbSync.Phase.Following.Resolver.OffChainVotes
  ( enqueueVoteMetaFetchFollow
  ) where

import Cardano.Prelude

import DbSync.Worker.OffChain.Types (VotingAnchorRef)

enqueueVoteMetaFetchFollow :: VotingAnchorRef -> IO ()
enqueueVoteMetaFetchFollow _ = pure ()
