-- | Ingest 'IdResolver' fragments for the @off_chain_pools@
-- extractor.
--
-- The worker independently polls PG for refs that lack a result, so
-- this hook is a no-op in production. The test resolver records the
-- call so the per-block extractor pass can be asserted on.
module DbSync.Phase.Ingest.Resolver.OffChainPools
  ( enqueuePoolMetaFetchIngest
  ) where

import Cardano.Prelude

import DbSync.Worker.OffChain.Types (PoolMetadataRef)

enqueuePoolMetaFetchIngest :: PoolMetadataRef -> IO ()
enqueuePoolMetaFetchIngest _ = pure ()
