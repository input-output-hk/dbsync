-- | Follow 'IdResolver' fragments for the @epoch_boundary@ extractor.
--
-- Only the dedup-style 'CostModel' lookup keeps an explicit resolver
-- field — the rest of the boundary tables allocate ids via PostgreSQL
-- @IDENTITY@ columns so they need no Follow-side assigner.
module DbSync.Phase.Following.Resolver.EpochBoundary
  ( resolveCostModelStub
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.EpochBoundary (CostModel)
import DbSync.Db.Schema.Ids (CostModelId)
import DbSync.Phase.Following.Resolver.Internal (todoResolve)

resolveCostModelStub :: ByteString -> CostModel -> IO (CostModelId, Bool)
resolveCostModelStub _ _ = todoResolve "resolveCostModel"
