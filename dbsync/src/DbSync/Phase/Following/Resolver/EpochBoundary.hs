-- | Follow 'IdResolver' fragments for the @epoch_boundary@ extractor.
--
-- Follow-phase plumbing not landed for any of these fields; both
-- flavours use the same stubs.
module DbSync.Phase.Following.Resolver.EpochBoundary
  ( assignAdaPotsIdStub
  , assignEpochParamIdStub
  , assignEpochStateIdStub
  , resolveCostModelStub
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.EpochBoundary (CostModel)
import DbSync.Db.Schema.Ids (AdaPotsId, CostModelId, EpochParamId, EpochStateId)
import DbSync.Phase.Following.Resolver.Internal (todoResolve)

assignAdaPotsIdStub :: IO AdaPotsId
assignAdaPotsIdStub = todoResolve "assignAdaPotsId"

assignEpochParamIdStub :: IO EpochParamId
assignEpochParamIdStub = todoResolve "assignEpochParamId"

assignEpochStateIdStub :: IO EpochStateId
assignEpochStateIdStub = todoResolve "assignEpochStateId"

resolveCostModelStub :: ByteString -> CostModel -> IO (CostModelId, Bool)
resolveCostModelStub _ _ = todoResolve "resolveCostModel"
