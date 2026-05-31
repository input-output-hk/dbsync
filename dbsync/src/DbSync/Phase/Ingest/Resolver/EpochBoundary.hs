-- | Ingest 'IdResolver' fragments for the @epoch_boundary@ extractor.
module DbSync.Phase.Ingest.Resolver.EpochBoundary
  ( assignAdaPotsIdIngest
  , assignEpochParamIdIngest
  , assignEpochStateIdIngest
  , resolveCostModelIngest
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map

import DbSync.Db.Schema.EpochBoundary (CostModel)
import DbSync.Db.Schema.Ids
  ( AdaPotsId (..)
  , CostModelId (..)
  , EpochParamId (..)
  , EpochStateId (..)
  )
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..), nextId)
import DbSync.Phase.Ingest.Resolver.Internal (bump)

assignAdaPotsIdIngest :: IORef ExtractState -> IO AdaPotsId
assignAdaPotsIdIngest stRef = bump stRef icAdaPotsId (\cs c -> cs { icAdaPotsId = c }) AdaPotsId

assignEpochParamIdIngest :: IORef ExtractState -> IO EpochParamId
assignEpochParamIdIngest stRef = bump stRef icEpochParamId (\cs c -> cs { icEpochParamId = c }) EpochParamId

assignEpochStateIdIngest :: IORef ExtractState -> IO EpochStateId
assignEpochStateIdIngest stRef = bump stRef icEpochStateId (\cs c -> cs { icEpochStateId = c }) EpochStateId

-- | Dedup cost-model lookups via the in-memory cache on
-- 'ExtractState'. Cache lives in 'ExtractState' so resume
-- pre-population from the database surfaces here without threading
-- a separate IORef through the constructor.
resolveCostModelIngest
  :: IORef ExtractState -> ByteString -> CostModel -> IO (CostModelId, Bool)
resolveCostModelIngest stRef hash _cm = atomicModifyIORef' stRef $ \st ->
  case Map.lookup hash (esCostModelCache st) of
    Just existing -> (st, (CostModelId existing, False))
    Nothing ->
      let (i, ctr') = nextId (icCostModelId (esIdCounters st))
          st' = st
            { esIdCounters     = (esIdCounters st) { icCostModelId = ctr' }
            , esCostModelCache = Map.insert hash i (esCostModelCache st)
            }
      in (st', (CostModelId i, True))
