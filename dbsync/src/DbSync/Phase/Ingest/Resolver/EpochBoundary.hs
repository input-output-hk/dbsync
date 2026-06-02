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
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)

assignAdaPotsIdIngest :: IORef ExtractState -> IO AdaPotsId
assignAdaPotsIdIngest extractStateRef =
  allocateNextId extractStateRef icAdaPotsId (\cs c -> cs { icAdaPotsId = c }) AdaPotsId

assignEpochParamIdIngest :: IORef ExtractState -> IO EpochParamId
assignEpochParamIdIngest extractStateRef =
  allocateNextId extractStateRef icEpochParamId (\cs c -> cs { icEpochParamId = c }) EpochParamId

assignEpochStateIdIngest :: IORef ExtractState -> IO EpochStateId
assignEpochStateIdIngest extractStateRef =
  allocateNextId extractStateRef icEpochStateId (\cs c -> cs { icEpochStateId = c }) EpochStateId

-- | Dedup cost-model lookups via the in-memory cache on
-- 'ExtractState'. Cache lives in 'ExtractState' so resume
-- pre-population from the database surfaces here without threading
-- a separate IORef through the constructor.
resolveCostModelIngest
  :: IORef ExtractState -> ByteString -> CostModel -> IO (CostModelId, Bool)
resolveCostModelIngest extractStateRef hash _cm =
  atomicModifyIORef' extractStateRef $ \st ->
    case Map.lookup hash (esCostModelCache st) of
      Just existing -> (st, (CostModelId existing, False))
      Nothing ->
        let (rawId, nextCounter) = nextId (icCostModelId (esIdCounters st))
            st' = st
              { esIdCounters     = (esIdCounters st) { icCostModelId = nextCounter }
              , esCostModelCache = Map.insert hash rawId (esCostModelCache st)
              }
         in (st', (CostModelId rawId, True))
