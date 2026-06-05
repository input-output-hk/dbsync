-- | Ingest 'IdResolver' fragments for the @epoch_boundary@ extractor.
module DbSync.Phase.Ingest.Resolver.EpochBoundary
  ( resolveCostModelIngest
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map

import DbSync.Db.Schema.EpochBoundary (CostModel)
import DbSync.Db.Schema.Ids (CostModelId (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..), nextId)

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
