-- | Shared id-allocation helper used by every per-extractor Ingest
-- resolver.
module DbSync.Phase.Ingest.Resolver.Internal
  ( allocateNextId
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef')

import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounter, IdCounters, nextId)

-- | Read the next id from the counter field, advance it, and wrap
-- the raw 'Int64' in the matching newtype.
--
-- The setter takes the existing 'IdCounters' first, then the new
-- 'IdCounter', so a per-field setter reads as
-- @\\counters newCounter -> counters { fieldName = newCounter }@,
-- which matches a record update at the call site.
allocateNextId
  :: IORef ExtractState
  -> (IdCounters -> IdCounter)                  -- ^ getter for the per-table counter
  -> (IdCounters -> IdCounter -> IdCounters)    -- ^ setter that returns updated 'IdCounters'
  -> (Int64 -> a)                               -- ^ id constructor (e.g. 'TxId')
  -> IO a
allocateNextId extractStateRef getCounter setCounter mkId =
  atomicModifyIORef' extractStateRef $ \st ->
    let (rawId, nextCounter) = nextId (getCounter (esIdCounters st))
        st' = st { esIdCounters = setCounter (esIdCounters st) nextCounter }
     in (st', mkId rawId)
