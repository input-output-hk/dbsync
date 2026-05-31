-- | Shared counter-bump helper used by every per-extractor Ingest
-- resolver.
module DbSync.Phase.Ingest.Resolver.Internal
  ( bump
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef')

import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounter, IdCounters, nextId)

-- | Atomically allocate the next id from the supplied counter field
-- and wrap it with the matching newtype constructor.
--
-- The setter takes the existing 'IdCounters' first, then the new
-- 'IdCounter', so per-field setters read as
-- @\\cs c -> cs { fieldName = c }@ — matching the visual order of a
-- record update at the call site.
bump
  :: IORef ExtractState
  -> (IdCounters -> IdCounter)
  -> (IdCounters -> IdCounter -> IdCounters)
  -> (Int64 -> a)
  -> IO a
bump stRef getCtr setCtr wrapId = atomicModifyIORef' stRef $ \st ->
  let (i, ctr') = nextId (getCtr (esIdCounters st))
      st' = st { esIdCounters = setCtr (esIdCounters st) ctr' }
  in (st', wrapId i)
