-- | Deduplication maps for entity ID assignment.
--
-- During 'IngestChainHistory', dedup maps store only @key -> ID@ mappings
-- (not full entity data). They ensure that lookup/reference tables
-- (pool_hash, stake_address, multi_asset, etc.) assign a stable database ID
-- to each unique blockchain entity.
--
-- On first encounter: assign a new ID, store the mapping, write the full row
-- to the COPY stream. On subsequent encounters: look up the existing ID,
-- no new COPY row needed.
--
-- == Memory design
--
-- Keys are 'ShortByteString' (unpinned, GHC-managed heap) rather than
-- 'ByteString' (pinned, causes fragmentation GHC cannot compact).
-- For a 28-byte hash this saves ~112 bytes per key (~3.5x reduction).
--
-- Maps use mutable 'BasicHashTable' from the @hashtables@ package
-- rather than immutable 'HashMap'. This eliminates path-copying GC
-- pressure (~300 bytes of short-lived garbage per insert with HashMap).
module DbSync.Phase.Ingest.DedupMap
  ( -- * Types
    DedupMap
  , DedupMaps (..)

    -- * Construction
  , newDedupMap
  , newMaps

    -- * Operations
  , lookupOrInsert
  , insertExisting
  , size
  , sizeApprox
  , dedupMapSizes
  ) where

import Cardano.Prelude

import Data.ByteString.Short (ShortByteString)
import Data.HashTable.IO (BasicHashTable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified Data.HashTable.IO as HT

-- * Types

-- | Maps a blockchain entity's natural key to its assigned database ID.
-- Stores ONLY the key and the ID -- not the full entity data.
--
-- Uses a mutable hash table with unpinned 'ShortByteString' keys.
-- All operations are in 'IO'.
data DedupMap = DedupMap
  { dmTable   :: !(BasicHashTable ShortByteString Int64)
  , dmCounter :: !(IORef Int64)
  }

-- | All dedup maps used during 'IngestChainHistory'.
data DedupMaps = DedupMaps
  { dmsPoolHash     :: !DedupMap  -- ^ pool key hash -> PoolHashId
  , dmsStakeAddress :: !DedupMap  -- ^ stake credential hash -> StakeAddressId
  , dmsSlotLeader   :: !DedupMap  -- ^ slot leader identifier -> SlotLeaderId
  , dmsMultiAsset   :: !DedupMap  -- ^ blake2b-224 (policy_id ++ asset_name) -> MultiAssetId
  , dmsScriptHash   :: !DedupMap  -- ^ script hash -> ScriptId
  }

-- * Construction

-- | Create an empty dedup map with a counter starting from 1.
newDedupMap :: IO DedupMap
newDedupMap = DedupMap <$> HT.new <*> newIORef 1

-- | Create all dedup maps, empty, with counters starting from 1.
newMaps :: IO DedupMaps
newMaps = DedupMaps
  <$> newDedupMap
  <*> newDedupMap
  <*> newDedupMap
  <*> newDedupMap
  <*> newDedupMap

-- * Operations

-- | Look up a key. If new, assign the next ID and return @(id, True)@.
-- If existing, return @(id, False)@. The 'Bool' indicates whether
-- a new COPY row should be written.
--
-- Mutates the hash table and counter in-place -- no allocation of
-- intermediate map structures.
lookupOrInsert :: ShortByteString -> DedupMap -> IO (Int64, Bool)
lookupOrInsert key dm = do
  existing <- HT.lookup (dmTable dm) key
  case existing of
    Just eid -> pure (eid, False)
    Nothing -> do
      newId <- readIORef (dmCounter dm)
      writeIORef (dmCounter dm) $! newId + 1
      HT.insert (dmTable dm) key newId
      pure (newId, True)

-- | Insert a (key, id) pair retaining the supplied id, and bump the
-- counter to @max(currentCounter, id + 1)@ so that subsequent
-- 'lookupOrInsert' allocations don't collide with rebuilt entries.
--
-- Used at boot to repopulate dedup maps from rows already in PG.
insertExisting :: ShortByteString -> Int64 -> DedupMap -> IO ()
insertExisting key existingId dm = do
  HT.insert (dmTable dm) key existingId
  cur <- readIORef (dmCounter dm)
  when (existingId >= cur) $
    writeIORef (dmCounter dm) $! existingId + 1

-- | Number of unique entries in the map.
size :: DedupMap -> IO Int
size dm = do
  entries <- HT.toList (dmTable dm)
  pure (length entries)

-- | O(1) approximate size from the ID counter.
-- Returns @counter - 1@: exact on a fresh run; on a resumed run where
-- 'insertExisting' bumped the counter, this is an upper bound (the max
-- assigned ID). Cheap enough to call at every epoch boundary.
sizeApprox :: DedupMap -> IO Int
sizeApprox dm = do
  cnt <- readIORef (dmCounter dm)
  pure $ max 0 (fromIntegral cnt - 1)

-- | Approximate entry counts for every dedup map, named for log output.
dedupMapSizes :: DedupMaps -> IO [(Text, Int)]
dedupMapSizes maps = do
  pool   <- sizeApprox (dmsPoolHash maps)
  stake  <- sizeApprox (dmsStakeAddress maps)
  leader <- sizeApprox (dmsSlotLeader maps)
  asset  <- sizeApprox (dmsMultiAsset maps)
  script <- sizeApprox (dmsScriptHash maps)
  pure
    [ ("pool",        pool)
    , ("stake",       stake)
    , ("slot_leader", leader)
    , ("multi_asset", asset)
    , ("script",      script)
    ]
