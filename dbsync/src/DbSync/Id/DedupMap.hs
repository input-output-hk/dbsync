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
module DbSync.Id.DedupMap
  ( -- * Types
    DedupMap (..)
  , DedupMaps (..)

    -- * Construction
  , empty
  , emptyMaps

    -- * Operations
  , lookupOrInsert
  , DbSync.Id.DedupMap.lookup
  , size
  ) where

import Cardano.Prelude hiding (empty)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import DbSync.Id.Counter (IdCounter, mkIdCounter, nextId)

-- * Types

-- | Maps a blockchain entity's natural key to its assigned database ID.
-- Stores ONLY the key and the ID — not the full entity data.
data DedupMap = DedupMap
  { dmMap     :: !(HashMap ByteString Int64)
  , dmCounter :: !IdCounter
  }
  deriving stock (Show)

-- | All dedup maps used during 'IngestChainHistory'.
data DedupMaps = DedupMaps
  { dmsPoolHash     :: !DedupMap  -- ^ pool key hash -> PoolHashId
  , dmsStakeAddress :: !DedupMap  -- ^ stake credential hash -> StakeAddressId
  , dmsSlotLeader   :: !DedupMap  -- ^ slot leader identifier -> SlotLeaderId
  , dmsMultiAsset   :: !DedupMap  -- ^ (policy_id ++ asset_name) -> MultiAssetId
  , dmsScriptHash   :: !DedupMap  -- ^ script hash -> ScriptId
  }
  deriving stock (Show)

-- * Construction

-- | Create an empty dedup map with a counter starting from 1.
empty :: DedupMap
empty = DedupMap HashMap.empty (mkIdCounter 1)

-- | Create all dedup maps, empty, with counters starting from 1.
emptyMaps :: DedupMaps
emptyMaps = DedupMaps empty empty empty empty empty

-- * Operations

-- | Look up a key. If new, assign the next ID and return @(id, True, updatedMap)@.
-- If existing, return @(id, False, unchangedMap)@. The 'Bool' indicates whether
-- a new COPY row should be written.
lookupOrInsert :: ByteString -> DedupMap -> (Int64, Bool, DedupMap)
lookupOrInsert key dm@(DedupMap m counter) =
  case HashMap.lookup key m of
    Just existingId -> (existingId, False, dm)
    Nothing ->
      let (newId, counter') = nextId counter
          m' = HashMap.insert key newId m
       in (newId, True, DedupMap m' counter')

-- | Look up without inserting. Returns 'Nothing' if not seen before.
lookup :: ByteString -> DedupMap -> Maybe Int64
lookup key (DedupMap m _) = HashMap.lookup key m

-- | Number of unique entries in the map.
size :: DedupMap -> Int
size (DedupMap m _) = HashMap.size m
