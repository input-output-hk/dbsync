{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | LSM-backed deduplication stores for entity ID assignment.
--
-- Each 'DedupStore' maps a blockchain entity's natural key
-- ('ShortByteString') to its assigned database ID ('Int64'). On
-- first encounter of a key the store allocates the next id from an
-- in-process counter, writes the @(key, id)@ pair to its LSM table,
-- and reports @isNew = True@ so the caller emits the matching COPY
-- row; subsequent encounters return the existing id with @isNew =
-- False@.
--
-- 'DedupStores' aggregates the five distinct kinds of dedup table
-- used during 'IngestChainHistory' — pool hash, stake address, slot
-- leader, multi-asset, script hash. All five live in the shared
-- 'LsmSession' under distinct snapshot labels so they coexist
-- without name collisions.
--
-- == Wire format
--
-- Values are a fixed 8-byte big-endian 'Int64'. No length prefix.
-- See 'encodeInt64' / 'decodeInt64'.
--
-- == Threading
--
-- All operations are called from a single thread (the consumer
-- thread for the hot path; the boot thread for 'newStores' and
-- 'rebuildDedupMaps' restore). @lsm-tree@ rejects concurrent writers
-- on a single table.
--
-- == Counter persistence
--
-- The LSM snapshot persists the @(key, id)@ table contents but
-- /not/ the next-id counter. On a resumed boot
-- 'DbSync.Checkpoint.SyncState.rebuildDedupMaps' calls
-- 'insertExisting' once per existing row, which raises the counter
-- to @max(existingId) + 1@.
module DbSync.Phase.Ingest.DedupStore
  ( -- * Types
    DedupStore
  , DedupStores (..)

    -- * Lifecycle
  , openDedupStore
  , closeDedupStore
  , newStores
  , closeStores

    -- * Hot path
  , lookupOrInsert
  , insertExisting

    -- * Sizes
  , sizeApprox
  , dedupStoreSizes

    -- * Epoch boundary
  , compactDedupStore
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Database.LSMTree as LSMTree

import DbSync.Phase.Ingest.LsmSession
  ( LsmSession (..)
  , defaultIngestTableConfig
  )

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A single dedup store: one LSM table plus an in-process id
-- counter.
--
-- The table handle is wrapped in an 'IORef' so 'compactDedupStore'
-- can atomically replace it with a freshly-opened-from-snapshot
-- handle without changing the 'DedupStore' value held by callers.
data DedupStore = DedupStore
  { dstTable        :: !(IORef (LSMTree.Table IO ShortByteString DedupIdBytes ByteString))
    -- ^ The blob type ('ByteString') is required for 'LSMTree.insert'
    -- to typecheck but never used — every call passes 'Nothing' for
    -- the optional blob.
  , dstCounter      :: !(IORef Int64)
    -- ^ Next id to allocate. Bumped by 'lookupOrInsert' on a miss
    -- and by 'insertExisting' when the incoming id is at or past
    -- the current value.
  , dstSnapshotName :: !LSMTree.SnapshotName
    -- ^ Per-store snapshot name. See 'newStores' for the five
    -- distinct names.
  , dstLabel        :: !LSMTree.SnapshotLabel
    -- ^ Per-store snapshot label. @lsm-tree@ rejects an open whose
    -- label differs from the save label.
  }

-- | The five dedup stores used during 'IngestChainHistory'.
data DedupStores = DedupStores
  { dstPoolHash     :: !DedupStore  -- ^ pool key hash -> PoolHashId
  , dstStakeAddress :: !DedupStore  -- ^ stake credential hash -> StakeAddressId
  , dstSlotLeader   :: !DedupStore  -- ^ slot leader identifier -> SlotLeaderId
  , dstMultiAsset   :: !DedupStore  -- ^ blake2b-224 (policy_id ++ asset_name) -> MultiAssetId
  , dstScriptHash   :: !DedupStore  -- ^ script hash -> ScriptId
  }

-- | Wire-format wrapper around an 'Int64' value. The
-- 'ResolveValue' instance is 'LSMTree.ResolveAsFirst' — collisions
-- on the same key only happen on replay, and the replayed value is
-- bit-identical to the original.
newtype DedupIdBytes = DedupIdBytes ShortByteString
  deriving stock (Eq, Show)
  deriving newtype (LSMTree.SerialiseValue)
  deriving LSMTree.ResolveValue via LSMTree.ResolveAsFirst DedupIdBytes

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Open one dedup store under the given session.
--
-- If the session already has a snapshot saved under the supplied
-- name, restore from it; otherwise create a fresh empty table with
-- 'defaultIngestTableConfig'. The counter is initialised to 1 in
-- both cases; 'DbSync.Checkpoint.SyncState.rebuildDedupMaps' is
-- responsible for bumping it past existing ids on a resumed boot.
openDedupStore
  :: LsmSession
  -> LSMTree.SnapshotLabel
  -> LSMTree.SnapshotName
  -> IO DedupStore
openDedupStore lsm label name = do
  let session = lsmHandle lsm
  hasSnap <- LSMTree.doesSnapshotExist session name
  table <-
    if hasSnap
      then LSMTree.openTableFromSnapshot session name label
      else LSMTree.newTableWith defaultIngestTableConfig session
  tableRef   <- newIORef table
  counterRef <- newIORef 1
  pure DedupStore
    { dstTable        = tableRef
    , dstCounter      = counterRef
    , dstSnapshotName = name
    , dstLabel        = label
    }

-- | Close the store's currently-active table. The session it lives
-- in is not touched.
closeDedupStore :: DedupStore -> IO ()
closeDedupStore store = do
  table <- readIORef (dstTable store)
  LSMTree.closeTable table

-- | Open all five dedup stores under the shared session.
newStores :: LsmSession -> IO DedupStores
newStores lsm = DedupStores
  <$> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-pool-hash")
                         (LSMTree.toSnapshotName "current-pool-hash")
  <*> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-stake-address")
                         (LSMTree.toSnapshotName "current-stake-address")
  <*> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-slot-leader")
                         (LSMTree.toSnapshotName "current-slot-leader")
  <*> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-multi-asset")
                         (LSMTree.toSnapshotName "current-multi-asset")
  <*> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-script-hash")
                         (LSMTree.toSnapshotName "current-script-hash")

-- | Close every store in the aggregate. The session stays open.
closeStores :: DedupStores -> IO ()
closeStores ds = do
  closeDedupStore (dstPoolHash     ds)
  closeDedupStore (dstStakeAddress ds)
  closeDedupStore (dstSlotLeader   ds)
  closeDedupStore (dstMultiAsset   ds)
  closeDedupStore (dstScriptHash   ds)

-- ---------------------------------------------------------------------------
-- Hot path
-- ---------------------------------------------------------------------------

-- | Look up a key. If new, allocate the next id, write
-- @(key, id)@ to the LSM table, and return @(id, True)@. If
-- existing, return @(existingId, False)@. The 'Bool' indicates
-- whether a new COPY row should be written.
lookupOrInsert :: ShortByteString -> DedupStore -> IO (Int64, Bool)
lookupOrInsert key store = do
  table  <- readIORef (dstTable store)
  result <- LSMTree.lookup table key
  case LSMTree.getValue result of
    Just bs | Just i <- decodeInt64 bs -> pure (i, False)
    _ -> do
      newId <- readIORef (dstCounter store)
      writeIORef (dstCounter store) $! newId + 1
      LSMTree.insert table key (encodeInt64 newId) Nothing
      pure (newId, True)

-- | Insert a @(key, id)@ pair retaining the supplied id, and bump
-- the counter to @max(currentCounter, id + 1)@ so subsequent
-- 'lookupOrInsert' allocations don't collide with rebuilt entries.
--
-- Used at boot to repopulate dedup stores from rows already in PG.
insertExisting :: ShortByteString -> Int64 -> DedupStore -> IO ()
insertExisting key existingId store = do
  table <- readIORef (dstTable store)
  LSMTree.insert table key (encodeInt64 existingId) Nothing
  cur <- readIORef (dstCounter store)
  when (existingId >= cur) $
    writeIORef (dstCounter store) $! existingId + 1

-- ---------------------------------------------------------------------------
-- Sizes
-- ---------------------------------------------------------------------------

-- | O(1) approximate size derived from the id counter.
-- Returns @counter - 1@: exact on a fresh run; on a resumed run
-- where 'insertExisting' bumped the counter, this is an upper bound
-- (the max assigned id). Cheap enough to call at every epoch
-- boundary.
sizeApprox :: DedupStore -> IO Int
sizeApprox store = do
  cnt <- readIORef (dstCounter store)
  pure $ max 0 (fromIntegral cnt - 1)

-- | Approximate entry counts for every dedup store, named for log
-- output.
dedupStoreSizes :: DedupStores -> IO [(Text, Int)]
dedupStoreSizes ds = do
  pool   <- sizeApprox (dstPoolHash     ds)
  stake  <- sizeApprox (dstStakeAddress ds)
  leader <- sizeApprox (dstSlotLeader   ds)
  asset  <- sizeApprox (dstMultiAsset   ds)
  script <- sizeApprox (dstScriptHash   ds)
  pure
    [ ("pool",        pool)
    , ("stake",       stake)
    , ("slot_leader", leader)
    , ("multi_asset", asset)
    , ("script",      script)
    ]

-- ---------------------------------------------------------------------------
-- Epoch boundary
-- ---------------------------------------------------------------------------

-- | Snapshot the store's current table, then close it and reopen
-- from the new snapshot, swapping the active handle in 'dstTable'.
--
-- Same shape as 'DbSync.Phase.Ingest.UtxoStore.compactUtxoStore':
-- caps the active LSM run count (and hence open file descriptors)
-- and durabilises a restart-resume anchor.
compactDedupStore :: DedupStore -> LsmSession -> IO ()
compactDedupStore store lsm = do
  let session = lsmHandle lsm
      name    = dstSnapshotName store
      label   = dstLabel        store
  oldTable <- readIORef (dstTable store)
  hasSnap  <- LSMTree.doesSnapshotExist session name
  when hasSnap $ LSMTree.deleteSnapshot session name
  LSMTree.saveSnapshot name label oldTable
  -- Open the new table and publish it before closing the old one,
  -- so an async exception between open and swap can't strand the
  -- new handle. The old table's runs survive in the snapshot dir
  -- as hardlinks once 'closeTable' unlinks the active-dir entries.
  mask_ $ do
    newTable <- LSMTree.openTableFromSnapshot session name label
    writeIORef (dstTable store) newTable
  LSMTree.closeTable oldTable

-- ---------------------------------------------------------------------------
-- Internal: wire format
-- ---------------------------------------------------------------------------

-- | Encode an 'Int64' as 8 big-endian bytes.
encodeInt64 :: Int64 -> DedupIdBytes
encodeInt64 i =
  DedupIdBytes
    . SBS.toShort
    . LBS.toStrict
    . BB.toLazyByteString
    $ BB.int64BE i

-- | Inverse of 'encodeInt64'. 'Nothing' on a length mismatch —
-- should never happen for a value the store itself produced, so
-- the call site treats 'Nothing' as a cache miss.
decodeInt64 :: DedupIdBytes -> Maybe Int64
decodeInt64 (DedupIdBytes sbs)
  | BS.length bs /= 8 = Nothing
  | otherwise         = Just (readInt64BE bs 0)
  where
    bs = SBS.fromShort sbs

readInt64BE :: ByteString -> Int -> Int64
readInt64BE bs off = fromIntegral (readWord64BE bs off)

readWord64BE :: ByteString -> Int -> Word64
readWord64BE bs off =
    fromIntegral (BS.index bs (off + 0)) `shiftL` 56
  + fromIntegral (BS.index bs (off + 1)) `shiftL` 48
  + fromIntegral (BS.index bs (off + 2)) `shiftL` 40
  + fromIntegral (BS.index bs (off + 3)) `shiftL` 32
  + fromIntegral (BS.index bs (off + 4)) `shiftL` 24
  + fromIntegral (BS.index bs (off + 5)) `shiftL` 16
  + fromIntegral (BS.index bs (off + 6)) `shiftL` 8
  + fromIntegral (BS.index bs (off + 7))
