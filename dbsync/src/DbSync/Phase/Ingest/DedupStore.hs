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
-- 'DbSync.SyncState.Row.rebuildDedupMaps' calls
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
  , allDedupStores

    -- * Hot path
  , lookupOrInsert
  , insertExisting

    -- * Sizes
  , sizeApprox
  , dedupStoreSizes

    -- * Epoch boundary
  , persistDedupStore
  , compactDedupStore
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.Storable (pokeByteOff)
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

-- | The dedup stores used during 'IngestChainHistory'.
data DedupStores = DedupStores
  { dstPoolHash      :: !DedupStore  -- ^ pool key hash -> PoolHashId
  , dstStakeAddress  :: !DedupStore  -- ^ stake credential hash -> StakeAddressId
  , dstSlotLeader    :: !DedupStore  -- ^ slot leader identifier -> SlotLeaderId
  , dstMultiAsset    :: !DedupStore  -- ^ blake2b-224 (policy_id ++ asset_name) -> MultiAssetId
  , dstScriptHash    :: !DedupStore  -- ^ script hash -> ScriptId
  , dstDatum         :: !DedupStore  -- ^ datum hash -> DatumId
  , dstRedeemerData  :: !DedupStore  -- ^ redeemer-data hash -> RedeemerDataId
  , dstDrepHash      :: !DedupStore  -- ^ DRep cred hash (or abstract sentinel) -> DrepHashId
  , dstCommitteeHash :: !DedupStore  -- ^ committee cred hash + has_script byte -> CommitteeHashId
  , dstVotingAnchor  :: !DedupStore  -- ^ encoded (url, data_hash, type) -> VotingAnchorId
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
-- both cases; 'DbSync.SyncState.Row.rebuildDedupMaps' is
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

-- | Open every dedup store under the shared session.
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
  <*> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-datum")
                         (LSMTree.toSnapshotName "current-datum")
  <*> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-redeemer-data")
                         (LSMTree.toSnapshotName "current-redeemer-data")
  <*> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-drep-hash")
                         (LSMTree.toSnapshotName "current-drep-hash")
  <*> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-committee-hash")
                         (LSMTree.toSnapshotName "current-committee-hash")
  <*> openDedupStore lsm (LSMTree.SnapshotLabel "dedup-voting-anchor")
                         (LSMTree.toSnapshotName "current-voting-anchor")

-- | Every store in the aggregate, for callers that apply a uniform
-- lifecycle step (close \/ persist \/ compact) across all of them.
allDedupStores :: DedupStores -> [DedupStore]
allDedupStores ds =
  [ dstPoolHash      ds
  , dstStakeAddress  ds
  , dstSlotLeader    ds
  , dstMultiAsset    ds
  , dstScriptHash    ds
  , dstDatum         ds
  , dstRedeemerData  ds
  , dstDrepHash      ds
  , dstCommitteeHash ds
  , dstVotingAnchor  ds
  ]

-- | Close every store in the aggregate. The session stays open.
closeStores :: DedupStores -> IO ()
closeStores = traverse_ closeDedupStore . allDedupStores

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
  pool   <- sizeApprox (dstPoolHash      ds)
  stake  <- sizeApprox (dstStakeAddress  ds)
  leader <- sizeApprox (dstSlotLeader    ds)
  asset  <- sizeApprox (dstMultiAsset    ds)
  script <- sizeApprox (dstScriptHash    ds)
  datum  <- sizeApprox (dstDatum         ds)
  rdata  <- sizeApprox (dstRedeemerData  ds)
  drep   <- sizeApprox (dstDrepHash      ds)
  comm   <- sizeApprox (dstCommitteeHash ds)
  anchor <- sizeApprox (dstVotingAnchor  ds)
  pure
    [ ("pool",           pool)
    , ("stake",          stake)
    , ("slot_leader",    leader)
    , ("multi_asset",    asset)
    , ("script",         script)
    , ("datum",          datum)
    , ("redeemer_data",  rdata)
    , ("drep_hash",      drep)
    , ("committee_hash", comm)
    , ("voting_anchor",  anchor)
    ]

-- ---------------------------------------------------------------------------
-- Epoch boundary
-- ---------------------------------------------------------------------------

-- | Flush the write buffer and atomically replace the store's
-- on-disk snapshot. Cheap (run files are hard-linked); runs at
-- every epoch boundary as the warm-restart anchor. Resume
-- correctness never depends on it — 'rebuildDedupMaps' repopulates
-- every store from PostgreSQL at boot.
persistDedupStore :: DedupStore -> LsmSession -> IO ()
persistDedupStore store lsm = mask_ $ do
  -- delete-then-save would lose this table's snapshot if interrupted
  -- between the two calls; mask covers the whole cycle so cancel
  -- either lands before any work or after a fresh snapshot exists.
  let session = lsmHandle lsm
  table <- readIORef (dstTable store)
  hasSnap <- LSMTree.doesSnapshotExist session (dstSnapshotName store)
  when hasSnap $ LSMTree.deleteSnapshot session (dstSnapshotName store)
  LSMTree.saveSnapshot (dstSnapshotName store) (dstLabel store) table

-- | 'persistDedupStore', then close the active table and reopen it
-- from the snapshot, swapping the handle in 'dstTable'.
--
-- Same shape and rationale as
-- 'DbSync.Phase.Ingest.UtxoStore.compactUtxoStore': caps the active
-- LSM run count (and hence open file descriptors) at the price of a
-- full re-read of the table's run files, so the boundary handler
-- runs it on a coarse epoch cadence only.
compactDedupStore :: DedupStore -> LsmSession -> IO ()
compactDedupStore store lsm = mask_ $ do
  persistDedupStore store lsm
  let session = lsmHandle lsm
  oldTable <- readIORef (dstTable store)
  newTable <-
    LSMTree.openTableFromSnapshot session (dstSnapshotName store) (dstLabel store)
  writeIORef (dstTable store) newTable
  LSMTree.closeTable oldTable

-- ---------------------------------------------------------------------------
-- Internal: wire format
-- ---------------------------------------------------------------------------

-- | Encode an 'Int64' as 8 big-endian bytes. Built directly — a
-- 'Data.ByteString.Builder' round-trip allocates a ~4KB pinned
-- chunk per call, and this runs once per new dedup entity plus once
-- per existing row during the boot-time 'insertExisting' rebuild.
encodeInt64 :: Int64 -> DedupIdBytes
encodeInt64 i =
  DedupIdBytes . SBS.toShort $
    BSI.unsafeCreate 8 $ \p -> pokeWord64BE p 0 (fromIntegral i)

pokeWord64BE :: Ptr Word8 -> Int -> Word64 -> IO ()
pokeWord64BE p off w =
  for_ [0 .. 7] $ \i ->
    pokeByteOff p (off + i)
      (fromIntegral (w `shiftR` ((7 - i) * 8)) :: Word8)

-- | Inverse of 'encodeInt64'. 'Nothing' on a length mismatch —
-- should never happen for a value the store itself produced, so
-- the call site treats 'Nothing' as a cache miss.
decodeInt64 :: DedupIdBytes -> Maybe Int64
decodeInt64 (DedupIdBytes sbs)
  | SBS.length sbs /= 8 = Nothing
  | otherwise           = Just (readInt64BE sbs 0)

readInt64BE :: ShortByteString -> Int -> Int64
readInt64BE sbs off = fromIntegral (readWord64BE sbs off)

readWord64BE :: ShortByteString -> Int -> Word64
readWord64BE sbs off =
    fromIntegral (SBS.index sbs (off + 0)) `shiftL` 56
  + fromIntegral (SBS.index sbs (off + 1)) `shiftL` 48
  + fromIntegral (SBS.index sbs (off + 2)) `shiftL` 40
  + fromIntegral (SBS.index sbs (off + 3)) `shiftL` 32
  + fromIntegral (SBS.index sbs (off + 4)) `shiftL` 24
  + fromIntegral (SBS.index sbs (off + 5)) `shiftL` 16
  + fromIntegral (SBS.index sbs (off + 6)) `shiftL` 8
  + fromIntegral (SBS.index sbs (off + 7))
