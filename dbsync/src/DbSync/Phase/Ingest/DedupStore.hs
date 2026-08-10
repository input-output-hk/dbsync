{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | LSM-backed deduplication stores for entity id assignment. Each
-- 'DedupStore' maps an entity's natural key to its database id, and
-- 'lookupOrInsert' allocates a new id on a miss.
--
-- Only one thread may access these: @lsm-tree@ rejects concurrent
-- writers on a table. A snapshot keeps the @(key, id)@ pairs but not
-- the counter, so a resumed boot runs
-- 'DbSync.SyncState.Row.rebuildDedupMaps' to raise it again.
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
  , lookupOnly
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

-- | One LSM table plus an in-process id counter. An 'IORef' holds
-- the table so 'compactDedupStore' can swap in a reopened handle
-- without changing the 'DedupStore' value that callers hold.
data DedupStore = DedupStore
  { dstTable        :: !(IORef (LSMTree.Table IO ShortByteString DedupIdBytes ByteString))
    -- ^ The blob type only makes 'LSMTree.insert' typecheck. Every
    -- call passes 'Nothing' for the optional blob.
  , dstCounter      :: !(IORef Int64)
    -- ^ Next id to allocate. 'lookupOrInsert' bumps it on a miss,
    -- and 'insertExisting' bumps it past an incoming id.
  , dstSnapshotName :: !LSMTree.SnapshotName
    -- ^ 'newStores' assigns a distinct name per store.
  , dstLabel        :: !LSMTree.SnapshotLabel
    -- ^ @lsm-tree@ rejects an open whose label differs from the save
    -- label.
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

-- | Wire-format wrapper around an 'Int64' value.
-- 'LSMTree.ResolveAsFirst' is safe here: two values collide on the
-- same key only on replay, and the replayed value is bit-identical
-- to the original.
newtype DedupIdBytes = DedupIdBytes ShortByteString
  deriving stock (Eq, Show)
  deriving newtype (LSMTree.SerialiseValue)
  deriving LSMTree.ResolveValue via LSMTree.ResolveAsFirst DedupIdBytes

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Restore the table from the session's snapshot of that name, or
-- create an empty one with 'defaultIngestTableConfig'. The counter
-- starts at 1 either way, and
-- 'DbSync.SyncState.Row.rebuildDedupMaps' bumps it past the existing
-- ids on a resumed boot.
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

-- | Closes the active table only. The session stays open.
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

-- | Every store in the aggregate, for a caller that applies one
-- lifecycle step across all of them.
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

-- | Look up a key. A new key allocates the next id, writes
-- @(key, id)@ to the table, and returns @(id, True)@. A known key
-- returns @(existingId, False)@. 'True' tells the caller to write a
-- new COPY row.
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

-- | Look up a key without inserting on a miss. 'Nothing' means the
-- key was never registered, which the slot-leader path reads as
-- "genesis-key leader, not a known pool".
lookupOnly :: ShortByteString -> DedupStore -> IO (Maybe Int64)
lookupOnly key store = do
  table  <- readIORef (dstTable store)
  result <- LSMTree.lookup table key
  pure $ case LSMTree.getValue result of
    Just bs -> decodeInt64 bs
    Nothing -> Nothing

-- | Insert a @(key, id)@ pair keeping the supplied id, and raise the
-- counter to @max(counter, id + 1)@, so a later 'lookupOrInsert'
-- cannot collide with a rebuilt entry. Boot calls this to repopulate
-- the stores from the rows PG already holds.
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

-- | O(1) size from the id counter, as @counter - 1@. A fresh run
-- gives the exact count. On a resumed run 'insertExisting' already
-- raised the counter, so this is an upper bound.
sizeApprox :: DedupStore -> IO Int
sizeApprox store = do
  cnt <- readIORef (dstCounter store)
  pure $ max 0 (fromIntegral cnt - 1)

-- | Approximate entry counts, named for log output.
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

-- | Flush the write buffer and replace the store's on-disk snapshot.
-- The snapshot hard-links the run files, so every epoch boundary can
-- run this as the warm-restart anchor. Resume correctness never
-- depends on it, because 'rebuildDedupMaps' repopulates every store
-- from PostgreSQL at boot.
persistDedupStore :: DedupStore -> LsmSession -> IO ()
persistDedupStore store lsm = mask_ $ do
  -- An interrupt between the delete and the save would lose the
  -- snapshot. The mask covers both, so a cancel lands either before
  -- any work or after a fresh snapshot exists.
  let session = lsmHandle lsm
  table <- readIORef (dstTable store)
  hasSnap <- LSMTree.doesSnapshotExist session (dstSnapshotName store)
  when hasSnap $ LSMTree.deleteSnapshot session (dstSnapshotName store)
  LSMTree.saveSnapshot (dstSnapshotName store) (dstLabel store) table

-- | 'persistDedupStore', then close the active table and reopen it
-- from the snapshot. This matches
-- 'DbSync.Phase.Ingest.UtxoStore.compactUtxoStore': it caps the
-- active run count and the open fds, but re-reads every run file, so
-- the boundary handler runs it on a coarse epoch cadence only.
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

-- | Encode an 'Int64' as 8 big-endian bytes. This builds them
-- directly: a 'Data.ByteString.Builder' round-trip allocates a 4KB
-- pinned chunk per call, and this runs once per new dedup entity,
-- plus once per row during the boot-time rebuild.
encodeInt64 :: Int64 -> DedupIdBytes
encodeInt64 i =
  DedupIdBytes . SBS.toShort $
    BSI.unsafeCreate 8 $ \p -> pokeWord64BE p 0 (fromIntegral i)

pokeWord64BE :: Ptr Word8 -> Int -> Word64 -> IO ()
pokeWord64BE p off w =
  for_ [0 .. 7] $ \i ->
    pokeByteOff p (off + i)
      (fromIntegral (w `shiftR` ((7 - i) * 8)) :: Word8)

-- | Inverse of 'encodeInt64'. 'Nothing' means a length mismatch,
-- which cannot happen for a value the store produced, so the call
-- site treats it as a cache miss.
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
