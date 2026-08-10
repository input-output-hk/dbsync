{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LSM-backed live UTxO cache mapping @(tx_hash, output_idx)@ to the
-- producing tx's @(TxId, TxOutId, value)@, one entry per unspent
-- output. 'DbSync.Extractor.Pipeline' fills it,
-- 'DbSync.Extractor.UTxO' reads it, and 'deleteConsumed' removes
-- spent outputs.
--
-- Only the consumer thread calls these operations: @lsm-tree@ rejects
-- concurrent writers on one table.
module DbSync.Phase.Ingest.UtxoStore
  ( -- * Types
    UtxoStore
  , UtxoTxEntry (..)
  , StoreStats (..)

    -- * Lifecycle
  , openUtxoStore
  , closeUtxoStore

    -- * Hot path
  , recordTx
  , lookupInput
  , deleteConsumed

    -- * Epoch boundary
  , persistUtxoStore
  , compactUtxoStore

    -- * Stats
  , readStoreStats

    -- * Wire format (exposed for testing)
  , UtxoOutputBytes (..)
  , encodeOutput
  , decodeOutput
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Foreign.Storable (pokeByteOff)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Database.LSMTree as LSMTree

import DbSync.Db.Schema.Ids (TxId (..), TxOutId (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Phase.Ingest.LsmSession
  ( LsmSession (..)
  , currentSnapshotName
  , defaultIngestTableConfig
  , ingestSnapshotLabel
  )

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Per-tx entry as it arrives from the pipeline. 'recordTx' splits
-- it into one LSM entry per output, keyed by @(tx_hash, idx)@.
data UtxoTxEntry = UtxoTxEntry
  { uteTxId    :: !TxId
  , uteOutputs :: !(Seq (TxOutId, DbLovelace))
    -- ^ Outputs in chain order, indexed by 'Word16' output index.
  }
  deriving stock (Eq, Show)

-- | Cumulative counters. The epoch boundary samples these for the
-- diagnostic log line.
data StoreStats = StoreStats
  { ssHits    :: !Word64
    -- ^ 'lookupInput' calls that found an entry.
  , ssMisses  :: !Word64
    -- ^ 'lookupInput' calls that found nothing.
  , ssInserts :: !Word64
    -- ^ Successful 'recordTx' calls, per tx, not per output.
  , ssDeletes :: !Word64
    -- ^ Successful 'deleteConsumed' calls, per output.
  }
  deriving stock (Eq, Show)

-- | Cache handle owning one table under the session passed to
-- 'openUtxoStore'. An 'IORef' holds the table so 'compactUtxoStore'
-- can swap in a reopened handle without changing the 'UtxoStore'
-- value that callers hold.
data UtxoStore = UtxoStore
  { usTable :: !(IORef (LSMTree.Table IO ShortByteString UtxoOutputBytes ByteString))
    -- ^ The blob type ('ByteString') is required for 'LSMTree.insert'
    -- to typecheck but never used — every call passes 'Nothing' for
    -- the optional blob.
  , usStats :: !(IORef StoreStats)
  }

-- | Wire-format wrapper around the encoded per-output value.
-- 'LSMTree.ResolveAsFirst' is safe here: two values collide on the
-- same @(tx_hash, idx)@ key only on replay, and the replayed value is
-- bit-identical to the original.
newtype UtxoOutputBytes = UtxoOutputBytes ShortByteString
  deriving stock (Eq, Show)
  deriving newtype (LSMTree.SerialiseValue)
  deriving LSMTree.ResolveValue via LSMTree.ResolveAsFirst UtxoOutputBytes

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Restore the table from the session's snapshot, or create an empty
-- one with 'defaultIngestTableConfig'.
openUtxoStore :: LsmSession -> IO UtxoStore
openUtxoStore lsm = do
  let session = lsmHandle lsm
  hasSnap <- LSMTree.doesSnapshotExist session currentSnapshotName
  table <-
    if hasSnap
      then LSMTree.openTableFromSnapshot session currentSnapshotName ingestSnapshotLabel
      else LSMTree.newTableWith defaultIngestTableConfig session
  tableRef <- newIORef table
  stats <- newIORef emptyStats
  pure UtxoStore { usTable = tableRef, usStats = stats }

-- | Closes the active table only. The session stays open.
closeUtxoStore :: UtxoStore -> IO ()
closeUtxoStore store = do
  table <- readIORef (usTable store)
  LSMTree.closeTable table

-- ---------------------------------------------------------------------------
-- Hot path
-- ---------------------------------------------------------------------------

-- | Insert one LSM entry per output, keyed by @(tx_hash, idx)@. A
-- later 'recordTx' on the same hash replaces those entries: 'insert'
-- carries the 'ResolveValue' constraint but never applies it, and
-- only 'upsert' does.
recordTx :: UtxoStore -> ByteString -> UtxoTxEntry -> IO ()
recordTx cache hash (UtxoTxEntry txId outputs)
  | Seq.null outputs = pure ()
  | otherwise = do
      table <- readIORef (usTable cache)
      let !entries = V.fromListN (Seq.length outputs)
            [ (mkKey hash (fromIntegral idx), encodeOutput txId outId val, Nothing)
            | (idx, (outId, val)) <- zip [0 :: Int ..] (toList outputs)
            ]
      LSMTree.inserts table entries
      modifyIORef' (usStats cache) $ \s ->
        s { ssInserts = ssInserts s + 1 }

-- | Look up the producer of an input. The triple holds the
-- producer's @tx.id@ (which @tx_in.tx_out_id@ stores, despite the
-- column name), the produced output's @tx_out.id@ for the
-- consumed-by UPDATE, and the value for the deposit calculation.
--
-- 'Nothing' means a cache miss. The caller then writes
-- @tx_out_id = NULL@ and the post-load resolve fills it in.
lookupInput
  :: UtxoStore
  -> ByteString   -- ^ producer tx hash
  -> Word16       -- ^ output index
  -> IO (Maybe (TxId, TxOutId, DbLovelace))
lookupInput cache hash idx = do
  let !key = mkKey hash idx
  table <- readIORef (usTable cache)
  result <- LSMTree.lookup table key
  case LSMTree.getValue result of
    Just val | Just out <- decodeOutput val -> do
      bumpHit
      pure (Just out)
    _ -> do
      bumpMiss
      pure Nothing
  where
    -- Plain (non-atomic) bumps: the consumer thread is the only
    -- writer; a cross-thread 'readStoreStats' tolerates a marginally
    -- stale snapshot.
    bumpHit  = modifyIORef' (usStats cache) $ \s ->
      s { ssHits   = ssHits   s + 1 }
    bumpMiss = modifyIORef' (usStats cache) $ \s ->
      s { ssMisses = ssMisses s + 1 }

-- | Remove a consumed output. The UTxO extractor calls this after a
-- regular input resolves, and after the collateral input of a
-- phase-2 failed tx resolves. LSM ignores a delete of a missing key,
-- so a stale call does no harm.
deleteConsumed :: UtxoStore -> ByteString -> Word16 -> IO ()
deleteConsumed cache hash idx = do
  let !key = mkKey hash idx
  table <- readIORef (usTable cache)
  LSMTree.delete table key
  modifyIORef' (usStats cache) $ \s ->
    s { ssDeletes = ssDeletes s + 1 }

-- ---------------------------------------------------------------------------
-- Epoch boundary
-- ---------------------------------------------------------------------------

-- | Flush the write buffer and replace the on-disk snapshot with the
-- table's current contents. The snapshot hard-links the run files
-- instead of copying them, so every epoch boundary can run this to
-- keep the restart anchor fresh.
persistUtxoStore :: UtxoStore -> LsmSession -> IO ()
persistUtxoStore store lsm = mask_ $ do
  -- An interrupt between the delete and the save would lose the
  -- snapshot. The mask covers both, so a cancel lands either before
  -- any work or after a fresh snapshot exists.
  let session = lsmHandle lsm
  table <- readIORef (usTable store)
  hasSnap <- LSMTree.doesSnapshotExist session currentSnapshotName
  when hasSnap $ LSMTree.deleteSnapshot session currentSnapshotName
  LSMTree.saveSnapshot currentSnapshotName ingestSnapshotLabel table

-- | 'persistUtxoStore', then close the active table and reopen it
-- from the snapshot. The reopen drops every run outside the
-- snapshot, which caps the active run and fd counts. It also
-- re-reads and CRC-checks every run file, so its cost grows with
-- store size — the boundary handler therefore runs this on a coarse
-- epoch cadence and 'persistUtxoStore' alone on the other
-- boundaries. The store must stay quiescent for the whole call.
compactUtxoStore :: UtxoStore -> LsmSession -> IO ()
compactUtxoStore store lsm = mask_ $ do
  persistUtxoStore store lsm
  let session = lsmHandle lsm
  oldTable <- readIORef (usTable store)
  newTable <-
    LSMTree.openTableFromSnapshot session currentSnapshotName ingestSnapshotLabel
  writeIORef (usTable store) newTable
  LSMTree.closeTable oldTable

-- ---------------------------------------------------------------------------
-- Stats
-- ---------------------------------------------------------------------------

readStoreStats :: UtxoStore -> IO StoreStats
readStoreStats = readIORef . usStats

emptyStats :: StoreStats
emptyStats = StoreStats 0 0 0 0

-- ---------------------------------------------------------------------------
-- Internal: wire format
-- ---------------------------------------------------------------------------

-- | Build the LSM key for one output: @hash (32 bytes) ++ idx
-- (Word16 BE, 2 bytes)@. The hash spreads the high 64 bits evenly
-- and the index suffix keeps that, so 'CompactIndex' stays optimal.
--
-- This builds the bytes directly, not through a
-- 'Data.ByteString.Builder': 'toLazyByteString' allocates a 4KB
-- pinned first chunk per call, and this runs once per output, input
-- and delete on the hot path.
mkKey :: ByteString -> Word16 -> ShortByteString
mkKey hash idx = SBS.toShort (hash <> idxBytes)
  where
    !idxBytes = BS.pack [fromIntegral (idx `shiftR` 8), fromIntegral idx]

-- | Encode one output's resolved row data. Fixed 24-byte layout:
--
-- @
-- 8 bytes  : txId    (Int64  big-endian)
-- 8 bytes  : outId   (Int64  big-endian)
-- 8 bytes  : value   (Word64 big-endian)
-- @
encodeOutput :: TxId -> TxOutId -> DbLovelace -> UtxoOutputBytes
encodeOutput tid oid val =
  UtxoOutputBytes . SBS.toShort $
    BSI.unsafeCreate 24 $ \p -> do
      pokeWord64BE p 0  (fromIntegral (getTxId tid))
      pokeWord64BE p 8  (fromIntegral (getTxOutId oid))
      pokeWord64BE p 16 (unDbLovelace val)

pokeWord64BE :: Ptr Word8 -> Int -> Word64 -> IO ()
pokeWord64BE p off w =
  for_ [0 .. 7] $ \i ->
    pokeByteOff p (off + i)
      (fromIntegral (w `shiftR` ((7 - i) * 8)) :: Word8)

-- | Inverse of 'encodeOutput'. 'Nothing' means a length mismatch,
-- which cannot happen for a value the cache produced, so the call
-- site treats it as a cache miss.
decodeOutput :: UtxoOutputBytes -> Maybe (TxId, TxOutId, DbLovelace)
decodeOutput (UtxoOutputBytes sbs)
  | SBS.length sbs /= 24 = Nothing
  | otherwise =
      -- Force each read here. The ids outlive this call by a whole
      -- epoch, because the ConsumedByBuffer holds one per resolved
      -- input until the boundary handoff. The newtypes erase at
      -- runtime, so a lazy read would retain a thunk plus the
      -- 24-byte payload for that entire window.
      let !tid = readInt64BE  sbs 0
          !oid = readInt64BE  sbs 8
          !val = readWord64BE sbs 16
      in Just (TxId tid, TxOutId oid, DbLovelace val)

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
