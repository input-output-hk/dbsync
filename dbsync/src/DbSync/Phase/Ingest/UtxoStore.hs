{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LSM-backed live UTxO cache mapping @(tx_hash, output_idx)@ to
-- the producing tx's @(TxId, TxOutId, value)@.
--
-- One entry per unspent output. Populated as each tx is processed
-- by 'DbSync.Block.Pipeline'; consulted by 'DbSync.Extractor.UTxO'
-- when resolving inputs; entries are removed by 'deleteConsumed'
-- when a regular input consumes them (or when a phase-2 failed
-- tx's collateral is consumed).
--
-- The active LSM table is held behind an 'IORef' because
-- 'compactUtxoStore' swaps it at every epoch boundary; readers \/
-- writers dereference on each call. All operations are called from
-- the consumer thread only — @lsm-tree@ rejects concurrent writers
-- on a single table.
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
  , compactUtxoStore

    -- * Stats
  , readStoreStats
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
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

-- | Per-tx entry as it arrives from the pipeline: producer 'TxId'
-- plus, for every output, its 'TxOutId' and lovelace value.
--
-- 'recordTx' splits this into one LSM entry per output, keyed by
-- @(tx_hash, idx)@.
data UtxoTxEntry = UtxoTxEntry
  { uteTxId    :: !TxId
  , uteOutputs :: !(Seq (TxOutId, DbLovelace))
    -- ^ Outputs in chain order, indexed by 'Word16' output index.
  }
  deriving stock (Eq, Show)

-- | Cumulative counters. Sampled at epoch boundaries for the
-- diagnostic log line.
data StoreStats = StoreStats
  { ssHits    :: !Word64
  , ssMisses  :: !Word64
  , ssInserts :: !Word64
    -- ^ Total successful 'recordTx' calls (per tx, not per output).
  , ssDeletes :: !Word64
    -- ^ Total successful 'deleteConsumed' calls (per output).
  }
  deriving stock (Eq, Show)

-- | Cache handle. Owns one table under the session passed to
-- 'openUtxoStore'; the session itself is owned by
-- 'DbSync.Env.IngestEnv' \/ closed at App-level shutdown.
--
-- The table handle is wrapped in an 'IORef' so 'compactUtxoStore'
-- can atomically replace it with a freshly-opened-from-snapshot
-- handle without changing the 'UtxoStore' value held by callers.
data UtxoStore = UtxoStore
  { usTable :: !(IORef (LSMTree.Table IO ShortByteString UtxoOutputBytes ByteString))
    -- ^ The blob type ('ByteString') is required for 'LSMTree.insert'
    -- to typecheck but never used — every call passes 'Nothing' for
    -- the optional blob.
  , usStats :: !(IORef StoreStats)
  }

-- | Wire-format wrapper around the encoded per-output value. The
-- 'ResolveValue' instance is 'LSMTree.ResolveAsFirst' — collisions
-- on the same @(tx_hash, idx)@ key only happen on replay, and the
-- replayed value is bit-identical to the original.
newtype UtxoOutputBytes = UtxoOutputBytes ShortByteString
  deriving stock (Eq, Show)
  deriving newtype (LSMTree.SerialiseValue)
  deriving LSMTree.ResolveValue via LSMTree.ResolveAsFirst UtxoOutputBytes

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Open the store's table.
--
-- If the session has a saved snapshot, restore from it; otherwise
-- create a fresh empty table with 'defaultIngestTableConfig'.
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

-- | Close the cache's currently-active table. The session it lives
-- in is not touched.
closeUtxoStore :: UtxoStore -> IO ()
closeUtxoStore store = do
  table <- readIORef (usTable store)
  LSMTree.closeTable table

-- ---------------------------------------------------------------------------
-- Hot path
-- ---------------------------------------------------------------------------

-- | Record a tx and its outputs in the cache.
--
-- Inserts one LSM entry per output, keyed by @(tx_hash, idx)@. A
-- subsequent 'recordTx' on the same hash replaces those entries —
-- the 'ResolveValue' constraint is type-system-only for 'insert',
-- only 'upsert' uses it.
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
      atomicModifyIORef' (usStats cache) $ \s ->
        (s { ssInserts = ssInserts s + 1 }, ())

-- | Look up the producer of an input by @(tx_hash, output_idx)@.
--
-- Returns the producer's @tx.id@ (for @tx_in.tx_out_id@ — yes, the
-- column name lags the meaning), the producer-output's @tx_out.id@
-- (for the consumed-by UPDATE), and the lovelace value (for the
-- deposit calculation).
--
-- 'Nothing' on cache miss; the caller writes the row with
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
    bumpHit  = atomicModifyIORef' (usStats cache) $ \s ->
      (s { ssHits   = ssHits   s + 1 }, ())
    bumpMiss = atomicModifyIORef' (usStats cache) $ \s ->
      (s { ssMisses = ssMisses s + 1 }, ())

-- | Remove a consumed output from the cache.
--
-- Called by the UTxO extractor after a regular input resolves (the
-- chain consumes that output exactly once) and after a phase-2
-- failed tx's collateral input resolves (the chain consumes the
-- collateral on failure). Deleting a non-existent key is a no-op
-- in LSM, so a stale call is harmless.
deleteConsumed :: UtxoStore -> ByteString -> Word16 -> IO ()
deleteConsumed cache hash idx = do
  let !key = mkKey hash idx
  table <- readIORef (usTable cache)
  LSMTree.delete table key
  atomicModifyIORef' (usStats cache) $ \s ->
    (s { ssDeletes = ssDeletes s + 1 }, ())

-- ---------------------------------------------------------------------------
-- Epoch boundary
-- ---------------------------------------------------------------------------

-- | Snapshot the current table, then close it and reopen from the
-- new snapshot, swapping the active handle in 'usTable'.
--
-- Effect: the active LSM directory drops every run that isn't part
-- of the snapshot (in-flight merges discarded, accumulated level-0
-- runs collapsed into the snapshot's compacted form). The
-- restart-resume invariant is preserved — the snapshot is durable
-- before the swap.
--
-- Called by 'DbSync.Phase.Ingest.Consumer' after each per-epoch
-- @lsCommit@. Synchronous on the consumer thread.
compactUtxoStore :: UtxoStore -> LsmSession -> IO ()
compactUtxoStore store lsm = do
  let session = lsmHandle lsm
  oldTable <- readIORef (usTable store)
  hasSnap <- LSMTree.doesSnapshotExist session currentSnapshotName
  when hasSnap $ LSMTree.deleteSnapshot session currentSnapshotName
  LSMTree.saveSnapshot currentSnapshotName ingestSnapshotLabel oldTable
  -- Open the new table and publish it before closing the old one,
  -- so an async exception between open and swap can't strand the
  -- new handle. The old table's runs survive in the snapshot dir
  -- as hardlinks once 'closeTable' unlinks the active-dir entries.
  mask_ $ do
    newTable <-
      LSMTree.openTableFromSnapshot session currentSnapshotName ingestSnapshotLabel
    writeIORef (usTable store) newTable
  LSMTree.closeTable oldTable

-- ---------------------------------------------------------------------------
-- Stats
-- ---------------------------------------------------------------------------

-- | Snapshot the live counters. Safe to call at any time.
readStoreStats :: UtxoStore -> IO StoreStats
readStoreStats = readIORef . usStats

emptyStats :: StoreStats
emptyStats = StoreStats 0 0 0 0

-- ---------------------------------------------------------------------------
-- Internal: wire format
-- ---------------------------------------------------------------------------

-- | Build the LSM key for one output.
--
-- Layout: @hash (32 bytes) ++ idx (Word16 BE, 2 bytes)@. Hash keys
-- are uniformly distributed in the high 64 bits and the index
-- suffix preserves that property, so 'CompactIndex' stays optimal.
mkKey :: ByteString -> Word16 -> ShortByteString
mkKey hash idx =
  SBS.toShort
    . LBS.toStrict
    . BB.toLazyByteString
    $ BB.byteString hash <> BB.word16BE idx

-- | Encode a single output's resolved row data.
--
-- Layout (24 bytes, fixed):
--
-- @
-- 8 bytes  : txId    (Int64  big-endian)
-- 8 bytes  : outId   (Int64  big-endian)
-- 8 bytes  : value   (Word64 big-endian)
-- @
encodeOutput :: TxId -> TxOutId -> DbLovelace -> UtxoOutputBytes
encodeOutput tid oid val =
  UtxoOutputBytes
    . SBS.toShort
    . LBS.toStrict
    . BB.toLazyByteString
    $ BB.int64BE (getTxId tid)
   <> BB.int64BE (getTxOutId oid)
   <> BB.word64BE (unDbLovelace val)

-- | Inverse of 'encodeOutput'. 'Nothing' on a length mismatch —
-- should never happen for a value the cache itself produced, so the
-- call site treats 'Nothing' as a cache miss.
decodeOutput :: UtxoOutputBytes -> Maybe (TxId, TxOutId, DbLovelace)
decodeOutput (UtxoOutputBytes sbs)
  | BS.length bs /= 24 = Nothing
  | otherwise          = Just
      ( TxId      (readInt64BE  bs 0)
      , TxOutId   (readInt64BE  bs 8)
      , DbLovelace (readWord64BE bs 16)
      )
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
