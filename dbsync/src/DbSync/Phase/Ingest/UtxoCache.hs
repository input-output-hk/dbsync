-- | Bounded FIFO map from tx hash to its assigned 'TxId' and per-output
-- lovelace values.
--
-- Populated as each tx is processed by the per-block pipeline; consulted
-- by the UTxO extractor when resolving inputs. A hit lets the extractor
-- write @tx_in.tx_out_id@ and accumulate input value for the deposit
-- calculation at COPY time; a miss falls back to the post-load resolve.
--
-- == Memory shape
--
-- Keys are 'ShortByteString' (unpinned, GHC-managed heap) rather than
-- pinned 'ByteString'. Storage is a mutable 'BasicHashTable' so inserts
-- mutate in-place without path-copying.
--
-- Eviction is FIFO over a bounded ring of recently inserted hashes:
-- the oldest entries are dropped first when 'ucCapacity' is exceeded.
-- A 5M-entry cache holds approximately 100k blocks of recent tx
-- history under modern mainnet density (~550MB heap).
module DbSync.Phase.Ingest.UtxoCache
  ( -- * Types
    UtxoCache
  , UtxoTxEntry (..)
  , CacheStats (..)

    -- * Construction
  , newUtxoCache

    -- * Operations
  , recordTx
  , lookupInput
  , readCacheStats

    -- * Pure helpers (exported for tests)
  , defaultCacheCapacity
  ) where

import Cardano.Prelude

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HT
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Sequence as Seq

import DbSync.Db.Schema.Ids (TxId, TxOutId)
import DbSync.Db.Types (DbLovelace)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Per-tx entry held in the cache: the assigned 'TxId' plus, for
-- every output, its assigned 'TxOutId' and lovelace value.
--
-- Carrying the 'TxOutId' lets the consumed-by UPDATE match by primary
-- key (@WHERE tx_out.id = u.out_id@) rather than by @(tx_id, index)@,
-- which would require an index that doesn't exist during Ingest.
data UtxoTxEntry = UtxoTxEntry
  { uteTxId    :: !TxId
  , uteOutputs :: !(Seq (TxOutId, DbLovelace))
    -- ^ Outputs in order. Indexed by 'Word16' output index;
    -- 'Seq.lookup' returns 'Nothing' for out-of-range probes.
  }
  deriving stock (Eq, Show)

-- | Cumulative hit / miss / eviction counters. Sampled at epoch
-- boundaries for the diagnostic log line.
data CacheStats = CacheStats
  { csHits         :: !Word64
  , csMisses       :: !Word64
  , csEntries      :: !Int
  , csEvictions    :: !Word64
  }
  deriving stock (Eq, Show)

-- | Mutable cache handle.
data UtxoCache = UtxoCache
  { ucTable    :: !(BasicHashTable ShortByteString UtxoTxEntry)
  , ucRing     :: !(IORef (Seq ShortByteString))
  , ucCapacity :: !Int
  , ucStats    :: !(IORef CacheStats)
  }

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Default capacity for production runs. Sized for ~83% hit rate on
-- mainnet at ~550MB RAM.
defaultCacheCapacity :: Int
defaultCacheCapacity = 5_000_000

-- | Allocate an empty cache with the supplied capacity.
newUtxoCache :: Int -> IO UtxoCache
newUtxoCache capacity = do
  table <- HT.new
  ring  <- newIORef Seq.empty
  stats <- newIORef (CacheStats 0 0 0 0)
  pure UtxoCache
    { ucTable    = table
    , ucRing     = ring
    , ucCapacity = max 1 capacity
    , ucStats    = stats
    }

-- ---------------------------------------------------------------------------
-- * Operations
-- ---------------------------------------------------------------------------

-- | Insert a tx's @(tx_id, output values)@ into the cache. Evicts the
-- oldest entry first when at capacity.
--
-- Re-inserting the same hash overwrites the previous entry and does
-- not bump the eviction ring — that path is only reached on resume
-- where the same tx is replayed.
recordTx :: UtxoCache -> ByteString -> UtxoTxEntry -> IO ()
recordTx cache hash entry = do
  let !key = SBS.toShort hash
  existing <- HT.lookup (ucTable cache) key
  case existing of
    Just _  -> HT.insert (ucTable cache) key entry
    Nothing -> do
      evicted <- atomicModifyIORef' (ucRing cache) $ \ring ->
        let ring'   = ring Seq.|> key
            atCap   = Seq.length ring' > ucCapacity cache
            (evicted', kept)
              | atCap = case Seq.viewl ring' of
                          Seq.EmptyL    -> (Nothing, ring')
                          oldest Seq.:< rest -> (Just oldest, rest)
              | otherwise = (Nothing, ring')
        in (kept, evicted')
      for_ evicted $ \old -> do
        HT.delete (ucTable cache) old
        atomicModifyIORef' (ucStats cache) $ \s ->
          (s { csEvictions = csEvictions s + 1 }, ())
      HT.insert (ucTable cache) key entry
      atomicModifyIORef' (ucStats cache) $ \s ->
        (s { csEntries = csEntries s + (if isJust evicted then 0 else 1) }, ())

-- | Look up the producer of an input. 'Nothing' on miss; the caller
-- writes the row with @tx_out_id = NULL@ and the post-load resolve
-- handles it.
--
-- Returns the producer's @tx.id@ (for @tx_in.tx_out_id@ — confusingly
-- named: it's the producing tx's row id, not the output's row id),
-- the producer-output's @tx_out.id@ (for the consumed-by UPDATE), and
-- the lovelace value (for the deposit calculation).
lookupInput
  :: UtxoCache
  -> ByteString   -- ^ producer tx hash
  -> Word16       -- ^ output index
  -> IO (Maybe (TxId, TxOutId, DbLovelace))
lookupInput cache hash idx = do
  let !key = SBS.toShort hash
  entry <- HT.lookup (ucTable cache) key
  case entry of
    Just ute
      | Just (outId, val) <- Seq.lookup (fromIntegral idx) (uteOutputs ute) -> do
          bumpHit
          pure (Just (uteTxId ute, outId, val))
    _ -> do
      bumpMiss
      pure Nothing
  where
    bumpHit = atomicModifyIORef' (ucStats cache) $ \s ->
      (s { csHits = csHits s + 1 }, ())
    bumpMiss = atomicModifyIORef' (ucStats cache) $ \s ->
      (s { csMisses = csMisses s + 1 }, ())

-- | Snapshot the live counters. Safe to call at any time; values
-- reflect the cumulative totals since 'newUtxoCache'.
readCacheStats :: UtxoCache -> IO CacheStats
readCacheStats = readIORef . ucStats
