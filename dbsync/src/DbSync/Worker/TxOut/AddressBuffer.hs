-- | Per-epoch buffer of address-resolution work waiting for the
-- 'DbSync.Worker.TxOut.Worker.TxOutWorker'.
--
-- During @IngestChainHistory@ the UTxO extractor does not look up
-- 'AddressId's synchronously. Instead it appends two facts to this
-- buffer per output:
--
--   * the raw address bytes and resolved @stake_address_id@, keyed so
--     duplicates within the epoch fold to one entry;
--   * the @(tx_out_id, raw_address)@ pair the worker needs to fill
--     @tx_out.address_id@.
--
-- At each epoch boundary the consumer 'takeAndReset's the buffer and
-- hands the snapshot to the worker thread; the buffer is then empty
-- for the next epoch's writes.
--
-- The buffer is owned by the main extraction thread; no STM
-- coordination is needed. The handoff to the worker is a TBQueue
-- defined elsewhere; this module just produces the snapshot value.
module DbSync.Worker.TxOut.AddressBuffer
  ( -- * Types
    EpochAddressBuffer (..)
  , AddressBufferRef

    -- * Construction
  , newAddressBufferRef
  , emptyEpochAddressBuffer

    -- * Mutation
  , recordTxOut
  , recordCollateralTxOut
  , takeAndReset

    -- * Inspection
  , addressBufferCounts
  , forceEpochAddressBuffer
  ) where

import Cardano.Prelude
import Prelude (seq)

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq

import DbSync.Db.Schema.Ids (CollateralTxOutId, StakeAddressId, TxOutId)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Snapshot of one epoch's worth of address-resolution work.
--
-- The tx-out lists are 'Seq' for O(1) snoc with FIFO order preserved
-- across the worker handoff, since an epoch can accumulate a large
-- number of outputs.
data EpochAddressBuffer = EpochAddressBuffer
  { eabAddresses :: !(Map ShortByteString (Maybe StakeAddressId))
    -- ^ Unique addresses seen this epoch, keyed by raw bytes. The
    -- value is the resolved @stake_address_id@ (from the
    -- 'dstStakeAddress' dedup store); the display text, has-script
    -- flag, and payment credential are pure functions of the raw key,
    -- rebuilt by the worker via
    -- 'DbSync.Db.Schema.Address.addressFromRaw' at flush time.
  , eabTxOutAddresses :: !(Seq (TxOutId, ShortByteString))
    -- ^ @(tx_out.id, raw_address)@ pairs in extraction order. The
    -- worker resolves each raw to the final address_id and
    -- @UPDATE@s the row.
  , eabCollateralTxOutAddresses :: !(Seq (CollateralTxOutId, ShortByteString))
    -- ^ Same shape as 'eabTxOutAddresses' for @collateral_tx_out@.
  }
  deriving stock (Eq, Show)

-- | Mutable handle for the active per-epoch buffer.
--
-- An 'IORef' is sufficient: only the main extraction thread writes
-- to it (via 'recordTxOut' \/ 'recordCollateralTxOut'), and only
-- the consumer thread reads it at epoch boundaries (via
-- 'takeAndReset'). The two never overlap because the consumer
-- runs in the same loop that drives extraction.
type AddressBufferRef = IORef EpochAddressBuffer

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

-- | Allocate a fresh empty buffer.
newAddressBufferRef :: IO AddressBufferRef
newAddressBufferRef = newIORef emptyEpochAddressBuffer

-- | The unit value of an empty buffer; convenient for tests and as
-- the 'takeAndReset' reset target.
emptyEpochAddressBuffer :: EpochAddressBuffer
emptyEpochAddressBuffer = EpochAddressBuffer
  { eabAddresses = Map.empty
  , eabTxOutAddresses = Seq.empty
  , eabCollateralTxOutAddresses = Seq.empty
  }

-- ---------------------------------------------------------------------------
-- * Mutation
-- ---------------------------------------------------------------------------

-- | Append a tx_out address-resolution pair to the buffer and
-- (idempotently) record the unique address entry.
--
-- 'Map.insert' keeps the first @stake_address_id@ seen for a given
-- raw key; the resolved id is deterministic on the raw bytes for the
-- duration of one ingest run, so the choice is immaterial.
recordTxOut :: AddressBufferRef -> TxOutId -> ByteString -> Maybe StakeAddressId -> IO ()
recordTxOut ref txOutId raw mStakeId =
  atomicModifyIORef' ref $ \buf ->
    let !key = SBS.toShort raw
        !buf' = buf
          { eabAddresses = Map.insertWith (\_ old -> old) key mStakeId (eabAddresses buf)
          , eabTxOutAddresses = eabTxOutAddresses buf |> (txOutId, key)
          }
    in (buf', ())

-- | Like 'recordTxOut' for @collateral_tx_out@.
recordCollateralTxOut
  :: AddressBufferRef -> CollateralTxOutId -> ByteString -> Maybe StakeAddressId -> IO ()
recordCollateralTxOut ref outId raw mStakeId =
  atomicModifyIORef' ref $ \buf ->
    let !key = SBS.toShort raw
        !buf' = buf
          { eabAddresses = Map.insertWith (\_ old -> old) key mStakeId (eabAddresses buf)
          , eabCollateralTxOutAddresses = eabCollateralTxOutAddresses buf |> (outId, key)
          }
    in (buf', ())

-- | Swap the buffer with an empty one and return the prior contents.
-- Called at each epoch boundary by the consumer.
takeAndReset :: AddressBufferRef -> IO EpochAddressBuffer
takeAndReset ref =
  atomicModifyIORef' ref $ \buf -> (emptyEpochAddressBuffer, buf)

-- ---------------------------------------------------------------------------
-- * Inspection
-- ---------------------------------------------------------------------------

-- | @(unique addresses, tx_out entries)@ in the snapshot; both O(1).
addressBufferCounts :: EpochAddressBuffer -> (Int, Int)
addressBufferCounts b =
  (Map.size (eabAddresses b), Seq.length (eabTxOutAddresses b))

-- | Force every leaf (id + key) to WHNF, collapsing any thunks the
-- snapshot retains. A single O(n) pass; diagnostic for the memory probe.
forceEpochAddressBuffer :: EpochAddressBuffer -> ()
forceEpochAddressBuffer b =
  Map.foldl' (\() v -> forceMaybeId v) () (eabAddresses b)
    `seq` foldl' forcePair () (eabTxOutAddresses b)
    `seq` foldl' forcePair () (eabCollateralTxOutAddresses b)
  where
    forceMaybeId Nothing  = ()
    forceMaybeId (Just i) = i `seq` ()
    forcePair :: () -> (a, ShortByteString) -> ()
    forcePair () (i, k) = i `seq` SBS.length k `seq` ()
