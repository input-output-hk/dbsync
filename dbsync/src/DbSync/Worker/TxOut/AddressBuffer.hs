-- | Per-epoch buffer of address-resolution work for the
-- 'DbSync.Worker.TxOut.Worker.TxOutWorker'.
--
-- During @IngestChainHistory@ the UTxO extractor does not resolve
-- 'AddressId's synchronously. It records the raw address bytes here
-- instead. The consumer 'takeAndReset's the buffer at each epoch
-- boundary and hands the snapshot to the worker.
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
-- The tx-out lists are 'Seq' for O(1) snoc that keeps FIFO order
-- across the worker handoff.
data EpochAddressBuffer = EpochAddressBuffer
  { eabAddresses :: !(Map ShortByteString (Maybe StakeAddressId))
    -- ^ Unique addresses this epoch, keyed by raw bytes, valued by the
    -- resolved @stake_address_id@. Display text, has-script flag and
    -- payment credential are pure functions of the key, so the worker
    -- rebuilds them with 'DbSync.Db.Schema.Address.addressFromRaw'.
  , eabTxOutAddresses :: !(Seq (TxOutId, ShortByteString))
    -- ^ @(tx_out.id, raw_address)@ pairs in extraction order.
  , eabCollateralTxOutAddresses :: !(Seq (CollateralTxOutId, ShortByteString))
    -- ^ Same shape as 'eabTxOutAddresses' for @collateral_tx_out@.
  }
  deriving stock (Eq, Show)

-- | Mutable handle for the active per-epoch buffer.
--
-- An 'IORef' is sufficient: the main extraction thread writes, the
-- consumer reads at epoch boundaries, and the two never overlap
-- because the consumer runs in the loop that drives extraction.
type AddressBufferRef = IORef EpochAddressBuffer

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

newAddressBufferRef :: IO AddressBufferRef
newAddressBufferRef = newIORef emptyEpochAddressBuffer

emptyEpochAddressBuffer :: EpochAddressBuffer
emptyEpochAddressBuffer = EpochAddressBuffer
  { eabAddresses = Map.empty
  , eabTxOutAddresses = Seq.empty
  , eabCollateralTxOutAddresses = Seq.empty
  }

-- ---------------------------------------------------------------------------
-- * Mutation
-- ---------------------------------------------------------------------------

-- | 'Map.insertWith' keeps the first @stake_address_id@ seen for a raw
-- key. The resolved id is deterministic on the raw bytes within one
-- ingest run, so the choice is immaterial.
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
