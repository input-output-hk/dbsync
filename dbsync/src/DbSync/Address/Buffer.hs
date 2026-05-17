-- | Per-epoch buffer of address-resolution work waiting for the
-- 'AddressResolver' worker.
--
-- During @IngestChainHistory@ the UTxO extractor does not look up
-- 'AddressId's synchronously. Instead it appends two facts to this
-- buffer per output:
--
--   * the raw address bytes plus its derived fields ('Address' row),
--     keyed so duplicates within the epoch fold to one entry;
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
module DbSync.Address.Buffer
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
  ) where

import Cardano.Prelude

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map

import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Ids (CollateralTxOutId, TxOutId)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Snapshot of one epoch's worth of address-resolution work.
data EpochAddressBuffer = EpochAddressBuffer
  { eabAddresses :: !(Map ShortByteString Address)
    -- ^ Unique addresses seen this epoch, keyed by raw bytes. The
    -- 'Address' value carries the precomputed Bech32 text, the
    -- has-script flag, the payment credential, and the resolved
    -- @stake_address_id@ (from the existing 'dmsStakeAddress'
    -- dedup map). The worker takes this verbatim to build address
    -- rows.
  , eabTxOutAddresses :: ![(TxOutId, ShortByteString)]
    -- ^ @(tx_out.id, raw_address)@ pairs in extraction order. The
    -- worker resolves each raw to the final address_id and
    -- @UPDATE@s the row.
  , eabCollateralTxOutAddresses :: ![(CollateralTxOutId, ShortByteString)]
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
  , eabTxOutAddresses = []
  , eabCollateralTxOutAddresses = []
  }

-- ---------------------------------------------------------------------------
-- * Mutation
-- ---------------------------------------------------------------------------

-- | Append a tx_out address-resolution pair to the buffer and
-- (idempotently) record the unique address entry.
--
-- 'Map.insert' keeps the first 'Address' seen for a given raw key,
-- which is fine because every encoding of the same raw bytes
-- produces the same 'Address' value (the derived fields are pure
-- functions of the raw bytes plus the resolved @stake_address_id@,
-- and the latter is itself deterministic on raw bytes for the
-- duration of one ingest run).
recordTxOut :: AddressBufferRef -> TxOutId -> ByteString -> Address -> IO ()
recordTxOut ref txOutId raw addr =
  atomicModifyIORef' ref $ \buf ->
    let !key = SBS.toShort raw
        !buf' = buf
          { eabAddresses = Map.insertWith (\_ old -> old) key addr (eabAddresses buf)
          , eabTxOutAddresses = (txOutId, key) : eabTxOutAddresses buf
          }
    in (buf', ())

-- | Like 'recordTxOut' for @collateral_tx_out@.
recordCollateralTxOut
  :: AddressBufferRef -> CollateralTxOutId -> ByteString -> Address -> IO ()
recordCollateralTxOut ref outId raw addr =
  atomicModifyIORef' ref $ \buf ->
    let !key = SBS.toShort raw
        !buf' = buf
          { eabAddresses = Map.insertWith (\_ old -> old) key addr (eabAddresses buf)
          , eabCollateralTxOutAddresses = (outId, key) : eabCollateralTxOutAddresses buf
          }
    in (buf', ())

-- | Swap the buffer with an empty one and return the prior contents.
-- Called at each epoch boundary by the consumer.
takeAndReset :: AddressBufferRef -> IO EpochAddressBuffer
takeAndReset ref =
  atomicModifyIORef' ref $ \buf -> (emptyEpochAddressBuffer, buf)
