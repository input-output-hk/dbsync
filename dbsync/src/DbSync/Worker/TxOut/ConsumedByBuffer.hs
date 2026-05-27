-- | Per-epoch buffer of @(producer_tx_out_id, consumer_tx_id)@ pairs
-- produced by the UTxO extractor whenever a cache-hit input resolves
-- to its producer.
--
-- The consumer hands the snapshot to the
-- 'DbSync.Worker.TxOut.TxOutWorker' at each epoch boundary; the
-- worker fans the pairs into one bulk UPDATE against
-- @tx_out.consumed_by_tx_id@ on its dedicated backend, matching rows
-- by @tx_out.id@ (PK lookup, no index on @(tx_id, index)@ needed
-- during Ingest).
--
-- A miss in the 'UtxoStore' does not enqueue a pair here — those
-- inputs fall through to the post-load resolve, which writes the
-- same column from the now-populated @tx_in.tx_out_id@.
module DbSync.Worker.TxOut.ConsumedByBuffer
  ( EpochConsumedByBuffer (..)
  , ConsumedByBufferRef

  , newConsumedByBufferRef
  , emptyEpochConsumedByBuffer

  , recordConsumedBy
  , takeAndReset
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq

import DbSync.Db.Schema.Ids (TxId, TxOutId)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | One epoch's worth of @tx_out.consumed_by_tx_id@ writes.
--
-- Two parallel 'Seq's: producer @tx_out.id@s and the consumer @tx@s
-- that spent them, in lockstep. The worker walks both with
-- 'Foldable.toList' and feeds them to a Hasql @unnest($1, $2)@ bulk
-- UPDATE. 'Seq' gives O(1) snoc; the cardinality is one entry per
-- cache-hit input, so Conway-era epochs reach ~100k entries here.
data EpochConsumedByBuffer = EpochConsumedByBuffer
  { ecbProducerTxOutIds :: !(Seq TxOutId)
  , ecbConsumerTxIds    :: !(Seq TxId)
  }
  deriving stock (Eq, Show)

type ConsumedByBufferRef = IORef EpochConsumedByBuffer

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

newConsumedByBufferRef :: IO ConsumedByBufferRef
newConsumedByBufferRef = newIORef emptyEpochConsumedByBuffer

emptyEpochConsumedByBuffer :: EpochConsumedByBuffer
emptyEpochConsumedByBuffer = EpochConsumedByBuffer Seq.empty Seq.empty

-- ---------------------------------------------------------------------------
-- * Mutation
-- ---------------------------------------------------------------------------

-- | Append one pair. Order across calls is preserved.
recordConsumedBy
  :: ConsumedByBufferRef
  -> TxOutId   -- ^ the producer output's tx_out.id
  -> TxId      -- ^ the consumer tx (the tx whose input is spending it)
  -> IO ()
recordConsumedBy ref producerOutId consumerTxId =
  atomicModifyIORef' ref $ \buf ->
    let !buf' = buf
          { ecbProducerTxOutIds = ecbProducerTxOutIds buf |> producerOutId
          , ecbConsumerTxIds    = ecbConsumerTxIds buf    |> consumerTxId
          }
    in (buf', ())

-- | Swap the buffer with an empty one and return the prior contents.
takeAndReset :: ConsumedByBufferRef -> IO EpochConsumedByBuffer
takeAndReset ref =
  atomicModifyIORef' ref $ \buf -> (emptyEpochConsumedByBuffer, buf)
