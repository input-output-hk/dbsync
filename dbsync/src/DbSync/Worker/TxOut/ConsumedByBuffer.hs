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
-- A miss in the 'UtxoCache' does not enqueue a pair here — those
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

import DbSync.Db.Schema.Ids (TxId, TxOutId)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | One epoch's worth of @tx_out.consumed_by_tx_id@ writes.
--
-- Stored as two parallel lists so the worker can pass them straight
-- to a Hasql @unnest($1, $2)@ bulk UPDATE without a re-shape pass.
data EpochConsumedByBuffer = EpochConsumedByBuffer
  { ecbProducerTxOutIds :: ![TxOutId]
  , ecbConsumerTxIds    :: ![TxId]
  }
  deriving stock (Eq, Show)

type ConsumedByBufferRef = IORef EpochConsumedByBuffer

-- ---------------------------------------------------------------------------
-- * Construction
-- ---------------------------------------------------------------------------

newConsumedByBufferRef :: IO ConsumedByBufferRef
newConsumedByBufferRef = newIORef emptyEpochConsumedByBuffer

emptyEpochConsumedByBuffer :: EpochConsumedByBuffer
emptyEpochConsumedByBuffer = EpochConsumedByBuffer [] []

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
          { ecbProducerTxOutIds = producerOutId : ecbProducerTxOutIds buf
          , ecbConsumerTxIds    = consumerTxId  : ecbConsumerTxIds buf
          }
    in (buf', ())

-- | Swap the buffer with an empty one and return the prior contents.
takeAndReset :: ConsumedByBufferRef -> IO EpochConsumedByBuffer
takeAndReset ref =
  atomicModifyIORef' ref $ \buf -> (emptyEpochConsumedByBuffer, buf)
