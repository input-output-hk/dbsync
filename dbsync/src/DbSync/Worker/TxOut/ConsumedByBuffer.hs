-- | Per-epoch buffer of @(producer_tx_out_id, consumer_tx_id)@ pairs
-- from the UTxO extractor.
--
-- The consumer hands the snapshot to the
-- 'DbSync.Worker.TxOut.Worker.TxOutWorker' at each epoch boundary. The
-- worker fans the pairs into one bulk UPDATE of
-- @tx_out.consumed_by_tx_id@, matching rows by @tx_out.id@.
--
-- A 'UtxoStore' miss records nothing here. Those inputs fall through
-- to the post-load resolve, which writes the same column from the
-- populated @tx_in.tx_out_id@.
module DbSync.Worker.TxOut.ConsumedByBuffer
  ( EpochConsumedByBuffer (..)
  , ConsumedByBufferRef

  , newConsumedByBufferRef
  , emptyEpochConsumedByBuffer

  , recordConsumedBy
  , takeAndReset
  , forceEpochConsumedByBuffer
  ) where

import Cardano.Prelude
import Prelude (seq) -- not re-exported by Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq

import DbSync.Db.Schema.Ids (TxId, TxOutId)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | One epoch's worth of @tx_out.consumed_by_tx_id@ writes.
--
-- The two 'Seq's stay in lockstep: the worker zips them into a Hasql
-- @unnest($1, $2)@ bulk UPDATE.
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

-- | Append one pair. Order across calls is preserved. Both ids are
-- forced on entry: an entry lives in the buffer until the epoch's
-- boundary handoff, so a thunk here would retain its closure (and
-- whatever bytes it reads from) for the whole epoch.
recordConsumedBy
  :: ConsumedByBufferRef
  -> TxOutId   -- ^ the producer output's tx_out.id
  -> TxId      -- ^ the consumer tx (the tx whose input is spending it)
  -> IO ()
recordConsumedBy ref !producerOutId !consumerTxId =
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

-- | Force every id to WHNF; diagnostic for the Ingest memory probe.
forceEpochConsumedByBuffer :: EpochConsumedByBuffer -> ()
forceEpochConsumedByBuffer b =
  foldl' (\() i -> i `seq` ()) () (ecbProducerTxOutIds b)
    `seq` foldl' (\() i -> i `seq` ()) () (ecbConsumerTxIds b)
