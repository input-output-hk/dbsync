-- | Ingest 'IdResolver' fragments for the @utxo@ extractor.
--
-- Handles address-buffer enqueue and UTxO-store cache interactions.
-- Per-row ID assignment for @tx_in@ / collateral / reference is
-- handled by PostgreSQL IDENTITY columns at COPY time.
module DbSync.Phase.Ingest.Resolver.UTxO
  ( -- * ID assignment
    assignCollateralTxOutIdIngest

    -- * Address buffering
  , recordTxOutAddressIngest
  , recordCollateralTxOutAddressIngest
  , resolveAddressIdIngest

    -- * UTxO cache interactions
  , resolveInputValuesIngest
  , resolveInputUtxoIngest
  , recordTxOutputsIngest
  , recordConsumedIngest
  , deleteCachedUtxoIngest
  ) where

import Cardano.Prelude

import Data.IORef (IORef)

import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Ids (AddressId, CollateralTxOutId (..), StakeAddressId, TxId, TxOutId)
import DbSync.Db.Types (DbLovelace)
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..))
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)
import DbSync.Phase.Ingest.UtxoStore (UtxoStore, UtxoTxEntry)
import qualified DbSync.Phase.Ingest.UtxoStore as UtxoStore
import DbSync.Worker.TxOut.AddressBuffer
  ( AddressBufferRef
  , recordCollateralTxOut
  , recordTxOut
  )
import DbSync.Worker.TxOut.ConsumedByBuffer (ConsumedByBufferRef, recordConsumedBy)

assignCollateralTxOutIdIngest :: IORef ExtractState -> IO CollateralTxOutId
assignCollateralTxOutIdIngest extractStateRef =
  allocateNextId extractStateRef icCollateralTxOutId
    (\cs c -> cs { icCollateralTxOutId = c }) CollateralTxOutId

-- ---------------------------------------------------------------------------
-- * Address buffering
-- ---------------------------------------------------------------------------

-- | Queue @(tx_out_id, raw, resolved stake id)@ for the
-- 'DbSync.Worker.TxOut.Worker' to bulk-fill @tx_out.address_id@ at
-- end of epoch.
recordTxOutAddressIngest
  :: AddressBufferRef -> TxOutId -> ByteString -> Maybe StakeAddressId -> IO ()
recordTxOutAddressIngest = recordTxOut

recordCollateralTxOutAddressIngest
  :: AddressBufferRef -> CollateralTxOutId -> ByteString -> Maybe StakeAddressId -> IO ()
recordCollateralTxOutAddressIngest = recordCollateralTxOut

-- | Follow-only entry point. Ingest extractors must record via the
-- async worker so @tx_out.address_id@ is filled in one bulk UPDATE
-- an epoch later rather than per-row.
resolveAddressIdIngest :: ByteString -> Address -> IO AddressId
resolveAddressIdIngest _ _ =
  panic "Phase.Ingest.Resolver: resolveAddressId is Follow-only; use recordTxOutAddress"

-- ---------------------------------------------------------------------------
-- * UTxO cache interactions
-- ---------------------------------------------------------------------------

-- | Bulk-resolve input values from the in-process UTxO cache; a miss
-- returns 'Nothing' and the row is written with @tx_out_id = NULL@.
resolveInputValuesIngest
  :: UtxoStore -> [(ByteString, Word16)] -> IO [Maybe DbLovelace]
resolveInputValuesIngest utxoStore pairs =
  forM pairs $ \(hash, idx) -> do
    m <- UtxoStore.lookupInput utxoStore hash idx
    pure (fmap (\(_, _, v) -> v) m)

resolveInputUtxoIngest
  :: UtxoStore -> ByteString -> Word16 -> IO (Maybe (TxId, TxOutId, DbLovelace))
resolveInputUtxoIngest = UtxoStore.lookupInput

recordTxOutputsIngest :: UtxoStore -> ByteString -> UtxoTxEntry -> IO ()
recordTxOutputsIngest = UtxoStore.recordTx

-- | When the @consumed_by_tx_id@ feature is on ('Just' buf), enqueue
-- the @(producer tx_out.id, consumer tx.id)@ pair for the post-epoch
-- backfill; otherwise drop silently.
recordConsumedIngest
  :: Maybe ConsumedByBufferRef
  -> TxOutId -> TxId -> IO ()
recordConsumedIngest mConsumedByBuf = case mConsumedByBuf of
  Just ref -> recordConsumedBy ref
  Nothing  -> \_ _ -> pure ()

deleteCachedUtxoIngest :: UtxoStore -> ByteString -> Word16 -> IO ()
deleteCachedUtxoIngest = UtxoStore.deleteConsumed
