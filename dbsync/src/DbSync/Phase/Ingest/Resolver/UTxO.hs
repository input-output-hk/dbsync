-- | Ingest 'IdResolver' fragments for the @utxo@ extractor.
--
-- Covers ID assignment for tx_in / collateral / reference and the
-- address-buffer + utxo-store interactions.
module DbSync.Phase.Ingest.Resolver.UTxO
  ( -- * ID assignment
    assignTxInIdIngest
  , assignCollateralTxInIdIngest
  , assignCollateralTxOutIdIngest
  , assignReferenceTxInIdIngest

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
import DbSync.Db.Schema.Ids
  ( AddressId
  , CollateralTxInId (..)
  , CollateralTxOutId (..)
  , ReferenceTxInId (..)
  , TxId
  , TxInId (..)
  , TxOutId
  )
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

-- ---------------------------------------------------------------------------
-- * ID assignment
-- ---------------------------------------------------------------------------

assignTxInIdIngest :: IORef ExtractState -> IO TxInId
assignTxInIdIngest extractStateRef =
  allocateNextId extractStateRef icTxInId (\cs c -> cs { icTxInId = c }) TxInId

assignCollateralTxInIdIngest :: IORef ExtractState -> IO CollateralTxInId
assignCollateralTxInIdIngest extractStateRef =
  allocateNextId extractStateRef icCollateralTxInId (\cs c -> cs { icCollateralTxInId = c }) CollateralTxInId

assignCollateralTxOutIdIngest :: IORef ExtractState -> IO CollateralTxOutId
assignCollateralTxOutIdIngest extractStateRef =
  allocateNextId extractStateRef icCollateralTxOutId (\cs c -> cs { icCollateralTxOutId = c }) CollateralTxOutId

assignReferenceTxInIdIngest :: IORef ExtractState -> IO ReferenceTxInId
assignReferenceTxInIdIngest extractStateRef =
  allocateNextId extractStateRef icReferenceTxInId (\cs c -> cs { icReferenceTxInId = c }) ReferenceTxInId

-- ---------------------------------------------------------------------------
-- * Address buffering
-- ---------------------------------------------------------------------------

-- | Queue @(tx_out_id, raw, derived address)@ for the
-- 'DbSync.Worker.TxOut.Worker' to bulk-fill @tx_out.address_id@ at
-- end of epoch.
recordTxOutAddressIngest
  :: AddressBufferRef -> TxOutId -> ByteString -> Address -> IO ()
recordTxOutAddressIngest = recordTxOut

recordCollateralTxOutAddressIngest
  :: AddressBufferRef -> CollateralTxOutId -> ByteString -> Address -> IO ()
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
