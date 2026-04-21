{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Transaction extraction from Shelley+ blocks.
--
-- Each @from*Tx@ function converts an era-specific ledger transaction
-- into our era-independent 'GenericTx'. Helpers are shared across eras
-- where possible; later eras add capabilities progressively.
--
-- Import patterns follow the original cardano-db-sync closely to
-- ensure the right lenses and type families are in scope per era.
--
-- __First pass:__ Redeemers, scripts, and governance fields are empty.
module DbSync.Block.Parser.Tx
  ( -- * Era-specific converters
    fromShelleyTx
  , fromAllegraTx
  , fromMaryTx
  , fromAlonzoTx
  , fromBabbageTx
  , fromConwayTx
  , fromDijkstraTx
  ) where

import Cardano.Prelude

import Cardano.Binary (serialize')
import qualified Cardano.Crypto.Hash as Crypto

-- Ledger re-export module that bundles most lenses for Babbage+.
-- Also re-exports Core, Mary, Alonzo, Allegra lenses.
import Cardano.Ledger.Babbage.Core as Core hiding (Tx, TxOut)
import qualified Cardano.Ledger.Core as Core

-- Era-specific modules for things not in the re-export bundle
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Allegra.Core (invalidBefore, invalidHereafter, vldtTxBodyL)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.BaseTypes (TxIx (..), strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxBody (ctbTreasuryDonation)
import Cardano.Ledger.Dijkstra.TxBody (dtbTreasuryDonation)
import Cardano.Ledger.Hashes (extractHash)
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..), AssetName (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Slotting.Slot (SlotNo (..))

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text
import Lens.Micro ((^.))

import Ouroboros.Consensus.Cardano.Block
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , DijkstraEra
  , MaryEra
  , ShelleyEra
  )

import DbSync.Block.Types
  ( GenericTx (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , GenericTxCertificate (..)
  , GenericTxWithdrawal (..)
  )

-- ---------------------------------------------------------------------------
-- * Shared helpers
-- ---------------------------------------------------------------------------

-- | Transaction hash as raw bytes.
txHashId :: Core.EraTx era => Core.Tx era -> ByteString
txHashId tx = Crypto.hashToBytes $ extractHash $ Core.hashAnnotated (tx ^. Core.bodyTxL)

-- | Transaction size in bytes.
getTxSize :: Core.EraTx era => Core.Tx era -> Word64
getTxSize tx = fromIntegral $ tx ^. Core.sizeTxF

-- | Extract inputs from a transaction body.
mkTxIn :: Core.EraTxBody era => Core.TxBody era -> [GenericTxIn]
mkTxIn txBody = map fromTxIn $ toList $ txBody ^. Core.inputsTxBodyL

-- | Convert a single ledger TxIn.
fromTxIn :: Ledger.TxIn -> GenericTxIn
fromTxIn (Ledger.TxIn (Ledger.TxId txid) (TxIx w64)) =
  GenericTxIn
    { txInHash  = Crypto.hashToBytes (extractHash txid)
    , txInIndex = fromIntegral w64
    }

-- | Extract outputs from a Shelley/Allegra body (Coin-only, no multi-assets).
mkTxOutCoin ::
  forall era.
  (Core.EraTxBody era, Core.Value era ~ Coin) =>
  Core.TxBody era ->
  [GenericTxOut]
mkTxOutCoin txBody = zipWith fromCoinTxOut [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)
  where
    fromCoinTxOut :: Word16 -> Core.TxOut era -> GenericTxOut
    fromCoinTxOut idx txOut =
      GenericTxOut
        { txOutIndex       = idx
        , txOutAddress     = addrToText (txOut ^. Core.addrTxOutL)
        , txOutAddressRaw  = Ledger.serialiseAddr (txOut ^. Core.addrTxOutL)
        , txOutValue       = fromIntegral (unCoin (txOut ^. Core.valueTxOutL))
        , txOutDataHash    = Nothing
        , txOutInlineDatum = Nothing
        , txOutRefScript   = Nothing
        , txOutMultiAssets  = []
        }

-- | Extract outputs from a Mary+ body (with multi-asset values).
mkTxOutMaryValue ::
  forall era.
  (Core.EraTxBody era, Core.Value era ~ MaryValue) =>
  Core.TxBody era ->
  [GenericTxOut]
mkTxOutMaryValue txBody = zipWith fromMaryTxOut [0 ..] $ toList (txBody ^. Core.outputsTxBodyL)
  where
    fromMaryTxOut :: Word16 -> Core.TxOut era -> GenericTxOut
    fromMaryTxOut idx txOut =
      let MaryValue ada multiAsset = txOut ^. Core.valueTxOutL
      in GenericTxOut
        { txOutIndex       = idx
        , txOutAddress     = addrToText (txOut ^. Core.addrTxOutL)
        , txOutAddressRaw  = Ledger.serialiseAddr (txOut ^. Core.addrTxOutL)
        , txOutValue       = fromIntegral (unCoin ada)
        , txOutDataHash    = Nothing
        , txOutInlineDatum = Nothing
        , txOutRefScript   = Nothing
        , txOutMultiAssets  = flattenMultiAsset multiAsset
        }

-- | Sum of all withdrawal amounts.
calcWithdrawalSum :: Core.EraTxBody era => Core.TxBody era -> Word64
calcWithdrawalSum bd =
  fromIntegral $ sum $ map unCoin $ Map.elems $
    Shelley.unWithdrawals (bd ^. Core.withdrawalsTxBodyL)

-- | Extract withdrawals. Uses 'EraTxBody' constraint which works across all eras.
mkTxWithdrawals :: Core.EraTxBody era => Core.TxBody era -> [GenericTxWithdrawal]
mkTxWithdrawals bd =
  map fromWithdrawal $ Map.toList $ Shelley.unWithdrawals $ bd ^. Core.withdrawalsTxBodyL
  where
    fromWithdrawal (ra, Coin c) =
      GenericTxWithdrawal
        { txwRewardAddress = Ledger.serialiseRewardAccount ra
        , txwAmount        = fromIntegral c
        }

-- | Extract certificates as raw CBOR bytes.
-- Full certificate parsing is deferred to the StakeDelegation extractor.
mkTxCertificatesRaw :: Core.EraTxBody era => Core.TxBody era -> [GenericTxCertificate]
mkTxCertificatesRaw bd =
  zipWith toCertRaw [0 ..] $ toList (bd ^. Core.certsTxBodyL)
  where
    toCertRaw idx cert =
      GenericTxCertificate
        { txCertIndex = idx
        , txCertBytes = serialize' cert
        }

-- | Extract raw metadata CBOR, if present.
getTxMetadataRaw :: Core.EraTx era => Core.Tx era -> Maybe ByteString
getTxMetadataRaw tx =
  case strictMaybeToMaybe (tx ^. Core.auxDataTxL) of
    Nothing  -> Nothing
    Just aux -> Just (serialize' aux)

-- | Validity interval extraction (Allegra+ eras).
getInterval :: AllegraEraTxBody era => Core.TxBody era -> (Maybe Word64, Maybe Word64)
getInterval txBody =
  ( fmap unSlotNo $ strictMaybeToMaybe $ invalidBefore interval
  , fmap unSlotNo $ strictMaybeToMaybe $ invalidHereafter interval
  )
  where
    interval = txBody ^. vldtTxBodyL

-- | Sum of output values.
sumOutputValues :: [GenericTxOut] -> Word64
sumOutputValues = sum . map txOutValue

-- | Extract minting from a Mary+ body as flat list.
getMint :: MaryEraTxBody era => Core.TxBody era -> [(ByteString, ByteString, Integer)]
getMint txBody = flattenMultiAsset (txBody ^. mintTxBodyL)

-- | Flatten a MultiAsset into @[(policy_id, asset_name, quantity)]@.
flattenMultiAsset :: MultiAsset -> [(ByteString, ByteString, Integer)]
flattenMultiAsset (MultiAsset m) =
  [ (policyIdBytes pid, assetNameBytes' an, qty)
  | (pid, assets) <- Map.toList m
  , (an, qty) <- Map.toList assets
  ]
  where
    policyIdBytes (PolicyID (Core.ScriptHash h)) = Crypto.hashToBytes h
    assetNameBytes' (AssetName sbs) = SBS.fromShort sbs

-- | Extract collateral inputs (Alonzo+ eras).
mkCollTxIn :: AlonzoEraTxBody era => Core.TxBody era -> [GenericTxIn]
mkCollTxIn txBody = map fromTxIn $ toList $ txBody ^. collateralInputsTxBodyL

-- | Extract reference inputs (Babbage+ eras).
mkRefTxIn :: BabbageEraTxBody era => Core.TxBody era -> [GenericTxIn]
mkRefTxIn txBody = map fromTxIn $ toList $ txBody ^. referenceInputsTxBodyL

-- | Address to text. Uses hex encoding of raw bytes for now.
-- TODO: Switch to proper Bech32 encoding for Shelley+ addresses.
addrToText :: Ledger.Addr -> Text
addrToText addr = Text.decodeUtf8 (Base16.encode (Ledger.serialiseAddr addr))

-- ---------------------------------------------------------------------------
-- * Shelley era
-- ---------------------------------------------------------------------------

fromShelleyTx :: (Word64, Core.Tx ShelleyEra) -> GenericTx
fromShelleyTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      outputs = mkTxOutCoin txBody
      fee = txBody ^. Core.feeTxBodyL
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = True
    , txScriptSize       = 0
    , txTreasuryDonation = 0
    , txInvalidBefore    = Nothing
    , txInvalidHereafter = Just $ unSlotNo (txBody ^. Shelley.ttlTxBodyL)
    , txInputs           = mkTxIn txBody
    , txOutputs          = outputs
    , txCollateralInputs = []
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = mkTxCertificatesRaw txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = getTxMetadataRaw tx
    , txMint             = []
    }

-- ---------------------------------------------------------------------------
-- * Allegra era (adds validity intervals)
-- ---------------------------------------------------------------------------

fromAllegraTx :: (Word64, Core.Tx AllegraEra) -> GenericTx
fromAllegraTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      outputs = mkTxOutCoin txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = True
    , txScriptSize       = 0
    , txTreasuryDonation = 0
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = mkTxIn txBody
    , txOutputs          = outputs
    , txCollateralInputs = []
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = mkTxCertificatesRaw txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = getTxMetadataRaw tx
    , txMint             = []
    }

-- ---------------------------------------------------------------------------
-- * Mary era (adds multi-assets)
-- ---------------------------------------------------------------------------

fromMaryTx :: (Word64, Core.Tx MaryEra) -> GenericTx
fromMaryTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      outputs = mkTxOutMaryValue txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = True
    , txScriptSize       = 0
    , txTreasuryDonation = 0
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = mkTxIn txBody
    , txOutputs          = outputs
    , txCollateralInputs = []
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = mkTxCertificatesRaw txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = getTxMetadataRaw tx
    , txMint             = getMint txBody
    }

-- ---------------------------------------------------------------------------
-- * Alonzo era (adds Plutus, collateral, phase-2 validation)
-- ---------------------------------------------------------------------------

fromAlonzoTx :: (Word64, Core.Tx AlonzoEra) -> GenericTx
fromAlonzoTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      outputs = mkTxOutMaryValue txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = isValid
    , txScriptSize       = 0  -- TODO: sum Plutus script sizes
    , txTreasuryDonation = 0
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = if isValid then mkTxIn txBody else collIns
    , txOutputs          = outputs
    , txCollateralInputs = collIns
    , txReferenceInputs  = []
    , txCollateralOutput = Nothing
    , txCertificates     = mkTxCertificatesRaw txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = getTxMetadataRaw tx
    , txMint             = getMint txBody
    }

-- ---------------------------------------------------------------------------
-- * Babbage era (adds reference inputs, collateral outputs, inline datums)
-- ---------------------------------------------------------------------------

fromBabbageTx :: (Word64, Core.Tx BabbageEra) -> GenericTx
fromBabbageTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      outputs = mkTxOutMaryValue txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      refIns = mkRefTxIn txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = isValid
    , txScriptSize       = 0  -- TODO: sum Plutus script sizes
    , txTreasuryDonation = 0
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = if isValid then mkTxIn txBody else collIns
    , txOutputs          = outputs
    , txCollateralInputs = collIns
    , txReferenceInputs  = refIns
    , txCollateralOutput = Nothing  -- TODO: extract Babbage collateral output
    , txCertificates     = mkTxCertificatesRaw txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = getTxMetadataRaw tx
    , txMint             = getMint txBody
    }

-- ---------------------------------------------------------------------------
-- * Conway era (adds governance, treasury donations)
-- ---------------------------------------------------------------------------

fromConwayTx :: (Word64, Core.Tx ConwayEra) -> GenericTx
fromConwayTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      outputs = mkTxOutMaryValue txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      refIns = mkRefTxIn txBody
      Coin donation = ctbTreasuryDonation txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = isValid
    , txScriptSize       = 0  -- TODO: sum Plutus script sizes
    , txTreasuryDonation = fromIntegral donation
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = if isValid then mkTxIn txBody else collIns
    , txOutputs          = outputs
    , txCollateralInputs = collIns
    , txReferenceInputs  = refIns
    , txCollateralOutput = Nothing  -- TODO: extract collateral output
    , txCertificates     = mkTxCertificatesRaw txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = getTxMetadataRaw tx
    , txMint             = getMint txBody
    }

-- ---------------------------------------------------------------------------
-- * Dijkstra era (same structure as Conway)
-- ---------------------------------------------------------------------------

fromDijkstraTx :: (Word64, Core.Tx DijkstraEra) -> GenericTx
fromDijkstraTx (blkIndex, tx) =
  let txBody = tx ^. Core.bodyTxL
      Alonzo.IsValid isValid = tx ^. Alonzo.isValidTxL
      outputs = mkTxOutMaryValue txBody
      fee = txBody ^. Core.feeTxBodyL
      (invBefore, invAfter) = getInterval txBody
      collIns = mkCollTxIn txBody
      refIns = mkRefTxIn txBody
      Coin donation = dtbTreasuryDonation txBody
  in GenericTx
    { txHash             = txHashId tx
    , txBlockIndex       = blkIndex
    , txSize             = getTxSize tx
    , txFee              = fromIntegral (unCoin fee)
    , txOutSum           = sumOutputValues outputs
    , txValidContract    = isValid
    , txScriptSize       = 0
    , txTreasuryDonation = fromIntegral donation
    , txInvalidBefore    = invBefore
    , txInvalidHereafter = invAfter
    , txInputs           = if isValid then mkTxIn txBody else collIns
    , txOutputs          = outputs
    , txCollateralInputs = collIns
    , txReferenceInputs  = refIns
    , txCollateralOutput = Nothing
    , txCertificates     = mkTxCertificatesRaw txBody
    , txWithdrawals      = mkTxWithdrawals txBody
    , txMetadata         = getTxMetadataRaw tx
    , txMint             = getMint txBody
    }
