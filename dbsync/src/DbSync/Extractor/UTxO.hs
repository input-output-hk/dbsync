{-# LANGUAGE OverloadedStrings #-}

-- | UTxO extractor.
--
-- Extracts transaction outputs and inputs into @tx_out@, @tx_in@,
-- @collateral_tx_in@, and @reference_tx_in@ tables.
--
-- During 'IngestChainHistory', @tx_in.tx_out_id@ is NULL — only
-- the spent tx hash and output index are stored. The FK is resolved
-- post-load via a SQL join in 'PreparingForChainTip'.
module DbSync.Extractor.UTxO
  ( utxoExtractor
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS

import DbSync.Block.Types (GenericTx (..), GenericTxIn (..))
import qualified DbSync.Block.Types as G
import DbSync.Db.Schema.Ids (TxId (..), TxOutId (..))
import DbSync.Db.Schema.UTxO
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Resolver (IdResolver (..))
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

utxoExtractor :: ExtractorDef
utxoExtractor = ExtractorDef
  { pdName         = "utxo"
  , pdVersion      = 1
  , pdDependencies = [("core", 1)]
  , pdTables       = [txOutTableDef, txInTableDef, collateralTxInTableDef, referenceTxInTableDef]
  , pdProcess      = processUTxO
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processUTxO :: ProcessBlockFn
processUTxO resolver writer ctx = do
  forM_ (bcTxs ctx) $ \tc -> do
    let txId   = tcTxId tc
        gtx    = tcGenTx tc
        outIds = tcOutIds tc

    -- 1. Write tx_out rows (using pre-assigned TxOutIds)
    forM_ (zip outIds (txOutputs gtx)) $ \(outId, gout) -> do
      let txOut = mkTxOut txId gout
      writeTxOut writer outId txOut

    -- 2. Write tx_in rows
    forM_ (txInputs gtx) $ \gin -> do
      inId <- assignTxInId resolver
      let txIn = mkTxIn txId gin
      writeTxIn writer inId txIn

    -- 3. Write collateral_tx_in rows
    forM_ (txCollateralInputs gtx) $ \gin -> do
      inId <- assignCollateralTxInId resolver
      let ci = mkCollateralTxIn txId gin
      writeCollateralTxIn writer inId ci

    -- 4. Write reference_tx_in rows
    forM_ (txReferenceInputs gtx) $ \gin -> do
      inId <- assignReferenceTxInId resolver
      let ri = mkReferenceTxIn txId gin
      writeReferenceTxIn writer inId ri

-- ---------------------------------------------------------------------------
-- * Record builders
-- ---------------------------------------------------------------------------

mkTxOut :: TxId -> G.GenericTxOut -> TxOut
mkTxOut txId gout = TxOut
  { txOutTxId              = txId
  , txOutIndex             = fromIntegral (G.txOutIndex gout)
  , txOutAddress           = G.txOutAddress gout
  , txOutAddressHasScript  = addressHasScript (G.txOutAddressRaw gout)
  , txOutPaymentCred       = extractPaymentCred (G.txOutAddressRaw gout)
  , txOutStakeAddressId    = Nothing  -- resolved by StakeDelegation extractor
  , txOutValue             = DbLovelace (G.txOutValue gout)
  , txOutDataHash          = G.txOutDataHash gout
  , txOutInlineDatumId     = Nothing  -- resolved by ScriptsDatums extractor
  , txOutReferenceScriptId = Nothing  -- resolved by ScriptsDatums extractor
  , txOutConsumedByTxId    = Nothing  -- resolved post-load
  }

mkTxIn :: TxId -> GenericTxIn -> TxIn
mkTxIn txId gin = TxIn
  { txInTxInId     = txId
  , txInTxOutId    = Nothing  -- deferred: resolved post-load via SQL join
  , txInTxOutIndex = fromIntegral (txInIndex gin)
  , txInTxOutHash  = txInHash gin
  , txInRedeemerId = Nothing  -- resolved by ScriptsDatums extractor
  }

mkCollateralTxIn :: TxId -> GenericTxIn -> CollateralTxIn
mkCollateralTxIn txId gin = CollateralTxIn
  { collateralTxInTxInId     = txId
  , collateralTxInTxOutId    = Nothing
  , collateralTxInTxOutIndex = fromIntegral (txInIndex gin)
  , collateralTxInTxOutHash  = txInHash gin
  }

mkReferenceTxIn :: TxId -> GenericTxIn -> ReferenceTxIn
mkReferenceTxIn txId gin = ReferenceTxIn
  { referenceTxInTxInId     = txId
  , referenceTxInTxOutId    = Nothing
  , referenceTxInTxOutIndex = fromIntegral (txInIndex gin)
  , referenceTxInTxOutHash  = txInHash gin
  }

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Check if an address contains a script (bit 4 of header byte).
-- Shelley+ addresses encode this in the header. Byron addresses
-- never contain scripts.
addressHasScript :: ByteString -> Bool
addressHasScript bs
  | BS.null bs = False
  | otherwise  =
      let header = BS.head bs
      in (header .&. 0x10) /= 0  -- bit 4 set = script address

-- | Extract the payment credential (first 28 bytes after header)
-- from a Shelley+ address. Returns Nothing for Byron addresses.
extractPaymentCred :: ByteString -> Maybe ByteString
extractPaymentCred bs
  | BS.length bs < 29 = Nothing  -- Too short for Shelley address
  | otherwise =
      let header = BS.head bs
      in if header .&. 0xE0 == 0x00  -- Byron address type
         then Nothing
         else Just $ BS.take 28 (BS.drop 1 bs)
