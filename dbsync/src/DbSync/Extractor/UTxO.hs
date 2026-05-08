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

    -- * Internal helpers (exported for tests)
  , extractStakeCred
  , mkAddress
  , mkTxOut
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.List (zip3)

import DbSync.Block.Types (GenericTx (..), GenericTxIn (..))
import qualified DbSync.Block.Types as G
import DbSync.Db.Schema.Address (Address (..), addressTableDef)
import DbSync.Db.Schema.Ids (AddressId, StakeAddressId, TxId (..))
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
  , pdDependencies = [("core", 1), ("stake_delegation", 1)]
  , pdTables       =
      [ addressTableDef
      , txOutTableDef
      , txInTableDef
      , collateralTxInTableDef
      , referenceTxInTableDef
      ]
  , pdProcess      = processUTxO
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processUTxO :: ProcessBlockFn
processUTxO resolver writer ctx = do
  forM_ (bcTxs ctx) $ \tc -> do
    let txId    = tcTxId tc
        gtx     = tcGenTx tc
        outIds  = tcOutIds tc
        stakeIds = tcOutStakeIds tc

    -- The pipeline pre-resolves @stakeIds@ so the address row and the
    -- tx_out row share the same StakeAddressId.
    forM_ (zip3 outIds stakeIds (txOutputs gtx)) $ \(outId, mStakeId, gout) -> do
      let addr = mkAddress mStakeId gout
      (addrId, isNew) <- resolveAddress resolver (G.txOutAddressRaw gout) addr
      when isNew $ writeAddress writer addrId addr
      let txOut = mkTxOut txId addrId mStakeId gout
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

mkAddress :: Maybe StakeAddressId -> G.GenericTxOut -> Address
mkAddress mStakeId gout = Address
  { addressAddress        = G.txOutAddress gout
  , addressRaw            = G.txOutAddressRaw gout
  , addressHasScript      = rawHasScript (G.txOutAddressRaw gout)
  , addressPaymentCred    = extractPaymentCred (G.txOutAddressRaw gout)
  , addressStakeAddressId = mStakeId
  }

mkTxOut :: TxId -> AddressId -> Maybe StakeAddressId -> G.GenericTxOut -> TxOut
mkTxOut txId addrId mStakeId gout = TxOut
  { txOutTxId              = txId
  , txOutIndex             = fromIntegral (G.txOutIndex gout)
  , txOutAddressId         = addrId
  , txOutStakeAddressId    = mStakeId
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
rawHasScript :: ByteString -> Bool
rawHasScript bs
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

-- | Extract the inline 28-byte stake credential from a Shelley address.
--
-- Returns 'Just' for base addresses (header types @0x00@\/@0x10@\/@0x20@\/@0x30@,
-- per CIP-19) where bytes 30-57 carry the stake key or script hash.
-- Pointer, enterprise, reward, and Byron addresses have no inline cred
-- and yield 'Nothing'.
extractStakeCred :: ByteString -> Maybe ByteString
extractStakeCred bs
  | BS.length bs < 57 = Nothing
  | otherwise =
      let typeBits = BS.head bs .&. 0xF0
      in if typeBits == 0x00
           || typeBits == 0x10
           || typeBits == 0x20
           || typeBits == 0x30
           then Just (BS.take 28 (BS.drop 29 bs))
           else Nothing
