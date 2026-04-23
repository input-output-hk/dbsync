{-# LANGUAGE OverloadedStrings #-}

-- | MultiAsset extractor.
--
-- Extracts multi-asset data into @multi_asset@, @ma_tx_mint@, and
-- @ma_tx_out@ tables.
--
-- The @multi_asset@ table uses a DedupMap: each unique (policy, name)
-- pair gets a single row and a stable 'MultiAssetId'. Subsequent
-- references (in mint or output events) reuse the same ID.
module DbSync.Extractor.MultiAsset
  ( multiAssetExtractor
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS

import DbSync.Block.Types (GenericTx (..), GenericTxOut (..))
import DbSync.Db.Schema.Ids (MultiAssetId (..), TxOutId (..))
import DbSync.Db.Schema.MultiAsset
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Resolver (IdResolver (..))
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

multiAssetExtractor :: ExtractorDef
multiAssetExtractor = ExtractorDef
  { pdName         = "multi_asset"
  , pdVersion      = 1
  , pdDependencies = [("core", 1), ("utxo", 1)]
  , pdTables       = [multiAssetTableDef, maTxMintTableDef, maTxOutTableDef]
  , pdProcess      = processMultiAsset
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processMultiAsset :: ProcessBlockFn
processMultiAsset resolver writer ctx = do
  forM_ (bcTxs ctx) $ \tc -> do
    let txId   = tcTxId tc
        gtx    = tcGenTx tc
        outIds = tcOutIds tc

    -- 1. Process minting/burning events
    forM_ (txMint gtx) $ \(policy, name, quantity) -> do
      maId <- resolveAndWriteMultiAsset resolver writer policy name
      mintId <- assignMaTxMintId resolver
      let mint = MaTxMint
            { maTxMintQuantity = quantity
            , maTxMintTxId     = txId
            , maTxMintIdent    = maId
            }
      writeMaTxMint writer mintId mint

    -- 2. Process multi-asset outputs
    forM_ (zip outIds (txOutputs gtx)) $ \(outId, gout) -> do
      forM_ (txOutMultiAssets gout) $ \(policy, name, quantity) -> do
        maId <- resolveAndWriteMultiAsset resolver writer policy name
        maoId <- assignMaTxOutId resolver
        let mao = MaTxOut
              { maTxOutQuantity = DbWord64 (fromIntegral quantity)
              , maTxOutTxOutId  = outId
              , maTxOutIdent    = maId
              }
        writeMaTxOut writer maoId mao

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Resolve a multi-asset by (policy, name). If new, write the
-- @multi_asset@ row to the writer.
resolveAndWriteMultiAsset
  :: IdResolver IO
  -> Writer IO
  -> ByteString    -- ^ policy ID
  -> ByteString    -- ^ asset name
  -> IO MultiAssetId
resolveAndWriteMultiAsset resolver writer policy name = do
  -- Build key as ShortByteString directly to avoid an intermediate
  -- pinned ByteString from (<>). SBS is unpinned and GC-friendly.
  let key = SBS.toShort policy <> SBS.toShort name
      ma = MultiAsset
        { multiAssetPolicy      = policy
        , multiAssetName        = name
        , multiAssetFingerprint = mkFingerprint policy name
        }
  (maId, isNew) <- resolveMultiAsset resolver key ma  -- key is ShortByteString
  when isNew $
    writeMultiAsset writer maId ma
  pure maId

-- | Compute a CIP-14 asset fingerprint placeholder.
-- TODO: Implement proper CIP-14 (bech32 encoding of blake2b hash).
-- For now, use a hex representation.
mkFingerprint :: ByteString -> ByteString -> Text
mkFingerprint policy name =
  "asset" <> toS @[Char] @Text (concatMap hexByte (BS.unpack (BS.take 20 (policy <> name))))
  where
    hexByte :: Word8 -> [Char]
    hexByte w =
      let hi = w `div` 16
          lo = w `mod` 16
      in [hexDigit hi, hexDigit lo]
    hexDigit :: Word8 -> Char
    hexDigit n
      | n < 10    = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')
