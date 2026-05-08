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

import DbSync.Block.Types (GenericTx (..), GenericTxOut (..))
import DbSync.Db.Schema.MultiAsset
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Extractor.SharedDedup (resolveAndWriteMultiAsset)
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


