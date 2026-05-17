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
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Writer (HasWriter (..), Writer (..))

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
processMultiAsset ctx = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  forM_ (bcTxs ctx) $ \tc -> do
    let txId   = tcTxId tc
        gtx    = tcGenTx tc
        outIds = tcOutIds tc

    -- Phase-2 failures don't mint and produce no on-chain outputs, so
    -- no ma_tx_mint or ma_tx_out rows belong to a failed tx.
    when (txValidContract gtx) $ do
      forM_ (txMint gtx) $ \(policy, name, quantity) -> do
        maId <- resolveAndWriteMultiAsset policy name
        mintId <- liftIO $ assignMaTxMintId resolver
        let mint = MaTxMint
              { maTxMintQuantity = quantity
              , maTxMintTxId     = txId
              , maTxMintIdent    = maId
              }
        liftIO $ writeMaTxMint writer mintId mint

      forM_ (zip outIds (txOutputs gtx)) $ \(outId, gout) -> do
        forM_ (txOutMultiAssets gout) $ \(policy, name, quantity) -> do
          maId <- resolveAndWriteMultiAsset policy name
          maoId <- liftIO $ assignMaTxOutId resolver
          let mao = MaTxOut
                { maTxOutQuantity = DbWord64 (fromIntegral quantity)
                , maTxOutTxOutId  = outId
                , maTxOutIdent    = maId
                }
          liftIO $ writeMaTxOut writer maoId mao


