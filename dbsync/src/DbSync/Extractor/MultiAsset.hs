{-# LANGUAGE OverloadedStrings #-}

-- | Writes multi-asset data into @multi_asset@, @ma_tx_mint@ and
-- @ma_tx_out@. A DedupStore backs @multi_asset@: each unique
-- @(policy, name)@ pair gets one row and a stable 'MultiAssetId' that
-- later mint and output references reuse.
module DbSync.Extractor.MultiAsset
  ( multiAssetExtractor
  ) where

import Cardano.Prelude

import DbSync.Parser.Types (GenericTx (..), GenericTxOut (..))
import DbSync.Db.Schema.MultiAsset
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Extractor.SharedDedup (resolveAndWriteMultiAsset)
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

multiAssetExtractor :: ExtractorDef
multiAssetExtractor = ExtractorDef
  { pdName    = "multi_asset"
  , pdTables  = [multiAssetTableDef, maTxMintTableDef, maTxOutTableDef]
  , pdProcess = processMultiAsset
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processMultiAsset :: ProcessBlockFn
processMultiAsset ctx = do
  writer <- asks getWriter
  forM_ (bcTxs ctx) $ \tc -> do
    let txId   = tcTxId tc
        gtx    = tcGenTx tc
        outIds = tcOutIds tc

    -- A phase-2 failure does not apply its mint, so only a valid tx
    -- contributes ma_tx_mint rows.
    when (txValidContract gtx) $
      forM_ (txMint gtx) $ \(policy, name, quantity) -> do
        maId <- resolveAndWriteMultiAsset policy name
        liftIO $ writeMaTxMint writer MaTxMint
          { maTxMintQuantity = quantity
          , maTxMintTxId     = txId
          , maTxMintIdent    = maId
          }

    -- Outputs always contribute ma_tx_out rows. For a phase-2 failure
    -- 'txOutputs' is the collateral-return output (Babbage onward) that
    -- the parser substitutes in, so its assets still land in ma_tx_out.
    forM_ (zip outIds (txOutputs gtx)) $ \(outId, gout) ->
      forM_ (txOutMultiAssets gout) $ \(policy, name, quantity) -> do
        maId <- resolveAndWriteMultiAsset policy name
        liftIO $ writeMaTxOut writer MaTxOut
          { maTxOutQuantity = DbWord64 (fromIntegral quantity)
          , maTxOutTxOutId  = outId
          , maTxOutIdent    = maId
          }


