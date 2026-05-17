{-# LANGUAGE OverloadedStrings #-}

-- | Metadata extractor.
--
-- Emits one @tx_metadata@ row per metadata key in a transaction.
-- Each row stores the single-key CBOR encoding of that pair (matching
-- what the original @cardano-db-sync@ writes) plus the no-schema JSON
-- rendering of the value.
module DbSync.Extractor.Metadata
  ( metadataExtractor
  ) where

import Cardano.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text

import DbSync.Block.Metadata (metadataValueToJson, serialiseSingleton)
import DbSync.Block.Types (GenericTx (..))
import DbSync.Db.Schema.Metadata
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

metadataExtractor :: ExtractorDef
metadataExtractor = ExtractorDef
  { pdName         = "metadata"
  , pdVersion      = 1
  , pdDependencies = [("core", 1)]
  , pdTables       = [txMetadataTableDef]
  , pdProcess      = processMetadata
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

-- | Walk every metadata key in every tx and emit one row per pair.
-- Empty maps (parser saw aux-data but it carried no metadata) yield
-- no rows. Failed phase-2 txs are skipped — their metadata is not
-- recorded on-chain.
processMetadata :: ProcessBlockFn
processMetadata ctx = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  forM_ (bcTxs ctx) $ \tc ->
    when (txValidContract (tcGenTx tc)) $
      case txMetadata (tcGenTx tc) of
        Nothing    -> pure ()
        Just mdMap -> forM_ (Map.toAscList mdMap) (liftIO . writeOne resolver writer (tcTxId tc))
  where
    writeOne r w txId (key, value) = do
      mdId <- assignTxMetadataId r
      let row = TxMetadata
            { txMetadataKey   = DbWord64 key
            , txMetadataJson  = renderJson value
            , txMetadataBytes = serialiseSingleton key value
            , txMetadataTxId  = txId
            }
      writeTxMetadata w mdId row

    -- 'Aeson.encode' yields valid UTF-8 by construction, so
    -- 'Text.decodeUtf8'' should never fail here; the 'Nothing'
    -- fallback is defensive.
    renderJson value =
      case Text.decodeUtf8' (LBS.toStrict (Aeson.encode (metadataValueToJson value))) of
        Right t -> Just t
        Left _  -> Nothing
