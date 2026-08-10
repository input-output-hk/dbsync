{-# LANGUAGE OverloadedStrings #-}

-- | Writes one @tx_metadata@ row per metadata key in a transaction.
-- The row holds the single-key CBOR encoding of the pair plus the
-- no-schema JSON rendering of the value. The JSON column takes SQL
-- @NULL@ when the value holds a Unicode NUL that PostgreSQL rejects;
-- see 'renderMetadataJson'.
module DbSync.Extractor.Metadata
  ( metadataExtractor
  ) where

import Cardano.Prelude

import qualified Data.Map.Strict as Map

import DbSync.Parser.Metadata (renderMetadataJson, serialiseSingleton)
import DbSync.Parser.Types (GenericTx (..))
import DbSync.Db.Schema.Metadata
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

metadataExtractor :: ExtractorDef
metadataExtractor = ExtractorDef
  { pdName    = "metadata"
  , pdTables  = [txMetadataTableDef]
  , pdProcess = processMetadata
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

-- | An empty map, where the parser saw aux-data that carried no
-- metadata, gives no rows. A failed phase-2 tx is skipped, because the
-- chain does not record its metadata.
processMetadata :: ProcessBlockFn
processMetadata ctx = do
  writer <- asks getWriter
  forM_ (bcTxs ctx) $ \tc ->
    when (txValidContract (tcGenTx tc)) $
      case txMetadata (tcGenTx tc) of
        Nothing    -> pure ()
        Just mdMap -> forM_ (Map.toAscList mdMap) (liftIO . writeOne writer (tcTxId tc))
  where
    writeOne w txId (key, value) =
      writeTxMetadata w TxMetadata
        { txMetadataKey   = DbWord64 key
        , txMetadataJson  = renderMetadataJson value
        , txMetadataBytes = serialiseSingleton key value
        , txMetadataTxId  = txId
        }
