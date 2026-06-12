{-# LANGUAGE OverloadedStrings #-}

-- | Metadata extractor.
--
-- Emits one @tx_metadata@ row per metadata key in a transaction.
-- Each row stores the single-key CBOR encoding of that pair (matching
-- what the original @cardano-db-sync@ writes) plus the no-schema JSON
-- rendering of the value — SQL @NULL@ when the value contains a
-- Unicode NUL that PostgreSQL cannot store (see 'renderMetadataJson').
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
