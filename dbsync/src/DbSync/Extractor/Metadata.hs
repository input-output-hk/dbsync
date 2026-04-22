{-# LANGUAGE OverloadedStrings #-}

-- | Metadata extractor.
--
-- Extracts transaction metadata into the @tx_metadata@ table.
-- Each metadata key in a transaction produces a separate row.
--
-- During 'IngestChainHistory', metadata bytes are stored as-is (CBOR).
-- JSON rendering is deferred (set to NULL) — it can be populated
-- post-load if needed.
module DbSync.Extractor.Metadata
  ( metadataExtractor
  ) where

import Cardano.Prelude

import qualified Data.ByteString as BS

import DbSync.Block.Types (GenericTx (..))
import DbSync.Db.Schema.Ids (TxId (..))
import DbSync.Db.Schema.Metadata
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Resolver (IdResolver (..))
import DbSync.Writer (Writer (..))

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

-- | Process metadata for each transaction.
--
-- If a transaction has metadata bytes, we write a single tx_metadata
-- row with key=0 and the raw CBOR bytes. Full per-key decomposition
-- will be added when we have a CBOR metadata parser.
processMetadata :: ProcessBlockFn
processMetadata resolver writer ctx = do
  forM_ (bcTxs ctx) $ \tc -> do
    let txId = tcTxId tc
        gtx  = tcGenTx tc
    case txMetadata gtx of
      Nothing -> pure ()
      Just mdBytes
        | BS.null mdBytes -> pure ()
        | otherwise -> do
            mdId <- assignTxMetadataId resolver
            let md = TxMetadata
                  { txMetadataKey   = DbWord64 0  -- placeholder key
                  , txMetadataJson  = Nothing      -- JSON rendering deferred
                  , txMetadataBytes = mdBytes
                  , txMetadataTxId  = txId
                  }
            writeTxMetadata writer mdId md
