{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CBOR extractor.
--
-- Stores raw CBOR-encoded transaction bytes alongside the parsed data,
-- enabling downstream consumers to re-serialise or replay transactions.
-- Disabled by default (config: @"cbor": { "enabled": false }@) since
-- the data is large and only needed for specific use cases.
module DbSync.Extractor.Cbor
  ( cborExtractor
  ) where

import Cardano.Prelude

import DbSync.Block.Types (GenericTx (..))
import DbSync.Db.Schema.CBOR
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Resolver (IdResolver (..))
import DbSync.Writer (Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

cborExtractor :: ExtractorDef
cborExtractor = ExtractorDef
  { pdName         = "cbor"
  , pdVersion      = 1
  , pdDependencies = [("core", 1)]
  , pdTables       = [txCborTableDef]
  , pdProcess      = processCbor
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processCbor :: ProcessBlockFn
processCbor resolver writer ctx = do
  forM_ (bcTxs ctx) $ \tc -> do
    let txId = tcTxId tc
        gtx  = tcGenTx tc

    -- Only write if CBOR bytes are available (Nothing for Byron)
    case txCborRaw gtx of
      Just !cborBytes -> do
        tcId <- assignTxCborId resolver
        let txCbor = TxCbor
              { txCborTxId  = txId
              , txCborBytes = cborBytes
              }
        writeTxCbor writer tcId txCbor
      Nothing ->
        pure ()
