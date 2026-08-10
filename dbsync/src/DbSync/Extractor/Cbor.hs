{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stores the raw CBOR bytes of each transaction, so downstream
-- consumers can re-serialise or replay it. The config disables this
-- extractor by default, because the data is large and few consumers
-- need it.
module DbSync.Extractor.Cbor
  ( cborExtractor
  ) where

import Cardano.Prelude

import DbSync.Parser.Types (GenericTx (..))
import DbSync.Db.Schema.CBOR
import DbSync.Extractor (ExtractorDef (..), ProcessBlockFn, BlockContext (..), TxContext (..))
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor definition
-- ---------------------------------------------------------------------------

cborExtractor :: ExtractorDef
cborExtractor = ExtractorDef
  { pdName    = "cbor"
  , pdTables  = [txCborTableDef]
  , pdProcess = processCbor
  }

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

processCbor :: ProcessBlockFn
processCbor ctx = do
  writer <- asks getWriter
  forM_ (bcTxs ctx) $ \tc -> do
    let txId = tcTxId tc
        gtx  = tcGenTx tc

    -- Only write if CBOR bytes are available (Nothing for Byron)
    case txCborRaw gtx of
      Just !cborBytes ->
        liftIO $ writeTxCbor writer TxCbor
          { txCborTxId  = txId
          , txCborBytes = cborBytes
          }
      Nothing ->
        pure ()
