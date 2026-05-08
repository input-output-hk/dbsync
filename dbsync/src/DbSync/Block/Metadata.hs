{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Metadata extraction and re-serialisation.
--
-- @cardano-ledger@ exposes a uniform @metadataTxAuxDataL@ lens on
-- the 'EraTxAuxData' class, so a single 'getMetadata' works across
-- every Shelley+ era without per-era pattern matching.
module DbSync.Block.Metadata
  ( -- * Re-export
    Metadatum (..)

    -- * Extraction
  , getMetadata

    -- * Encoding helpers
  , serialiseSingleton
  , metadataValueToJson
  ) where

import Cardano.Prelude

import Cardano.Ledger.Binary.Encoding (serialize')
import Cardano.Ledger.Binary.Version (shelleyProtVer)
import Cardano.Ledger.Core (EraTxAuxData, TxAuxData, metadataTxAuxDataL)
import Cardano.Ledger.Metadata (Metadatum (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector
import Lens.Micro ((^.))

-- ---------------------------------------------------------------------------
-- * Extraction
-- ---------------------------------------------------------------------------

-- | Project the metadata map out of an era's auxiliary data.
-- Strips the script payload that Allegra+ wrappers also carry.
getMetadata :: EraTxAuxData era => TxAuxData era -> Map Word64 Metadatum
getMetadata aux = aux ^. metadataTxAuxDataL

-- ---------------------------------------------------------------------------
-- * Encoding helpers
-- ---------------------------------------------------------------------------

-- | Re-encode a single @(key, value)@ as the bytes stored in
-- @tx_metadata.bytes@. Matches the original's
-- @serialiseTxMetadataToCbor (Map.singleton key value)@.
serialiseSingleton :: Word64 -> Metadatum -> ByteString
serialiseSingleton key value =
  serialize' shelleyProtVer (Map.singleton key value)

-- | Render a 'Metadatum' as no-schema JSON. Mirrors @cardano-api@'s
-- @metadataValueToJsonNoSchema@ — total but lossy: distinct
-- 'Metadatum' values can collapse to the same JSON (e.g. integer key
-- @1@ and string key @"1"@ both render as @\"1\"@). Downstream
-- tooling (Koios, Blockfrost) consumes @tx_metadata.json@ expecting
-- exactly this shape, so the mapping is fixed by ecosystem
-- compatibility, not by us.
metadataValueToJson :: Metadatum -> Aeson.Value
metadataValueToJson = go
  where
    go :: Metadatum -> Aeson.Value
    go (I n)     = Aeson.Number (fromInteger n)
    go (B bs)    = Aeson.String (bytesPrefix <> Text.decodeLatin1 (Base16.encode bs))
    go (S txt)   = Aeson.String txt
    go (List xs) = Aeson.Array (Vector.fromList (map go xs))
    go (Map kvs) =
      Aeson.object
        [ (Aeson.Key.fromText (renderKey k), go v) | (k, v) <- kvs ]

    -- JSON keys are strings; metadata keys aren't. Scalars render
    -- directly; structured keys round-trip through JSON as a string.
    renderKey :: Metadatum -> Text
    renderKey (I n)   = show n
    renderKey (B bs)  = bytesPrefix <> Text.decodeLatin1 (Base16.encode bs)
    renderKey (S txt) = txt
    renderKey other   =
      Text.Lazy.toStrict (Aeson.Text.encodeToLazyText (go other))

    bytesPrefix :: Text
    bytesPrefix = "0x"
