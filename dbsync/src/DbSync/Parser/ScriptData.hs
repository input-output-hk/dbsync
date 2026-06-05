{-# LANGUAGE OverloadedStrings #-}

-- | JSON encoding for Plutus datums and redeemer data.
--
-- Produces the canonical tagged-object shape stored in the
-- @datum.value@ and @redeemer_data.value@ JSONB columns:
-- @{"int":n}@, @{"bytes":hex}@, @{"list":[…]}@,
-- @{"map":[{"k":…,"v":…},…]}@, @{"constructor":n,"fields":[…]}@.
module DbSync.Parser.ScriptData
  ( plutusDataToJson
  ) where

import Cardano.Prelude

import Cardano.Ledger.Plutus.Data (Data (..), getPlutusData)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified PlutusLedgerApi.V1 as Plutus

-- | Render a Plutus 'Data' value as canonical JSON text.
plutusDataToJson :: Data era -> Text
plutusDataToJson = renderJson . plutusValue . getPlutusData
  where
    plutusValue :: Plutus.Data -> Aeson.Value
    plutusValue = \case
      Plutus.Constr i fields ->
        Aeson.object
          [ "constructor" .= i
          , "fields"      .= Aeson.Array (Vector.fromList (plutusValue <$> fields))
          ]
      Plutus.Map kvs ->
        Aeson.object
          [ "map" .= Aeson.Array (Vector.fromList (kvJson <$> kvs))
          ]
      Plutus.List xs ->
        Aeson.object
          [ "list" .= Aeson.Array (Vector.fromList (plutusValue <$> xs))
          ]
      Plutus.I n ->
        Aeson.object [ "int" .= n ]
      Plutus.B bs ->
        Aeson.object [ "bytes" .= Text.decodeUtf8 (Base16.encode bs) ]

    kvJson :: (Plutus.Data, Plutus.Data) -> Aeson.Value
    kvJson (k, v) =
      Aeson.object
        [ "k" .= plutusValue k
        , "v" .= plutusValue v
        ]

renderJson :: Aeson.Value -> Text
renderJson = Text.decodeUtf8 . LBS.toStrict . Aeson.encode
