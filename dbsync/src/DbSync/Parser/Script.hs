{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | JSON encoders for native scripts.
--
-- Shelley @MultiSig@ and Allegra+ @Timelock@ scripts render to the
-- canonical JSON shape consumed by the @script.json@ column.
-- Dijkstra-era native scripts have no JSON encoder and use the
-- @script.bytes@ CBOR column instead.
module DbSync.Parser.Script
  ( multiSigToJson
  , timelockToJson
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as Text

multiSigToJson
  :: (Shelley.ShelleyEraScript era, Core.NativeScript era ~ Shelley.MultiSig era)
  => Shelley.MultiSig era -> Text
multiSigToJson = renderJson . multiSigValue
  where
    multiSigValue :: (Shelley.ShelleyEraScript era, Core.NativeScript era ~ Shelley.MultiSig era)
                  => Shelley.MultiSig era -> Aeson.Value
    multiSigValue = \case
      Shelley.RequireSignature kh ->
        requireSignatureJson kh
      Shelley.RequireAllOf scripts ->
        allOfJson (multiSigValue <$> toList scripts)
      Shelley.RequireAnyOf scripts ->
        anyOfJson (multiSigValue <$> toList scripts)
      Shelley.RequireMOf m scripts ->
        atLeastJson m (multiSigValue <$> toList scripts)
      _ -> Aeson.Null

timelockToJson
  :: (Allegra.AllegraEraScript era, Core.NativeScript era ~ Allegra.Timelock era)
  => Allegra.Timelock era -> Text
timelockToJson = renderJson . timelockValue
  where
    timelockValue :: (Allegra.AllegraEraScript era, Core.NativeScript era ~ Allegra.Timelock era)
                  => Allegra.Timelock era -> Aeson.Value
    timelockValue = \case
      Shelley.RequireSignature kh ->
        requireSignatureJson kh
      Shelley.RequireAllOf scripts ->
        allOfJson (timelockValue <$> toList scripts)
      Shelley.RequireAnyOf scripts ->
        anyOfJson (timelockValue <$> toList scripts)
      Shelley.RequireMOf m scripts ->
        atLeastJson m (timelockValue <$> toList scripts)
      Allegra.RequireTimeStart slot ->
        timeStartJson slot
      Allegra.RequireTimeExpire slot ->
        timeExpireJson slot
      _ -> Aeson.Null

requireSignatureJson :: Ledger.KeyHash r -> Aeson.Value
requireSignatureJson (Ledger.KeyHash h) =
  Aeson.object
    [ "type"    .= Aeson.String "sig"
    , "keyHash" .= Aeson.String (Crypto.hashToTextAsHex h)
    ]

allOfJson :: [Aeson.Value] -> Aeson.Value
allOfJson scripts =
  Aeson.object
    [ "type"    .= Aeson.String "all"
    , "scripts" .= scripts
    ]

anyOfJson :: [Aeson.Value] -> Aeson.Value
anyOfJson scripts =
  Aeson.object
    [ "type"    .= Aeson.String "any"
    , "scripts" .= scripts
    ]

atLeastJson :: Int -> [Aeson.Value] -> Aeson.Value
atLeastJson req scripts =
  Aeson.object
    [ "type"     .= Aeson.String "atLeast"
    , "required" .= req
    , "scripts"  .= scripts
    ]

timeStartJson :: SlotNo -> Aeson.Value
timeStartJson slot =
  Aeson.object
    [ "type" .= Aeson.String "after"
    , "slot" .= slot
    ]

timeExpireJson :: SlotNo -> Aeson.Value
timeExpireJson slot =
  Aeson.object
    [ "type" .= Aeson.String "before"
    , "slot" .= slot
    ]

renderJson :: Aeson.Value -> Text
renderJson = Text.decodeUtf8 . LBS.toStrict . Aeson.encode
