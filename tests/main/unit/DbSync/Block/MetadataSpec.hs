{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the metadata extraction helpers.
--
-- Round-trip checks (encode → decode → original) anchor the CBOR
-- contract; spot vectors anchor the no-schema JSON shape against the
-- mapping documented in @cardano-api@'s @TxMetadata@ haddock.
module DbSync.Block.MetadataSpec (spec) where

import Cardano.Prelude

import qualified Cardano.Ledger.Binary.Decoding as Binary
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Block.Metadata
  ( Metadatum (..)
  , metadataValueToJson
  , serialiseSingleton
  )

spec :: Spec
spec = do
  describe "serialiseSingleton / round-trip" $ do
    it "decoding the encoded singleton recovers the (key, value) pair" $
      forM_ sampleValues $ \(key, value) ->
        decodeSingleton (serialiseSingleton key value)
          `shouldBe` Right (Map.singleton key value)

    it "produces the canonical 1-entry map prefix 0xa1" $ do
      let bytes = serialiseSingleton 0 (I 0)
      BS.head bytes `shouldBe` 0xa1

  describe "metadataValueToJson / scalars" $ do
    it "encodes integers as JSON numbers" $
      jsonText (I 42) `shouldBe` "42"

    it "encodes text as JSON strings" $
      jsonText (S "hello") `shouldBe` "\"hello\""

    it "encodes bytes with the 0x prefix and lowercase hex" $
      jsonText (B (BS.pack [0xab, 0xcd, 0xef]))
        `shouldBe` "\"0xabcdef\""

  describe "metadataValueToJson / containers" $ do
    it "encodes lists as JSON arrays preserving order" $
      jsonText (List [I 1, I 2, I 3]) `shouldBe` "[1,2,3]"

    it "encodes int-keyed maps with stringified keys" $
      jsonText (Map [(I 1, S "a"), (I 2, S "b")])
        `shouldBe` "{\"1\":\"a\",\"2\":\"b\"}"

    it "encodes byte-keyed maps with the 0x prefix in the key" $
      jsonText (Map [(B (BS.pack [0x01]), S "x")])
        `shouldBe` "{\"0x01\":\"x\"}"

  describe "metadataValueToJson / lossy mapping" $ do
    -- Documents the known no-schema lossiness: when an integer
    -- key and a text key render to the same JSON key, JSON
    -- object semantics drop one of them. Last-occurrence wins.
    it "integer key 1 and text key '1' collide and one is dropped" $
      jsonText (Map [(I 1, S "i"), (S "1", S "t")])
        `shouldBe` "{\"1\":\"t\"}"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | UTF-8 bytes of @Aeson.encode (metadataValueToJson v)@. Always
-- pure ASCII for our fixtures, so direct 'show' would also work,
-- but going via 'encode' exercises the same path the extractor uses.
jsonText :: Metadatum -> Text
jsonText = decodeUtf8 . LBS.toStrict . Aeson.encode . metadataValueToJson

-- | Decode the bytes 'serialiseSingleton' produced. Wraps
-- 'Cardano.Ledger.Binary.Decoding.decodeFull''.
decodeSingleton :: ByteString -> Either Binary.DecoderError (Map Word64 Metadatum)
decodeSingleton = Binary.decodeFull' Binary.shelleyProtVer

-- | Vectors covering each Metadatum constructor as the value side
-- of a singleton map.
sampleValues :: [(Word64, Metadatum)]
sampleValues =
  [ (0,                    I 0)
  , (1,                    I (-7))
  , (42,                   S "hello")
  , (100,                  B (BS.pack [0xde, 0xad, 0xbe, 0xef]))
  , (674,                  List [I 1, I 2, I 3])
  , (maxBound,             Map [(S "k", I 9)])
  , (1985,                 List [Map [(I 0, S "nested")]])
  ]
