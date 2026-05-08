{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Bech32 / CIP-14 encoders.
--
-- The eight @mkAssetFingerprint@ vectors are golden — they come from
-- the upstream cardano-db-sync test suite, which itself lifted them
-- from the CIP-14 spec. They cross-check Blake2b-160, Bech32, and
-- the @asset@ HRP together.
--
-- The other encoders use round-trip checks (encode → decode →
-- original bytes) and structural assertions on the HRP and the
-- 29-byte reward-address layout. Hard-coding outputs we generated
-- ourselves would only verify "the encoder behaves the same as it
-- did the day this test was written", not the actual contract.
module DbSync.Util.Bech32Spec (spec) where

import Cardano.Prelude

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Util.Bech32
  ( mkAssetFingerprint
  , serialisePoolKeyHashToBech32
  , serialiseShelleyAddrToBech32
  , serialiseStakeKeyHashToBech32
  , serialiseStakeScriptHashToBech32
  , serialiseToBech32
  , serialiseVrfVkToBech32
  )

spec :: Spec
spec = do
  describe "mkAssetFingerprint / CIP-14 vectors" $ do
    it "empty asset name with policy 7eae28af…dcc373" $
      mkAssetFingerprint
        (hex "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373")
        ""
        `shouldBe` "asset1rjklcrnsdzqp65wjgrg55sy9723kw09mlgvlc3"

    it "empty asset name with policy 7eae28af…dcc37e" $
      mkAssetFingerprint
        (hex "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc37e")
        ""
        `shouldBe` "asset1nl0puwxmhas8fawxp8nx4e2q3wekg969n2auw3"

    it "empty asset name with policy 1e349c9b…81df209" $
      mkAssetFingerprint
        (hex "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209")
        ""
        `shouldBe` "asset1uyuxku60yqe57nusqzjx38aan3f2wq6s93f6ea"

    it "asset name 504154415445 with policy 7eae28af…dcc373" $
      mkAssetFingerprint
        (hex "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373")
        (hex "504154415445")
        `shouldBe` "asset13n25uv0yaf5kus35fm2k86cqy60z58d9xmde92"

    it "asset name 504154415445 with policy 1e349c9b…81df209" $
      mkAssetFingerprint
        (hex "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209")
        (hex "504154415445")
        `shouldBe` "asset1hv4p5tv2a837mzqrst04d0dcptdjmluqvdx9k3"

    it "32-byte asset name (max length) with policy 1e349c9b…81df209" $
      mkAssetFingerprint
        (hex "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209")
        (hex "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373")
        `shouldBe` "asset1aqrdypg669jgazruv5ah07nuyqe0wxjhe2el6f"

    it "swapped policy / asset name reverses the fingerprint" $
      mkAssetFingerprint
        (hex "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373")
        (hex "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209")
        `shouldBe` "asset17jd78wukhtrnmjh3fngzasxm8rck0l2r4hhyyt"

    it "32-byte all-zero asset name" $
      mkAssetFingerprint
        (hex "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373")
        (hex "0000000000000000000000000000000000000000000000000000000000000000")
        `shouldBe` "asset1pkpwyknlvul7az0xx8czhl60pyel45rpje4z8w"

  describe "serialiseToBech32 / round-trip" $ do
    it "decoding the encoded form recovers the input bytes" $
      forM_ samplePayloads $ \payload ->
        decodeBytes "asset" (serialiseToBech32 "asset" payload) `shouldBe` Just payload

    it "decoded HRP equals the input HRP" $
      forM_ samplePayloads $ \payload ->
        decodeHrp (serialiseToBech32 "pool" payload) `shouldBe` Just "pool"

  describe "serialisePoolKeyHashToBech32" $ do
    it "produces a 'pool1…' string for any 28-byte hash" $
      forM_ poolHashSamples $ \h -> do
        let out = serialisePoolKeyHashToBech32 h
        out `shouldSatisfy` Text.isPrefixOf "pool1"

    it "round-trips back to the original 28 bytes" $
      forM_ poolHashSamples $ \h ->
        decodeBytes "pool" (serialisePoolKeyHashToBech32 h) `shouldBe` Just h

  describe "serialiseVrfVkToBech32" $ do
    it "produces a 'vrf_vk1…' string for any 32-byte key" $
      forM_ vrfKeySamples $ \k -> do
        let out = serialiseVrfVkToBech32 k
        out `shouldSatisfy` Text.isPrefixOf "vrf_vk1"

    it "round-trips back to the original 32 bytes" $
      forM_ vrfKeySamples $ \k ->
        decodeBytes "vrf_vk" (serialiseVrfVkToBech32 k) `shouldBe` Just k

  describe "serialiseShelleyAddrToBech32" $ do
    it "uses HRP 'addr' when the header's low bit is 1 (mainnet)" $ do
      let mainnetHeaders = [0x01, 0x11, 0x21, 0x31]  -- base addrs, network 1
      forM_ mainnetHeaders $ \h ->
        serialiseShelleyAddrToBech32 (BS.cons h (BS.replicate 56 0xaa))
          `shouldSatisfy` Text.isPrefixOf "addr1"

    it "uses HRP 'addr_test' when the header's low bit is 0 (testnet)" $ do
      let testnetHeaders = [0x00, 0x10, 0x20, 0x30]
      forM_ testnetHeaders $ \h ->
        serialiseShelleyAddrToBech32 (BS.cons h (BS.replicate 56 0xaa))
          `shouldSatisfy` Text.isPrefixOf "addr_test1"

    it "round-trips back to the original address bytes (mainnet)" $ do
      let addr = BS.cons 0x01 (BS.replicate 56 0x5a)
      decodeBytes "addr" (serialiseShelleyAddrToBech32 addr) `shouldBe` Just addr

    it "round-trips back to the original address bytes (testnet)" $ do
      let addr = BS.cons 0x00 (BS.replicate 56 0x5a)
      decodeBytes "addr_test" (serialiseShelleyAddrToBech32 addr)
        `shouldBe` Just addr

  describe "serialiseStakeKeyHashToBech32" $ do
    let cred = BS.replicate 28 0xaa

    it "uses HRP 'stake' on mainnet" $
      serialiseStakeKeyHashToBech32 True cred
        `shouldSatisfy` Text.isPrefixOf "stake1"

    it "uses HRP 'stake_test' on testnet" $
      serialiseStakeKeyHashToBech32 False cred
        `shouldSatisfy` Text.isPrefixOf "stake_test1"

    it "encoded payload is 29 bytes: header || credential" $ do
      decodeBytes "stake" (serialiseStakeKeyHashToBech32 True cred)
        `shouldBe` Just (BS.cons 0xE1 cred)
      decodeBytes "stake_test" (serialiseStakeKeyHashToBech32 False cred)
        `shouldBe` Just (BS.cons 0xE0 cred)

  describe "serialiseStakeScriptHashToBech32" $ do
    let cred = BS.replicate 28 0xbb

    it "uses HRP 'stake' on mainnet, script header 0xF1" $
      decodeBytes "stake" (serialiseStakeScriptHashToBech32 True cred)
        `shouldBe` Just (BS.cons 0xF1 cred)

    it "uses HRP 'stake_test' on testnet, script header 0xF0" $
      decodeBytes "stake_test" (serialiseStakeScriptHashToBech32 False cred)
        `shouldBe` Just (BS.cons 0xF0 cred)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Decode a hex literal. Panics on malformed input — only used for
-- compile-time test vectors.
hex :: ByteString -> ByteString
hex bs = case Base16.decode bs of
  Right b -> b
  Left e  -> panic ("hex: " <> show e)

-- | Decode a Bech32 string and return the payload bytes when the HRP
-- matches @expected@. 'Nothing' if decoding fails or the HRP differs —
-- the test asserts on @Just bytes@ to make both failures visible.
decodeBytes :: Text -> Text -> Maybe ByteString
decodeBytes expected encoded = do
  (hrp, dataPart) <- rightToMaybe (Bech32.decodeLenient encoded)
  guard (Bech32.humanReadablePartToText hrp == expected)
  Bech32.dataPartToBytes dataPart

-- | Decode just the HRP from a Bech32 string.
decodeHrp :: Text -> Maybe Text
decodeHrp encoded = do
  (hrp, _) <- rightToMaybe (Bech32.decodeLenient encoded)
  pure (Bech32.humanReadablePartToText hrp)

-- ---------------------------------------------------------------------------
-- Sample payloads
-- ---------------------------------------------------------------------------

-- | A handful of distinct byte patterns used to drive round-trip
-- checks. Length isn't fixed — round-trip works for any payload Bech32
-- can carry.
samplePayloads :: [ByteString]
samplePayloads =
  [ ""
  , BS.replicate 1  0xab
  , BS.replicate 20 0x00
  , BS.replicate 28 0xff
  , BS.replicate 32 0x5a
  , hex "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
  ]

-- | Distinct 28-byte pool hashes — the all-zero / all-one edges plus
-- a non-trivial pattern.
poolHashSamples :: [ByteString]
poolHashSamples =
  [ BS.replicate 28 0x00
  , BS.replicate 28 0xff
  , hex "00112233445566778899aabbccddeeff00112233445566778899aabb"
  ]

-- | Distinct 32-byte VRF verification keys.
vrfKeySamples :: [ByteString]
vrfKeySamples =
  [ BS.replicate 32 0x00
  , BS.replicate 32 0x5a
  , hex "0000000000000000000000000000000000000000000000000000000000000000"
  ]
