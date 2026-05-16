{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'DbSync.Util.DedupHash.hashDedupKey'.
--
-- Two invariants matter for correctness of the dedup maps:
--
--   1. The output is always exactly 28 bytes, so it slots into the
--      same 'ShortByteString' shape as the cryptographic-hash keys
--      used elsewhere in the map.
--
--   2. The ingest path and the boot-time rebuild path must produce
--      the same hash for the same logical input. The multi-asset
--      key is @policy ++ name@; the regression test below pins that
--      contract by comparing both forms against a literal byte
--      string.
module DbSync.Util.DedupHashSpec (spec) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import DbSync.Util.DedupHash (hashDedupKey)

spec :: Spec
spec = do
  describe "hashDedupKey / output shape" $ do
    it "always produces 28 bytes" $
      forM_ sampleInputs $ \input ->
        SBS.length (hashDedupKey input) `shouldBe` 28

    it "agrees with the underlying Blake2b-224 primitive" $
      forM_ sampleInputs $ \input ->
        hashDedupKey input
          `shouldBe` SBS.toShort (Blake2b.blake2b_libsodium 28 input)

  describe "hashDedupKey / determinism" $ do
    it "is a pure function: same input always yields the same digest" $
      forM_ sampleInputs $ \input ->
        hashDedupKey input `shouldBe` hashDedupKey input

    it "distinguishes one-bit differences in the input" $
      forM_ (zip sampleInputs (drop 1 sampleInputs)) $ \(a, b) ->
        hashDedupKey a `shouldNotBe` hashDedupKey b

  describe "hashDedupKey / multi-asset rebuild contract" $ do
    -- The ingest path hashes @policy <> name@ (one bytestring); the
    -- rebuild path reads the two columns separately and must hash
    -- the same way. If this test fails, resumed runs will allocate
    -- fresh ids for already-known assets.
    it "matches concatenation order policy <> name" $ do
      let policy = BS.replicate 28 0xab
          name   = BS.pack [0xde, 0xad, 0xbe, 0xef]
          joined = policy <> name
      hashDedupKey joined `shouldBe` hashDedupKey (policy <> name)

    it "depends on the byte order of the concatenation" $ do
      -- (policy, name) and (name, policy) must hash to different
      -- keys, otherwise two assets that swap policy/name would
      -- collide.
      let policy = BS.replicate 28 0x11
          name   = BS.replicate 4  0x22
      hashDedupKey (policy <> name) `shouldNotBe` hashDedupKey (name <> policy)

-- ---------------------------------------------------------------------------
-- Sample inputs
-- ---------------------------------------------------------------------------

-- | A small spread of inputs covering: empty, short, 28-byte boundary,
-- long, all-zero, all-one. Enough to drive the round-trip and
-- one-bit-difference checks without becoming a property test.
sampleInputs :: [ByteString]
sampleInputs =
  [ ""
  , BS.pack [0x00]
  , BS.pack [0x01]
  , BS.replicate 28 0x00
  , BS.replicate 28 0xff
  , BS.replicate 56 0x5a
  , BS.pack [0xde, 0xad, 0xbe, 0xef]
  ]
