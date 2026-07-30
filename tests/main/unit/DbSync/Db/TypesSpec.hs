{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for foundational types in 'DbSync.Db.Types': 'DbInt65'
-- round-trips (including the 'minBound' edge case), COPY builders
-- (decimal ASCII, the exact enum strings the schema @CHECK@s require),
-- and the @numeric@ codec conversions.
module DbSync.Db.TypesSpec (spec) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Scientific as Sci
import Data.WideWord (Word128)

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

import DbSync.Db.Types
  ( DbLovelace (..)
  , DbWord64 (..)
  , RewardSource (..)
  , ScriptPurpose (..)
  , ScriptType (..)
  , SyncState (..)
  , bInt65
  , bRational
  , bRewardSource
  , bScriptPurpose
  , bScriptType
  , bSyncState
  , bWord128
  , fromDbInt65
  , rationalToScientific
  , scientificToWord128
  , scientificToWord64
  , toDbInt65
  , word128ToScientific
  , word64ToScientific
  )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

bs :: Builder -> ByteString
bs = LBS.toStrict . toLazyByteString

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "DbInt65 round-trip" $ do
    it "0 round-trips" $
      fromDbInt65 (toDbInt65 0) `shouldBe` 0

    it "small positive round-trips" $
      fromDbInt65 (toDbInt65 42) `shouldBe` 42

    it "small negative round-trips" $
      fromDbInt65 (toDbInt65 (-42)) `shouldBe` (-42)

    it "maxBound round-trips" $
      fromDbInt65 (toDbInt65 maxBound) `shouldBe` (maxBound :: Int64)

    it "minBound round-trips (the abs-would-overflow case)" $
      fromDbInt65 (toDbInt65 minBound) `shouldBe` (minBound :: Int64)

    prop "is total: fromDbInt65 . toDbInt65 = id over Int64" $
      \i -> fromDbInt65 (toDbInt65 i) == (i :: Int64)

  describe "bInt65" $ do
    it "encodes 0 as \"0\"" $
      bs (bInt65 (toDbInt65 0)) `shouldBe` "0"

    it "encodes 1234567890 as decimal ASCII" $
      bs (bInt65 (toDbInt65 1234567890)) `shouldBe` "1234567890"

    it "encodes a negative as a signed decimal" $
      bs (bInt65 (toDbInt65 (-7))) `shouldBe` "-7"

    it "encodes minBound as the full Int64 range" $
      bs (bInt65 (toDbInt65 minBound)) `shouldBe` "-9223372036854775808"

  describe "bWord128" $ do
    it "encodes 0 as \"0\"" $
      bs (bWord128 0) `shouldBe` "0"

    it "encodes a Word64-fitting value correctly" $
      bs (bWord128 1000000) `shouldBe` "1000000"

    it "encodes maxBound @Word64 correctly" $
      bs (bWord128 (fromIntegral (maxBound :: Word64)))
        `shouldBe` "18446744073709551615"

    it "encodes a value that exceeds maxBound @Word64" $
      -- 2^64 = 18446744073709551616
      bs (bWord128 (fromIntegral (maxBound :: Word64) + 1 :: Word128))
        `shouldBe` "18446744073709551616"

  describe "ScriptPurpose builder" $ do
    it "emits the PG strings the original schema's CHECK accepts" $ do
      bs (bScriptPurpose Spend)   `shouldBe` "spend"
      bs (bScriptPurpose Mint)    `shouldBe` "mint"
      bs (bScriptPurpose Cert)    `shouldBe` "cert"
      bs (bScriptPurpose Rewrd)   `shouldBe` "reward"
      bs (bScriptPurpose Vote)    `shouldBe` "vote"
      bs (bScriptPurpose Propose) `shouldBe` "propose"

  describe "ScriptType builder" $ do
    it "emits camel-case PG strings (multisig, plutusV1 …)" $ do
      bs (bScriptType MultiSig) `shouldBe` "multisig"
      bs (bScriptType Timelock) `shouldBe` "timelock"
      bs (bScriptType PlutusV1) `shouldBe` "plutusV1"
      bs (bScriptType PlutusV2) `shouldBe` "plutusV2"
      bs (bScriptType PlutusV3) `shouldBe` "plutusV3"
      bs (bScriptType PlutusV4) `shouldBe` "plutusV4"

  describe "RewardSource builder" $ do
    it "emits snake-case PG strings (the @Rwd@ prefix is haskell-side only)" $ do
      bs (bRewardSource RwdLeader)         `shouldBe` "leader"
      bs (bRewardSource RwdMember)         `shouldBe` "member"
      bs (bRewardSource RwdReserves)       `shouldBe` "reserves"
      bs (bRewardSource RwdTreasury)       `shouldBe` "treasury"
      bs (bRewardSource RwdDepositRefund)  `shouldBe` "refund"
      bs (bRewardSource RwdProposalRefund) `shouldBe` "proposal_refund"

  describe "SyncState builder" $
    it "emits the legacy lagging/following strings" $ do
      bs (bSyncState SyncLagging)   `shouldBe` "lagging"
      bs (bSyncState SyncFollowing) `shouldBe` "following"

  -- ---------------------------------------------------------------------
  -- Scientific / Word conversions
  --
  -- These exercise the @numeric@ encoder/decoder pair without a database
  -- round-trip. PostgreSQL normalises trailing zeros on the wire — a
  -- value like 380_000_000_000_000_000 comes back from hasql as
  -- 'Sci.Scientific' 38 16 (coefficient 38, exponent 16). Reading just
  -- the coefficient is a silent corruption bug; the helpers must honour
  -- the exponent.
  -- ---------------------------------------------------------------------
  describe "scientificToWord64" $ do
    it "decodes a coefficient-plus-exponent representation correctly" $
      -- 38 * 10^16 = 380_000_000_000_000_000
      scientificToWord64 (Sci.scientific 38 16) `shouldBe` 380_000_000_000_000_000

    it "decodes 0 as 0" $
      scientificToWord64 0 `shouldBe` 0

    it "decodes maxBound @Word64 (above Int64 range) without truncation" $
      scientificToWord64 (fromInteger (toInteger (maxBound :: Word64)))
        `shouldBe` (maxBound :: Word64)

    prop "round-trips Word64 through normalised Scientific" $ \(w :: Word64) ->
      scientificToWord64 (Sci.normalize (word64ToScientific w)) == w

  describe "scientificToWord128" $ do
    it "decodes a normalised Scientific that fits in Word64" $
      scientificToWord128 (Sci.scientific 38 16)
        `shouldBe` (380_000_000_000_000_000 :: Word128)

    it "decodes a Scientific that exceeds Word64" $
      -- 2^64 = 18446744073709551616
      let sci = Sci.scientific 18446744073709551616 0
      in scientificToWord128 sci
           `shouldBe` (fromIntegral (maxBound :: Word64) + 1 :: Word128)

    it "round-trips a curated set of values likely to expose exponent bugs" $ do
      -- Each value is one PostgreSQL plausibly normalises with trailing zeros.
      let problemValues :: [Word128]
          problemValues =
            [ 0
            , 1
            , 10
            , 100
            , 1_000_000                          -- 1 ADA in lovelace
            , 36_000_000_000                     -- ~36k ADA, typical epoch fees
            , 380_000_000_000_000_000            -- ~38B ADA, typical epoch out_sum
            , 45_000_000_000_000_000             -- total ADA supply in lovelace
            , maxBound                           -- Word128 max
            ]
      mapM_
        (\w -> scientificToWord128 (Sci.normalize (word128ToScientific w))
                 `shouldBe` w)
        problemValues

  describe "DbLovelace ↔ Scientific" $
    prop "round-trips through normalised Scientific" $ \w ->
      let lov = DbLovelace w
          encoded = word64ToScientific (unDbLovelace lov)
          decoded = DbLovelace (scientificToWord64 (Sci.normalize encoded))
      in decoded == lov

  describe "DbWord64 ↔ Scientific" $
    prop "round-trips through normalised Scientific" $ \w ->
      let dw = DbWord64 w
          encoded = word64ToScientific (unDbWord64 dw)
          decoded = DbWord64 (scientificToWord64 (Sci.normalize encoded))
      in decoded == dw

  describe "bRational" $ do
    it "encodes a terminating decimal exactly" $
      bs (bRational 0.075) `shouldBe` "0.075"

    it "encodes whole numbers in fixed notation" $
      bs (bRational 15) `shouldBe` "15.0"

    it "encodes the longest terminating ledger denominator exactly" $ do
      -- 1/2^63 = 5^63/10^63: 63 fractional digits, the worst case for
      -- a Word64-bounded denominator.
      let digits = show (5 ^ (63 :: Int) :: Integer)
          expected = "0." <> BS8.pack (replicate (63 - length digits) '0' <> digits)
      bs (bRational (1 % (2 ^ (63 :: Int)))) `shouldBe` expected

    it "truncates a non-terminating expansion at the fractional-digit cap" $
      bs (bRational (2 % 3)) `shouldBe` ("0." <> BS8.replicate 80 '6')

    it "truncates negative values toward zero" $
      bs (bRational ((-2) % 3)) `shouldBe` ("-0." <> BS8.replicate 80 '6')

  describe "rationalToScientific" $
    prop "round-trips terminating rationals exactly" $ \(n :: Int64) (a :: Word8) (b :: Word8) ->
      -- Denominators 2^a·5^b terminate within the cap; toRational on
      -- the Scientific must recover the input exactly.
      let denom = 2 ^ (a `mod` 30) * 5 ^ (b `mod` 30) :: Integer
          r = toInteger n % denom
      in toRational (rationalToScientific r) == r
