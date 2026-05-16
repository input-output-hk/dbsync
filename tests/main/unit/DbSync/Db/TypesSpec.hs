{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for foundational types in 'DbSync.Db.Types'.
--
-- Three classes of property are exercised:
--
--   1. 'DbInt65' is a sign-magnitude packing of 'Int64' into 'Word64' —
--      encode\/decode must round-trip including the 'minBound'
--      edge case where 'abs' would overflow.
--   2. 'bInt65' \/ 'bWord128' produce decimal ASCII suitable for the
--      PostgreSQL @numeric@ COPY format.
--   3. Each enum's COPY builder emits the exact ASCII string the
--      original schema's @CHECK@ constraints require — a single
--      bad string here is a silent data-corruption bug, so we hard
--      code the expected output for every constructor.
module DbSync.Db.TypesSpec (spec) where

import Cardano.Prelude

import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Scientific as Sci
import Data.WideWord (Word128)

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

import DbSync.Db.Types
  ( AnchorType (..)
  , DbLovelace (..)
  , DbWord64 (..)
  , GovActionType (..)
  , RewardSource (..)
  , ScriptPurpose (..)
  , ScriptType (..)
  , SyncState (..)
  , Vote (..)
  , VoterRole (..)
  , bAnchorType
  , bGovActionType
  , bInt65
  , bRewardSource
  , bScriptPurpose
  , bScriptType
  , bSyncState
  , bVote
  , bVoterRole
  , bWord128
  , fromDbInt65
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

  describe "Vote builder" $
    it "emits the upper-case Yes/No/Abstain strings" $ do
      bs (bVote VoteYes)     `shouldBe` "Yes"
      bs (bVote VoteNo)      `shouldBe` "No"
      bs (bVote VoteAbstain) `shouldBe` "Abstain"

  describe "VoterRole builder" $
    it "emits the upstream-style camel-case role strings" $ do
      bs (bVoterRole ConstitutionalCommittee) `shouldBe` "ConstitutionalCommittee"
      bs (bVoterRole DRep)                    `shouldBe` "DRep"
      bs (bVoterRole SPO)                     `shouldBe` "SPO"

  describe "GovActionType builder" $ do
    it "emits the long camel-case action names" $ do
      bs (bGovActionType ParameterChange)     `shouldBe` "ParameterChange"
      bs (bGovActionType HardForkInitiation)  `shouldBe` "HardForkInitiation"
      bs (bGovActionType TreasuryWithdrawals) `shouldBe` "TreasuryWithdrawals"
      bs (bGovActionType NoConfidence)        `shouldBe` "NoConfidence"
      bs (bGovActionType NewConstitution)     `shouldBe` "NewConstitution"
      bs (bGovActionType InfoAction)          `shouldBe` "InfoAction"

    it "maps NewCommitteeType → \"NewCommittee\" (constructor differs from PG)" $
      bs (bGovActionType NewCommitteeType) `shouldBe` "NewCommittee"

  describe "AnchorType builder" $
    it "emits the snake-case anchor-kind strings" $ do
      bs (bAnchorType GovActionAnchor)      `shouldBe` "gov_action"
      bs (bAnchorType DrepAnchor)           `shouldBe` "drep"
      bs (bAnchorType OtherAnchor)          `shouldBe` "other"
      bs (bAnchorType VoteAnchor)           `shouldBe` "vote"
      bs (bAnchorType CommitteeDeRegAnchor) `shouldBe` "committee_dereg"
      bs (bAnchorType ConstitutionAnchor)   `shouldBe` "constitution"

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
