{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @off_chain_vote_*@ COPY encoders: JSONB passthrough,
-- nullable-field NULL encoding, and timestamp format.
module DbSync.Schema.OffChainVoteSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Ids
  ( OffChainVoteDataId (..)
  , VotingAnchorId (..)
  )
import DbSync.Db.Schema.OffChainVote
  ( OffChainVoteAuthor (..)
  , OffChainVoteData (..)
  , OffChainVoteFetchError (..)
  , encodeOffChainVoteAuthorCopy
  , encodeOffChainVoteDataCopy
  , encodeOffChainVoteFetchErrorCopy
  )

spec :: Spec
spec = do
  describe "encodeOffChainVoteDataCopy" $ do
    it "writes the json column verbatim" $ do
      let fields = BS8.split '\t' (BS8.init (encodeOffChainVoteDataCopy sampleVoteData))
      fields !! 2 `shouldBe` "{\"title\":\"Sample\"}"

    it "encodes is_valid Nothing as \\N" $ do
      let fields = BS8.split '\t' (BS8.init (encodeOffChainVoteDataCopy sampleVoteData))
      fields !! 7 `shouldBe` "\\N"

    it "encodes is_valid Just True as t" $ do
      let row = encodeOffChainVoteDataCopy
                  sampleVoteData { offChainVoteDataIsValid = Just True }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 7 `shouldBe` "t"

  describe "encodeOffChainVoteAuthorCopy" $
    it "encodes a missing optional warning as \\N" $ do
      let fields = BS8.split '\t' (BS8.init (encodeOffChainVoteAuthorCopy sampleAuthor))
      fields !! 5 `shouldBe` "\\N"

  describe "encodeOffChainVoteFetchErrorCopy" $ do
    it "encodes fetch_time as YYYY-MM-DD HH:MM:SS" $ do
      let row = encodeOffChainVoteFetchErrorCopy sampleFetchError
          fields = BS8.split '\t' (BS8.init row)
      fields !! 2 `shouldBe` "2024-06-15 12:34:56"

    it "encodes retry_count as decimal" $ do
      let row = encodeOffChainVoteFetchErrorCopy sampleFetchError
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "5"

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleVoteData :: OffChainVoteData
sampleVoteData = OffChainVoteData
  { offChainVoteDataVotingAnchorId = VotingAnchorId 7
  , offChainVoteDataHash           = "\x01\x02\x03\x04"
  , offChainVoteDataJson           = "{\"title\":\"Sample\"}"
  , offChainVoteDataBytes          = "\x01\x02\x03\x04"
  , offChainVoteDataWarning        = Nothing
  , offChainVoteDataLanguage       = "en-US"
  , offChainVoteDataComment        = Nothing
  , offChainVoteDataIsValid        = Nothing
  }

sampleAuthor :: OffChainVoteAuthor
sampleAuthor = OffChainVoteAuthor
  { offChainVoteAuthorOffChainVoteDataId = OffChainVoteDataId 11
  , offChainVoteAuthorName               = Just "Alice"
  , offChainVoteAuthorWitnessAlgorithm   = "ed25519"
  , offChainVoteAuthorPublicKey          = "pk"
  , offChainVoteAuthorSignature          = "sig"
  , offChainVoteAuthorWarning            = Nothing
  }

sampleFetchError :: OffChainVoteFetchError
sampleFetchError = OffChainVoteFetchError
  { offChainVoteFetchErrorVotingAnchorId = VotingAnchorId 7
  , offChainVoteFetchErrorFetchError     = "http: connection refused"
  , offChainVoteFetchErrorFetchTime      = sampleTime
  , offChainVoteFetchErrorRetryCount     = 5
  }

sampleTime :: UTCTime
sampleTime = UTCTime
  (fromGregorian 2024 6 15)
  (secondsToDiffTime (12 * 3600 + 34 * 60 + 56))
