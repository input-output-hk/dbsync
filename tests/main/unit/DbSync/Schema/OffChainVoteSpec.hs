{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @off_chain_vote_*@ table schemas and COPY encoders.
--
-- Pure: no PostgreSQL, no chain. Verifies golden column order,
-- identity-leaf flags, the JSONB column, unique constraints, and the
-- field-to-column alignment of the COPY encoders.
module DbSync.Schema.OffChainVoteSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.ByteString as BS
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
  , OffChainVoteDrepData (..)
  , OffChainVoteExternalUpdate (..)
  , OffChainVoteFetchError (..)
  , OffChainVoteGovActionData (..)
  , OffChainVoteReference (..)
  , encodeOffChainVoteAuthorCopy
  , encodeOffChainVoteDataCopy
  , encodeOffChainVoteDrepDataCopy
  , encodeOffChainVoteExternalUpdateCopy
  , encodeOffChainVoteFetchErrorCopy
  , encodeOffChainVoteGovActionDataCopy
  , encodeOffChainVoteReferenceCopy
  , offChainVoteAuthorTableDef
  , offChainVoteDataTableDef
  , offChainVoteDrepDataTableDef
  , offChainVoteExternalUpdateTableDef
  , offChainVoteFetchErrorTableDef
  , offChainVoteGovActionDataTableDef
  , offChainVoteReferenceTableDef
  )
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )

spec :: Spec
spec = do
  describe "offChainVoteDataTableDef" $ do
    it "is named off_chain_vote_data" $
      tdName offChainVoteDataTableDef `shouldBe` "off_chain_vote_data"

    it "is UNLOGGED during ingest" $
      tdMode offChainVoteDataTableDef `shouldBe` TableUnlogged

    it "lists columns in golden order" $
      map cdName (tdColumns offChainVoteDataTableDef) `shouldBe`
        [ "id"
        , "voting_anchor_id"
        , "hash"
        , "json"
        , "bytes"
        , "warning"
        , "language"
        , "comment"
        , "is_valid"
        ]

    it "marks json as JSONB" $
      cdType (tdColumns offChainVoteDataTableDef !! 3) `shouldBe` PgJsonb

    it "marks is_valid as BOOLEAN" $
      cdType (tdColumns offChainVoteDataTableDef !! 8) `shouldBe` PgBoolean

    it "is an identity leaf" $
      tdIdentityColumns offChainVoteDataTableDef `shouldBe` ["id"]

    it "is unique on (hash, voting_anchor_id)" $
      map toList (tdUniqueConstraints offChainVoteDataTableDef)
        `shouldBe` [["hash", "voting_anchor_id"]]

    it "allows NULL on warning, comment, is_valid" $ do
      let nullableNames =
            [ cdName c
            | c <- tdColumns offChainVoteDataTableDef
            , cdNullable c
            ]
      nullableNames `shouldBe` ["warning", "comment", "is_valid"]

  describe "offChainVoteGovActionDataTableDef" $ do
    it "is named off_chain_vote_gov_action_data" $
      tdName offChainVoteGovActionDataTableDef
        `shouldBe` "off_chain_vote_gov_action_data"

    it "is an identity leaf" $
      tdIdentityColumns offChainVoteGovActionDataTableDef `shouldBe` ["id"]

    it "lists columns in golden order" $
      map cdName (tdColumns offChainVoteGovActionDataTableDef) `shouldBe`
        [ "id"
        , "off_chain_vote_data_id"
        , "title"
        , "abstract"
        , "motivation"
        , "rationale"
        ]

  describe "offChainVoteDrepDataTableDef" $ do
    it "is named off_chain_vote_drep_data" $
      tdName offChainVoteDrepDataTableDef `shouldBe` "off_chain_vote_drep_data"

    it "marks the optional fields nullable" $ do
      let nullableNames =
            [ cdName c
            | c <- tdColumns offChainVoteDrepDataTableDef
            , cdNullable c
            ]
      nullableNames `shouldBe`
        [ "payment_address"
        , "objectives"
        , "motivations"
        , "qualifications"
        , "image_url"
        , "image_hash"
        ]

  describe "offChainVoteAuthorTableDef" $ do
    it "is named off_chain_vote_author" $
      tdName offChainVoteAuthorTableDef `shouldBe` "off_chain_vote_author"

    it "lists columns in golden order" $
      map cdName (tdColumns offChainVoteAuthorTableDef) `shouldBe`
        [ "id"
        , "off_chain_vote_data_id"
        , "name"
        , "witness_algorithm"
        , "public_key"
        , "signature"
        , "warning"
        ]

  describe "offChainVoteReferenceTableDef" $ do
    it "is named off_chain_vote_reference" $
      tdName offChainVoteReferenceTableDef `shouldBe` "off_chain_vote_reference"

  describe "offChainVoteExternalUpdateTableDef" $ do
    it "is named off_chain_vote_external_update" $
      tdName offChainVoteExternalUpdateTableDef
        `shouldBe` "off_chain_vote_external_update"

  describe "offChainVoteFetchErrorTableDef" $ do
    it "is named off_chain_vote_fetch_error" $
      tdName offChainVoteFetchErrorTableDef
        `shouldBe` "off_chain_vote_fetch_error"

    it "uses TIMESTAMP for fetch_time" $
      cdType (tdColumns offChainVoteFetchErrorTableDef !! 3) `shouldBe` PgTimestamp

    it "is unique on (voting_anchor_id, retry_count)" $
      map toList (tdUniqueConstraints offChainVoteFetchErrorTableDef)
        `shouldBe` [["voting_anchor_id", "retry_count"]]

  describe "encodeOffChainVoteDataCopy" $ do
    it "produces a row terminated with newline" $
      BS8.last (encodeOffChainVoteDataCopy sampleVoteData) `shouldBe` '\n'

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

  describe "encodeOffChainVoteGovActionDataCopy" $ do
    it "produces a row terminated with newline" $
      BS8.last (encodeOffChainVoteGovActionDataCopy sampleGov) `shouldBe` '\n'

    it "separates every non-id column with a tab" $ do
      let row = encodeOffChainVoteGovActionDataCopy sampleGov
          tabCount = BS.count (fromIntegral (fromEnum '\t')) row
          nonIdCols = length (tdColumns offChainVoteGovActionDataTableDef)
                        - length (tdIdentityColumns offChainVoteGovActionDataTableDef)
      tabCount `shouldBe` nonIdCols - 1

  describe "encodeOffChainVoteAuthorCopy" $ do
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

  describe "encodeOffChainVoteDrepDataCopy" $
    it "produces a row terminated with newline" $
      BS8.last (encodeOffChainVoteDrepDataCopy sampleDrep) `shouldBe` '\n'

  describe "encodeOffChainVoteReferenceCopy" $
    it "produces a row terminated with newline" $
      BS8.last (encodeOffChainVoteReferenceCopy sampleRef) `shouldBe` '\n'

  describe "encodeOffChainVoteExternalUpdateCopy" $
    it "produces a row terminated with newline" $
      BS8.last (encodeOffChainVoteExternalUpdateCopy sampleExt) `shouldBe` '\n'

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

sampleGov :: OffChainVoteGovActionData
sampleGov = OffChainVoteGovActionData
  { offChainVoteGovActionDataOffChainVoteDataId = OffChainVoteDataId 11
  , offChainVoteGovActionDataTitle              = "Title"
  , offChainVoteGovActionDataAbstract           = "Abstract"
  , offChainVoteGovActionDataMotivation         = "Motivation"
  , offChainVoteGovActionDataRationale          = "Rationale"
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

sampleDrep :: OffChainVoteDrepData
sampleDrep = OffChainVoteDrepData
  { offChainVoteDrepDataOffChainVoteDataId = OffChainVoteDataId 11
  , offChainVoteDrepDataPaymentAddress     = Nothing
  , offChainVoteDrepDataGivenName          = "Alice"
  , offChainVoteDrepDataObjectives         = Nothing
  , offChainVoteDrepDataMotivations        = Nothing
  , offChainVoteDrepDataQualifications     = Nothing
  , offChainVoteDrepDataImageUrl           = Nothing
  , offChainVoteDrepDataImageHash          = Nothing
  }

sampleRef :: OffChainVoteReference
sampleRef = OffChainVoteReference
  { offChainVoteReferenceOffChainVoteDataId = OffChainVoteDataId 11
  , offChainVoteReferenceLabel              = "spec"
  , offChainVoteReferenceUri                = "https://example.test/spec"
  , offChainVoteReferenceHashDigest         = Nothing
  , offChainVoteReferenceHashAlgorithm      = Nothing
  }

sampleExt :: OffChainVoteExternalUpdate
sampleExt = OffChainVoteExternalUpdate
  { offChainVoteExternalUpdateOffChainVoteDataId = OffChainVoteDataId 11
  , offChainVoteExternalUpdateTitle              = "Update"
  , offChainVoteExternalUpdateUri                = "https://example.test/update"
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
