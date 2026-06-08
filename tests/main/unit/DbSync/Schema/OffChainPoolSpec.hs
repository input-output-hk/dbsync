{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @off_chain_pool_data@ and @off_chain_pool_fetch_error@
-- table schemas and COPY encoders.
--
-- Pure: no PostgreSQL, no chain. Verifies golden column order,
-- identity-leaf flags, the JSONB column, unique constraints, and the
-- field-to-column alignment of the COPY encoder.
module DbSync.Schema.OffChainPoolSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Ids (PoolHashId (..), PoolMetadataRefId (..))
import DbSync.Db.Schema.OffChainPool
  ( OffChainPoolData (..)
  , OffChainPoolFetchError (..)
  , encodeOffChainPoolDataCopy
  , encodeOffChainPoolFetchErrorCopy
  , offChainPoolDataTableDef
  , offChainPoolFetchErrorTableDef
  )
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )

spec :: Spec
spec = do
  describe "offChainPoolDataTableDef" $ do
    it "is named off_chain_pool_data" $
      tdName offChainPoolDataTableDef `shouldBe` "off_chain_pool_data"

    it "is UNLOGGED during ingest" $
      tdMode offChainPoolDataTableDef `shouldBe` TableUnlogged

    it "lists columns in golden order" $
      map cdName (tdColumns offChainPoolDataTableDef) `shouldBe`
        [ "id"
        , "pool_id"
        , "ticker_name"
        , "hash"
        , "json"
        , "bytes"
        , "pmr_id"
        ]

    it "marks json as JSONB" $
      cdType (tdColumns offChainPoolDataTableDef !! 4) `shouldBe` PgJsonb

    it "is an identity leaf" $
      tdIdentityColumns offChainPoolDataTableDef `shouldBe` ["id"]

    it "is unique on (pool_id, pmr_id)" $
      map toList (tdUniqueConstraints offChainPoolDataTableDef)
        `shouldBe` [["pool_id", "pmr_id"]]

    it "marks every column NOT NULL" $
      all (not . cdNullable) (tdColumns offChainPoolDataTableDef) `shouldBe` True

  describe "offChainPoolFetchErrorTableDef" $ do
    it "is named off_chain_pool_fetch_error" $
      tdName offChainPoolFetchErrorTableDef `shouldBe` "off_chain_pool_fetch_error"

    it "lists columns in golden order" $
      map cdName (tdColumns offChainPoolFetchErrorTableDef) `shouldBe`
        [ "id"
        , "pool_id"
        , "fetch_time"
        , "pmr_id"
        , "fetch_error"
        , "retry_count"
        ]

    it "uses TIMESTAMP for fetch_time" $
      cdType (tdColumns offChainPoolFetchErrorTableDef !! 2) `shouldBe` PgTimestamp

    it "is an identity leaf" $
      tdIdentityColumns offChainPoolFetchErrorTableDef `shouldBe` ["id"]

    it "is unique on (pool_id, fetch_time, retry_count)" $
      map toList (tdUniqueConstraints offChainPoolFetchErrorTableDef)
        `shouldBe` [["pool_id", "fetch_time", "retry_count"]]

  describe "encodeOffChainPoolDataCopy" $ do
    it "produces a row terminated with newline" $ do
      let row = encodeOffChainPoolDataCopy samplePoolData
      BS8.last row `shouldBe` '\n'

    it "separates every non-id column with a tab" $ do
      let row = encodeOffChainPoolDataCopy samplePoolData
          tabCount = BS.count (fromIntegral (fromEnum '\t')) row
          nonIdCols = length (tdColumns offChainPoolDataTableDef)
                        - length (tdIdentityColumns offChainPoolDataTableDef)
      tabCount `shouldBe` nonIdCols - 1

    it "writes the json column verbatim (no JSONB-side escaping)" $ do
      let row = encodeOffChainPoolDataCopy samplePoolData
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "{\"name\":\"Sample\"}"

  describe "encodeOffChainPoolFetchErrorCopy" $ do
    it "produces a row terminated with newline" $ do
      let row = encodeOffChainPoolFetchErrorCopy sampleFetchError
      BS8.last row `shouldBe` '\n'

    it "encodes fetch_time as YYYY-MM-DD HH:MM:SS" $ do
      let row = encodeOffChainPoolFetchErrorCopy sampleFetchError
          fields = BS8.split '\t' (BS8.init row)
      fields !! 1 `shouldBe` "2024-06-15 12:34:56"

    it "encodes retry_count as decimal" $ do
      let row = encodeOffChainPoolFetchErrorCopy sampleFetchError
          fields = BS8.split '\t' (BS8.init row)
      fields !! 4 `shouldBe` "3"

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

samplePoolData :: OffChainPoolData
samplePoolData = OffChainPoolData
  { offChainPoolDataPoolId     = PoolHashId 42
  , offChainPoolDataTickerName = "SAMPL"
  , offChainPoolDataHash       = "\x01\x02\x03\x04"
  , offChainPoolDataJson       = "{\"name\":\"Sample\"}"
  , offChainPoolDataBytes      = "\x01\x02\x03\x04"
  , offChainPoolDataPmrId      = PoolMetadataRefId 7
  }

sampleFetchError :: OffChainPoolFetchError
sampleFetchError = OffChainPoolFetchError
  { offChainPoolFetchErrorPoolId     = PoolHashId 42
  , offChainPoolFetchErrorFetchTime  = sampleTime
  , offChainPoolFetchErrorPmrId      = PoolMetadataRefId 7
  , offChainPoolFetchErrorFetchError = "http: connection refused"
  , offChainPoolFetchErrorRetryCount = 3
  }

sampleTime :: UTCTime
sampleTime = UTCTime
  (fromGregorian 2024 6 15)
  (secondsToDiffTime (12 * 3600 + 34 * 60 + 56))
