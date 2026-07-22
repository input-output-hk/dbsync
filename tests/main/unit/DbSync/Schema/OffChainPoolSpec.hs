{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @off_chain_pool_data@ and @off_chain_pool_fetch_error@
-- COPY encoders: JSONB passthrough, timestamp format, decimal fields.
module DbSync.Schema.OffChainPoolSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
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
  )

spec :: Spec
spec = do
  describe "encodeOffChainPoolDataCopy" $
    it "writes the json column verbatim (no JSONB-side escaping)" $ do
      let row = encodeOffChainPoolDataCopy samplePoolData
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "{\"name\":\"Sample\"}"

  describe "encodeOffChainPoolFetchErrorCopy" $ do
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
