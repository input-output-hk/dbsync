-- | Backoff schedule for off-chain fetch retries.
module DbSync.Worker.OffChain.RetrySpec (spec) where

import Cardano.Prelude

import Data.Time.Clock.POSIX (POSIXTime)

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Worker.OffChain.Retry (Retry (..), newRetry, retryAgain)

-- Fixed base instant; the schedule is relative to it.
now :: POSIXTime
now = 1000

spec :: Spec
spec = do
  newRetrySpec
  retryAgainSpec

newRetrySpec :: Spec
newRetrySpec = describe "DbSync.Worker.OffChain.Retry.newRetry" $

  it "schedules the first attempt for now, with a zero failure count" $
    newRetry now `shouldBe` Retry now now 0

retryAgainSpec :: Spec
retryAgainSpec = describe "DbSync.Worker.OffChain.Retry.retryAgain" $ do

  it "backs off 150s after the first failure and counts it" $
    retryAgain now 0 `shouldBe` Retry now (now + 150) 1

  it "grows the backoff geometrically for the early retries" $ do
    retryRetryTime (retryAgain now 1) `shouldBe` now + 270
    retryRetryTime (retryAgain now 2) `shouldBe` now + 510
    retryRetryTime (retryAgain now 3) `shouldBe` now + 990

  it "caps the backoff at one day from the fifth retry onward" $ do
    retryRetryTime (retryAgain now 4) `shouldBe` now + 86400
    retryRetryTime (retryAgain now 5) `shouldBe` now + 86400
    retryRetryTime (retryAgain now 99) `shouldBe` now + 86400

  it "carries the incremented count and the given fetch time" $
    retryAgain 5000 41 `shouldBe` Retry 5000 (5000 + 86400) 42
