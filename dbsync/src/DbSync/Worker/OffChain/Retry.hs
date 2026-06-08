{-# LANGUAGE NumericUnderscores #-}

-- | Exponential-backoff retry schedule for off-chain fetches.
module DbSync.Worker.OffChain.Retry
  ( Retry (..)
  , newRetry
  , retryAgain
  ) where

import Cardano.Prelude

import Data.Time.Clock.POSIX (POSIXTime)

-- | Next-attempt schedule for a single off-chain fetch ref.
data Retry = Retry
  { retryFetchTime :: !POSIXTime
    -- ^ The (UTC, POSIX) time of the most recent attempt — or
    -- \"now\" for a never-attempted ref.
  , retryRetryTime :: !POSIXTime
    -- ^ The earliest time the next attempt may run.
  , retryCount     :: !Word64
    -- ^ Number of failed attempts so far.
  }
  deriving stock (Eq, Show)

-- | Schedule a first attempt for a ref that has never been tried.
newRetry :: POSIXTime -> Retry
newRetry now = Retry
  { retryFetchTime = now
  , retryRetryTime = now
  , retryCount     = 0
  }

-- | Schedule the next attempt after a failure.
--
-- Backoff grows quickly for the first few retries and caps at one
-- day. The 5th retry onward stays at the 24-hour cap.
retryAgain :: POSIXTime -> Word64 -> Retry
retryAgain fetchTime prevCount = Retry
  { retryFetchTime = fetchTime
  , retryRetryTime = fetchTime + backoff
  , retryCount     = next
  }
  where
    next = prevCount + 1
    dayInSeconds = 24 * 60 * 60
    backoff
      | next >= 5 = dayInSeconds
      | otherwise = min dayInSeconds (30 + (2 ^ next) * 60)
