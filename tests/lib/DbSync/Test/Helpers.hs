{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared, dependency-free test utilities.
--
-- Lives below 'DbSync.Test.AppHarness' and 'DbSync.Test.PgAssertions'
-- so both can use these helpers without creating a dependency edge
-- between them.
module DbSync.Test.Helpers
  ( waitFor
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- | Poll @predicate@ every 200 ms up to @timeoutSecs@. Panics with a
-- descriptive message if it never returns 'True'. The @label@ is the
-- "what" being waited on and is interpolated into the timeout
-- message — pick something readable like
-- @"sync_complete=true"@ or @"post-Prep schema state to settle"@.
waitFor :: Text -> IO Bool -> Int -> IO ()
waitFor label predicate timeoutSecs = do
  start <- getCurrentTime
  go start
  where
    go startedAt = do
      ok <- predicate
      if ok
        then pure ()
        else do
          now <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime now startedAt) :: Double
          if elapsed >= fromIntegral timeoutSecs
            then panic $
              "waitFor " <> label <> ": timed out after "
                <> T.pack (show timeoutSecs) <> "s"
            else do
              threadDelay 200_000
              go startedAt
