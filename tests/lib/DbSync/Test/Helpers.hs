{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared, dependency-free test utilities.
--
-- Lives below 'DbSync.Test.AppHarness' and 'DbSync.Test.PgAssertions'
-- so both can use these helpers without creating a dependency edge
-- between them.
module DbSync.Test.Helpers
  ( waitFor
  , waitForStable
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

-- | Poll @sample@ every 200 ms until two consecutive readings are
-- equal. Returns that stable reading. Panics with @label@ in the
-- message if it doesn't stabilise within @timeoutSecs@.
--
-- Use when the test needs to wait for an in-flight operation to
-- finish without having a direct "done" signal — e.g., the Ingest
-- consumer crossing an epoch boundary and returning to block
-- processing. A stable sample is the observable proxy for "nothing
-- is changing right now".
waitForStable :: Eq a => Text -> IO a -> Int -> IO a
waitForStable label sample timeoutSecs = do
  start  <- getCurrentTime
  seed   <- sample
  go start seed
  where
    go startedAt prev = do
      threadDelay 200_000
      cur <- sample
      if cur == prev
        then pure cur
        else do
          now <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime now startedAt) :: Double
          if elapsed >= fromIntegral timeoutSecs
            then panic $
              "waitForStable " <> label <> ": never stabilised within "
                <> T.pack (show timeoutSecs) <> "s"
            else go startedAt cur
