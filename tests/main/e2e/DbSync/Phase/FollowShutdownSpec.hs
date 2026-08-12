{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end coverage for the Follow loop's shutdown path. The
-- signal fires while a forge stream is in flight, so the consumer
-- holds a per-block PG transaction when it is asked to stop.
module DbSync.Phase.FollowShutdownSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Trace.Backend (mkTestTracer)
import DbSync.Trace.Types (AppTracer, LogMsg (..))
import DbSync.Test.AppHarness (ledgerEnabledTestConfig, waitForSyncComplete, withTempDir)
import DbSync.Test.E2E (conwayConfigDir, withAppSession)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode
  ( forgeAndPushBlocks
  , forgeAndPushUntilNextEpoch
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Follow shutdown" $
  it "stops the consumer between blocks while a forge stream is in flight" $
    for_ [1 .. shutdownRounds] $ \_ -> runShutdownRound

-- | Sessions per example. The signal lands wherever the consumer
-- happens to be, so one round covers only one of the parked and
-- mid-block cases.
shutdownRounds :: Int
shutdownRounds = 3

-- | Longest acceptable gap between the shutdown signal and 'runApp'
-- returning. The loop only has to finish its current block, close one
-- connection and unwind.
maxShutdownSeconds :: NominalDiffTime
maxShutdownSeconds = 10

-- ---------------------------------------------------------------------------
-- * One session
-- ---------------------------------------------------------------------------

-- | Ingest a seed chain, cross an epoch boundary at tip, then fire the
-- shutdown signal straight away.
--
-- The boundary is what makes this a shutdown test: crossing it queues
-- a ledger snapshot, so the signal arrives while the snapshot writer
-- still holds the LSM session and its PG connection.
runShutdownRound :: IO ()
runShutdownRound =
  withMockNode conwayConfigDir $ \mn ->
    withTempDir "dbsync-test-follow-shutdown" $ \ledgerDir -> do
      logsRef <- newIORef []
      let tracer = mkTestTracer logsRef :: AppTracer

      -- 150 blocks → past k=10 and one Conway-config epoch boundary,
      -- so Ingest exits cleanly into Prep.
      _ <- forgeAndPushBlocks mn 150

      firedAt <- newIORef =<< getCurrentTime
      (`onException` dumpTail logsRef) $ do
        withAppSession tracer ledgerEnabledTestConfig mn ledgerDir $ \_app -> do
          waitForSyncComplete 90
          before <- countRows (tdName blockTableDef)
          crossed <- length <$> forgeAndPushUntilNextEpoch mn
          waitFor "the epoch-crossing blocks reach the block table"
            (do n <- countRows (tdName blockTableDef)
                pure (n >= before + crossed))
            60
          writeIORef firedAt =<< getCurrentTime

        -- 'withAppSession' fires the signal and waits for 'runApp'
        -- before it returns, so this spans the shutdown.
        elapsed <- diffUTCTime <$> getCurrentTime <*> readIORef firedAt
        elapsed `shouldSatisfy` (< maxShutdownSeconds)

        exits <- countLogsMatching logsRef isConsumerLoopExit
        exits `shouldBe` 1

-- ---------------------------------------------------------------------------
-- * Log predicates
-- ---------------------------------------------------------------------------

-- | The line 'DbSync.Phase.Following.Run.run' emits when the stop flag
-- ends the loop. A cancelled loop never logs it. The component is the
-- current phase, so match the message alone.
isConsumerLoopExit :: LogMsg -> Bool
isConsumerLoopExit =
  T.isInfixOf "shutdown requested; consumer loop exiting" . lmMessage

countLogsMatching :: IORef [LogMsg] -> (LogMsg -> Bool) -> IO Int
countLogsMatching ref p = length . filter p <$> readIORef ref

-- | Print the newest captured lines on failure. The tracer is silent
-- during the run because stderr I\/O at every trace point perturbs the
-- shutdown timing enough to hide the race.
dumpTail :: IORef [LogMsg] -> IO ()
dumpTail ref = do
  msgs <- readIORef ref
  hPutStrLn stderr ("--- last 40 app log lines ---" :: Text)
  for_ (reverse (take 40 msgs)) $ \m ->
    hPutStrLn stderr (lmComponent m <> ": " <> lmMessage m)
