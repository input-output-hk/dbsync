{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for 'DbSync.Phase.Current'.
module DbSync.Phase.CurrentSpec (spec) where

import Cardano.Prelude

import Data.IORef (modifyIORef', newIORef, readIORef)
import Control.Tracer (Tracer (..))

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.AppM (runAppM)
import DbSync.Db.Phase (SyncPhase (..))
import DbSync.Phase.Current
  ( newCurrentPhase
  , readCurrentPhase
  , setCurrentPhase
  )
import DbSync.Trace.Types (AppTracer, LogMsg (..))

-- | A tracer that records every 'LogMsg' it receives. Used to
-- assert that 'setCurrentPhase' logs only on real transitions.
capturingTracer :: IO (AppTracer, IO [LogMsg])
capturingTracer = do
  ref <- newIORef ([] :: [LogMsg])
  let tracer = Tracer (\msg -> modifyIORef' ref (msg :))
      readAll = reverse <$> readIORef ref
  pure (tracer, readAll)

spec :: Spec
spec = describe "DbSync.Phase.Current" $ do
  describe "newCurrentPhase / readCurrentPhase" $
    it "returns the seeded phase" $ do
      ref <- newCurrentPhase IngestChainHistory
      readCurrentPhase ref `shouldReturnP` IngestChainHistory

  describe "setCurrentPhase" $ do
    it "updates the held phase" $ do
      (tracer, _) <- capturingTracer
      ref <- newCurrentPhase IngestChainHistory
      runAppM tracer (setCurrentPhase ref FollowingVolatileTail)
      readCurrentPhase ref `shouldReturnP` FollowingVolatileTail

    it "emits one log line on a real transition" $ do
      (tracer, readLogs) <- capturingTracer
      ref <- newCurrentPhase IngestChainHistory
      runAppM tracer (setCurrentPhase ref PreparingForVolatileTail)
      logs <- readLogs
      map lmMessage logs `shouldBe`
        ["phase IngestChainHistory -> PreparingForVolatileTail"]

    it "is a no-op when the value already matches" $ do
      (tracer, readLogs) <- capturingTracer
      ref <- newCurrentPhase FollowingChainTip
      runAppM tracer (setCurrentPhase ref FollowingChainTip)
      readCurrentPhase ref `shouldReturnP` FollowingChainTip
      logs <- readLogs
      map lmMessage logs `shouldBe` []

    it "logs each transition through a full lifecycle" $ do
      (tracer, readLogs) <- capturingTracer
      ref <- newCurrentPhase IngestChainHistory
      runAppM tracer (setCurrentPhase ref PreparingForVolatileTail)
      runAppM tracer (setCurrentPhase ref FollowingVolatileTail)
      runAppM tracer (setCurrentPhase ref FollowingChainTip)
      -- A rollback in steady-state drops us back to volatile-tail.
      runAppM tracer (setCurrentPhase ref FollowingVolatileTail)
      runAppM tracer (setCurrentPhase ref FollowingChainTip)
      logs <- readLogs
      map lmMessage logs `shouldBe`
        [ "phase IngestChainHistory -> PreparingForVolatileTail"
        , "phase PreparingForVolatileTail -> FollowingVolatileTail"
        , "phase FollowingVolatileTail -> FollowingChainTip"
        , "phase FollowingChainTip -> FollowingVolatileTail"
        , "phase FollowingVolatileTail -> FollowingChainTip"
        ]

-- | hspec equivalent of @action `shouldReturn` x@ with a tighter name.
shouldReturnP :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturnP action expected = do
  actual <- action
  actual `shouldBe` expected
