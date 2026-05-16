{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for 'DbSync.Phase.Ref'.
module DbSync.Phase.RefSpec (spec) where

import Cardano.Prelude

import Data.IORef (modifyIORef', newIORef, readIORef)
import Control.Tracer (Tracer (..))

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Phase (SyncPhase (..))
import DbSync.Phase.Ref
  ( newSyncPhaseRef
  , readSyncPhase
  , setSyncPhase
  )
import DbSync.Trace.Types (AppTracer, LogMsg (..))

-- | A tracer that records every 'LogMsg' it receives. Used to
-- assert that 'setSyncPhase' logs only on real transitions.
capturingTracer :: IO (AppTracer, IO [LogMsg])
capturingTracer = do
  ref <- newIORef ([] :: [LogMsg])
  let tracer = Tracer (\msg -> modifyIORef' ref (msg :))
      readAll = reverse <$> readIORef ref
  pure (tracer, readAll)

spec :: Spec
spec = describe "DbSync.Phase.Ref" $ do
  describe "newSyncPhaseRef / readSyncPhase" $
    it "returns the seeded phase" $ do
      ref <- newSyncPhaseRef IngestChainHistory
      readSyncPhase ref `shouldReturnP` IngestChainHistory

  describe "setSyncPhase" $ do
    it "updates the held phase" $ do
      (tracer, _) <- capturingTracer
      ref <- newSyncPhaseRef IngestChainHistory
      setSyncPhase tracer ref FollowingVolatileTail
      readSyncPhase ref `shouldReturnP` FollowingVolatileTail

    it "emits one log line on a real transition" $ do
      (tracer, readLogs) <- capturingTracer
      ref <- newSyncPhaseRef IngestChainHistory
      setSyncPhase tracer ref PreparingForVolatileTail
      logs <- readLogs
      map lmMessage logs `shouldBe`
        ["phase IngestChainHistory -> PreparingForVolatileTail"]

    it "is a no-op when the value already matches" $ do
      (tracer, readLogs) <- capturingTracer
      ref <- newSyncPhaseRef FollowingChainTip
      setSyncPhase tracer ref FollowingChainTip
      readSyncPhase ref `shouldReturnP` FollowingChainTip
      logs <- readLogs
      map lmMessage logs `shouldBe` []

    it "logs each transition through a full lifecycle" $ do
      (tracer, readLogs) <- capturingTracer
      ref <- newSyncPhaseRef IngestChainHistory
      setSyncPhase tracer ref PreparingForVolatileTail
      setSyncPhase tracer ref FollowingVolatileTail
      setSyncPhase tracer ref FollowingChainTip
      -- A rollback in steady-state drops us back to volatile-tail.
      setSyncPhase tracer ref FollowingVolatileTail
      setSyncPhase tracer ref FollowingChainTip
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
