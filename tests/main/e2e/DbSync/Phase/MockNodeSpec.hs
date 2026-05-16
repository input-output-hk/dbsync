{-# LANGUAGE OverloadedStrings #-}

-- | Smoke tests for 'DbSync.Test.MockNode'.
--
-- Boots the harness, forges a handful of blocks into the
-- ChainSync server, and asserts the chain advances. Doesn't yet
-- drive the dbsync app — that's the IngestPrepFollowSpec, which
-- builds on this harness.
module DbSync.Phase.MockNodeSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Ouroboros.Network.Block as Network
import System.Directory (doesFileExist)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Test.MockNode
  ( MockNode (..)
  , currentChainLength
  , currentTip
  , forgeAndPushBlocks
  , withMockNode
  )

spec :: Spec
spec = describe "DbSync.Test.MockNode" $ do

  it "binds a Unix socket under /tmp and tears it down on exit" $ do
    sockPath <- withMockNode conwayConfigDir $ \mn -> do
      -- The server forks asynchronously; bind() may race past us if
      -- we read straight away. Poll up to 1s.
      waitForFile (mnSocketPath mn) 100
      pure (mnSocketPath mn)
    -- Bracket cleanup removed the file.
    sockExistsAfter <- doesFileExist sockPath
    sockExistsAfter `shouldBe` False

  it "starts at chain length 0 (genesis only)" $
    withMockNode conwayConfigDir $ \mn -> do
      n <- currentChainLength mn
      n `shouldBe` 0

  it "advances the server chain when blocks are forged and pushed" $
    withMockNode conwayConfigDir $ \mn -> do
      _ <- forgeAndPushBlocks mn 5
      n <- currentChainLength mn
      n `shouldBe` 5

  it "publishes a tip with the right BlockNo after 3 forged blocks" $
    withMockNode conwayConfigDir $ \mn -> do
      _ <- forgeAndPushBlocks mn 3
      tip <- currentTip mn
      case tip of
        Network.TipGenesis        -> panic "tip still at genesis"
        Network.Tip slot _hash bn -> do
          bn `shouldBe` BlockNo 3
          slot `shouldSatisfy` (>= SlotNo 1)

-- | Path to the vendored Conway test fixtures.
conwayConfigDir :: FilePath
conwayConfigDir = "data/config-conway"

-- | Poll for @path@ to exist, sleeping 10ms between attempts up to
-- @maxAttempts@. Fails the spec if it never appears.
waitForFile :: FilePath -> Int -> IO ()
waitForFile path maxAttempts = go (max 1 maxAttempts)
  where
    go 0 = panic $ "waitForFile: " <> toS path <> " never appeared"
    go n = do
      exists <- doesFileExist path
      unless exists $ do
        threadDelay 10_000
        go (n - 1)
