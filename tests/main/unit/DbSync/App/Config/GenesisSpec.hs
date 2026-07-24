-- | Tests for genesis config reading.
--
-- Reads the four era genesis files from the committed Conway test
-- fixture and verifies we can build a TopLevelConfig (which gives us
-- ChainSync codecs).
module DbSync.App.Config.GenesisSpec
  ( spec
  ) where

import Cardano.Prelude

import DbSync.App.Config.Genesis (readCardanoGenesisConfig, mkTopLevelConfig)
import DbSync.App.Config.Node (parseNodeConfig)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | The committed Conway node-config + genesis fixture, resolved
-- relative to the test package directory (where cabal runs the suite).
configDir :: FilePath
configDir = "data/config-conway"

spec :: Spec
spec = describe "DbSync.App.Config.Genesis" $ do
  describe "readCardanoGenesisConfig" $ do
    it "reads all four genesis files from the Conway fixture" $ do
      Right nc <- parseNodeConfig (configDir <> "/test-config.json")
      result <- readCardanoGenesisConfig nc configDir
      isRight result `shouldBe` True

  describe "mkTopLevelConfig" $ do
    it "builds a TopLevelConfig from genesis data" $ do
      Right nc <- parseNodeConfig (configDir <> "/test-config.json")
      Right gc <- readCardanoGenesisConfig nc configDir
      -- If this evaluates without throwing, we have a valid TopLevelConfig
      let topLevel = mkTopLevelConfig nc gc
      -- Force evaluation to ensure it's not a thunk hiding an error
      void $ evaluate topLevel
