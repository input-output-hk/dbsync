-- | Tests for genesis config reading.
--
-- Reads real genesis files from the testnet directory and verifies
-- we can build a TopLevelConfig (which gives us ChainSync codecs).
module DbSync.Config.GenesisSpec
  ( spec
  ) where

import Cardano.Prelude

import DbSync.Config.Genesis (readCardanoGenesisConfig, mkTopLevelConfig)
import DbSync.Config.Node (parseNodeConfig)
import DbSync.Config.Types (NodeConfig (..))
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Directory containing the real mainnet genesis files.
testnetDir :: FilePath
testnetDir = "/Volumes/Cmdv4TB/Code/IOG/testnet"

spec :: Spec
spec = describe "DbSync.Config.Genesis" $ do
  describe "readCardanoGenesisConfig" $ do
    it "reads all four genesis files from testnet directory" $ do
      Right nc <- parseNodeConfig (testnetDir <> "/config.json")
      result <- readCardanoGenesisConfig nc testnetDir
      isRight result `shouldBe` True

  describe "mkTopLevelConfig" $ do
    it "builds a TopLevelConfig from genesis data" $ do
      Right nc <- parseNodeConfig (testnetDir <> "/config.json")
      Right gc <- readCardanoGenesisConfig nc testnetDir
      -- If this evaluates without throwing, we have a valid TopLevelConfig
      let topLevel = mkTopLevelConfig nc gc
      -- Force evaluation to ensure it's not a thunk hiding an error
      void $ evaluate topLevel
