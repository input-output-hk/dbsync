-- | Tests for cardano-node config parsing.
--
-- We only extract genesis paths, hashes, and network magic from the
-- node config. All the logging/tracing/P2P keys are ignored.
module DbSync.Config.NodeSpec
  ( spec
  ) where

import Cardano.Prelude

import DbSync.Config.Node (parseNodeConfig)
import DbSync.Config.Types
  ( NetworkMagicConfig (..)
  , NodeConfig (..)
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "DbSync.Config.Node" $ do
  describe "parseNodeConfig (mainnet-style)" $ do
    it "extracts genesis file paths" $ do
      result <- parseNodeConfig "test-fixtures/node-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc -> do
          ncByronGenesisFile nc `shouldBe` "byron-genesis.json"
          ncShelleyGenesisFile nc `shouldBe` "shelley-genesis.json"
          ncAlonzoGenesisFile nc `shouldBe` "alonzo-genesis.json"
          ncConwayGenesisFile nc `shouldBe` "conway-genesis.json"

    it "extracts genesis hashes" $ do
      result <- parseNodeConfig "test-fixtures/node-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc -> do
          ncByronGenesisHash nc `shouldBe`
            "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
          ncShelleyGenesisHash nc `shouldBe`
            "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81"
          ncConwayGenesisHash nc `shouldBe`
            Just "15a199f895e461ec0ffc6dd4e4028af28a492ab4e806d39cb674c88f7643ef62"

    it "extracts RequiresNoMagic for mainnet" $ do
      result <- parseNodeConfig "test-fixtures/node-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc ->
          ncRequiresNetworkMagic nc `shouldBe` RequiresNoMagic

  describe "parseNodeConfig (testnet-style)" $ do
    it "extracts RequiresMagic for testnets" $ do
      result <- parseNodeConfig "test-fixtures/node-config-testnet.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc ->
          ncRequiresNetworkMagic nc `shouldBe` RequiresMagic

    it "handles optional ConwayGenesisHash" $ do
      result <- parseNodeConfig "test-fixtures/node-config-testnet.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc ->
          ncConwayGenesisHash nc `shouldBe` Nothing

    it "ignores test hard fork epoch keys without failing" $ do
      -- The node config has TestShelleyHardForkAtEpoch etc.
      -- We don't parse them — just make sure they don't cause failure
      result <- parseNodeConfig "test-fixtures/node-config-testnet.json"
      result `shouldSatisfy` isRight

  describe "parseNodeConfig (real production config)" $ do
    it "parses the actual testnet node config" $ do
      -- Test against the real config from the cookbook
      result <- parseNodeConfig "/Volumes/Cmdv4TB/Code/IOG/testnet/config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc -> do
          ncByronGenesisFile nc `shouldBe` "byron-genesis.json"
          ncRequiresNetworkMagic nc `shouldBe` RequiresNoMagic
