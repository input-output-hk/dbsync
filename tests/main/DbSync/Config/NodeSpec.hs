-- | Tests for node configuration parsing.
--
-- Two-stage parsing:
--   1. Parse db-sync-config.json → extract NodeConfigFile path
--   2. Parse the referenced config.json → NodeConfig with genesis paths, hashes, triggers
module DbSync.Config.NodeSpec
  ( spec
  ) where

import Cardano.Prelude

import DbSync.Config.Node (parseDbSyncNodeConfig, parseNodeConfig)
import DbSync.Config.Types
  ( DbSyncNodeConfig (..)
  , NetworkMagicConfig (..)
  , NodeConfig (..)
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "DbSync.Config.Node" $ do
  describe "parseDbSyncNodeConfig" $ do
    it "extracts NodeConfigFile from db-sync-config.json" $ do
      result <- parseDbSyncNodeConfig "fixtures/db-sync-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right dsc ->
          dscNodeConfigFile dsc `shouldBe` "config.json"

    it "extracts optional NetworkName" $ do
      result <- parseDbSyncNodeConfig "fixtures/db-sync-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right dsc ->
          dscNetworkName dsc `shouldBe` Just "mainnet"

    it "parses the real db-sync-config.json" $ do
      result <- parseDbSyncNodeConfig "/Volumes/Cmdv4TB/Code/IOG/testnet/db-sync-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right dsc ->
          dscNodeConfigFile dsc `shouldBe` "config.json"

  describe "parseNodeConfig (mainnet-style)" $ do
    it "extracts genesis file paths" $ do
      result <- parseNodeConfig "fixtures/node-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc -> do
          ncByronGenesisFile nc `shouldBe` "byron-genesis.json"
          ncShelleyGenesisFile nc `shouldBe` "shelley-genesis.json"
          ncAlonzoGenesisFile nc `shouldBe` "alonzo-genesis.json"
          ncConwayGenesisFile nc `shouldBe` "conway-genesis.json"

    it "extracts genesis hashes" $ do
      result <- parseNodeConfig "fixtures/node-config.json"
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
      result <- parseNodeConfig "fixtures/node-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc ->
          ncRequiresNetworkMagic nc `shouldBe` RequiresNoMagic

    it "defaults hard fork triggers to Nothing on mainnet" $ do
      result <- parseNodeConfig "fixtures/node-config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc ->
          ncTestShelleyHardForkAtEpoch nc `shouldBe` Nothing

  describe "parseNodeConfig (testnet-style)" $ do
    it "extracts RequiresMagic for testnets" $ do
      result <- parseNodeConfig "fixtures/node-config-testnet.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc ->
          ncRequiresNetworkMagic nc `shouldBe` RequiresMagic

    it "handles optional ConwayGenesisHash" $ do
      result <- parseNodeConfig "fixtures/node-config-testnet.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc ->
          ncConwayGenesisHash nc `shouldBe` Nothing

    it "parses testnet hard fork trigger epochs" $ do
      result <- parseNodeConfig "fixtures/node-config-testnet.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc -> do
          ncTestShelleyHardForkAtEpoch nc `shouldBe` Just 1
          ncTestAlonzoHardForkAtEpoch nc `shouldBe` Just 4
          ncTestConwayHardForkAtEpoch nc `shouldBe` Just 6

  describe "parseNodeConfig (real production config)" $ do
    it "parses the actual testnet node config" $ do
      result <- parseNodeConfig "/Volumes/Cmdv4TB/Code/IOG/testnet/config.json"
      case result of
        Left err -> panic $ "Parse failed: " <> show err
        Right nc -> do
          ncByronGenesisFile nc `shouldBe` "byron-genesis.json"
          ncRequiresNetworkMagic nc `shouldBe` RequiresNoMagic
