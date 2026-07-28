-- | Unit tests for the network-name derivation used by the boot-time
-- network gate and the @dbsync_sync_state.network_name@ column.
module DbSync.ChainSync.ConnectionSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldBe)

import Ouroboros.Network.Magic (NetworkMagic (..))

import DbSync.ChainSync.Connection (networkNameFromMagic)

spec :: Spec
spec = describe "DbSync.ChainSync.Connection" $
  describe "networkNameFromMagic" $ do
    it "names the well-known networks" $ do
      networkNameFromMagic (NetworkMagic 764824073) `shouldBe` "mainnet"
      networkNameFromMagic (NetworkMagic 1)         `shouldBe` "preprod"
      networkNameFromMagic (NetworkMagic 2)         `shouldBe` "preview"
      networkNameFromMagic (NetworkMagic 4)         `shouldBe` "sanchonet"

    it "falls back to magic-<N> for anything else" $ do
      networkNameFromMagic (NetworkMagic 42) `shouldBe` "magic-42"
      networkNameFromMagic (NetworkMagic 0)  `shouldBe` "magic-0"
