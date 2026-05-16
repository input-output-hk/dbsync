{-# LANGUAGE OverloadedStrings #-}

-- | Multi-block / multi-epoch scenarios using the chain-gen harness
-- ('DbSync.Test.MockChain').
--
-- Where 'DbSync.Phase.FollowingSpec' uses hand-crafted
-- 'GenericBlock' fixtures (one or two blocks per scenario), this
-- spec drives a real forging 'Interpreter' so we can assert against
-- the database after dozens of blocks across genuine epoch
-- boundaries.
--
-- The first scenario is intentionally a smoke test: if
-- 'withMockChain' boots without crashing then the genesis files
-- load, the leader credentials parse, the 'ProtocolInfo' resolves,
-- the 'BlockForging' actions allocate, and the interpreter is ready
-- to forge. Anything else we want to test layers on top of that.
module DbSync.Phase.MockChainSpec (spec) where

import Cardano.Prelude

import qualified Cardano.Slotting.Slot as Slot

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Cardano.Mock.Forging.Interpreter as Mock

import DbSync.Test.MockChain
  ( MockChain (..)
  , currentEpochNo
  , forgeNextBlocks
  , withMockChain
  )

-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "DbSync.Test.MockChain" $ do

  describe "withMockChain (Conway test config)" $ do

    it "boots an interpreter from the vendored Conway fixtures" $
      withMockChain conwayConfigDir $ \mc -> do
        epoch <- currentEpochNo mc
        epoch `shouldBe` Slot.EpochNo 0

    it "starts at slot 0, block 1 (Byron-shape genesis)" $
      withMockChain conwayConfigDir $ \mc -> do
        slot  <- Mock.getCurrentSlot (mcInterpreter mc)
        blkNo <- Mock.getNextBlockNo (mcInterpreter mc)
        slot  `shouldBe` Slot.SlotNo 0
        blkNo `shouldBe` 1

  describe "forgeNextBlocks" $

    it "forges 3 empty blocks and reports the next BlockNo as 4" $
      withMockChain conwayConfigDir $ \mc -> do
        blks <- forgeNextBlocks mc 3
        length blks `shouldBe` 3
        nextBlkNo <- Mock.getNextBlockNo (mcInterpreter mc)
        nextBlkNo `shouldBe` 4

-- | Path to the vendored Conway test fixtures (genesis files,
-- @test-config.json@, and @pools/bulk1.creds@). Lives under
-- @tests/data/config-conway/@.
conwayConfigDir :: FilePath
conwayConfigDir = "data/config-conway"
