{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the 'shouldFlipToTip' predicate that drives the
-- 'FollowingVolatileTail' -> 'FollowingChainTip' transition.
module DbSync.Phase.Following.FlipPredicateSpec (spec) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Phase.Following.Run (shouldFlipToTip)
import DbSync.Phase.Type (SyncPhase (..))

spec :: Spec
spec = describe "shouldFlipToTip" $ do
  it "flips when applied has caught the chain tip exactly" $
    shouldFlipToTip False FollowingVolatileTail (tipAt tipBlock) (BlockNo tipBlock)
      `shouldBe` True

  -- Steady-state at mainnet chain tip: the consumer's per-block
  -- work runs at roughly the same cadence as block production, so
  -- the receiver always has the next block staged when the flip
  -- check runs.
  it "flips when applied is one block behind the chain tip" $
    shouldFlipToTip False FollowingVolatileTail (tipAt tipBlock) (BlockNo (tipBlock - 1))
      `shouldBe` True

  it "stays in volatile-tail when applied is two blocks behind the chain tip" $
    shouldFlipToTip False FollowingVolatileTail (tipAt tipBlock) (BlockNo (tipBlock - 2))
      `shouldBe` False

  it "flips when applied has advanced past the published tip" $
    shouldFlipToTip False FollowingVolatileTail (tipAt tipBlock) (BlockNo (tipBlock + 1))
      `shouldBe` True

  it "stays in volatile-tail while the replay window is active" $
    shouldFlipToTip True FollowingVolatileTail (tipAt tipBlock) (BlockNo tipBlock)
      `shouldBe` False

  it "stays in volatile-tail when no server tip has been observed" $
    shouldFlipToTip False FollowingVolatileTail Nothing (BlockNo tipBlock)
      `shouldBe` False

  it "is a no-op when the phase is already FollowingChainTip" $
    shouldFlipToTip False FollowingChainTip (tipAt tipBlock) (BlockNo tipBlock)
      `shouldBe` False

  it "is a no-op when the phase is IngestChainHistory" $
    shouldFlipToTip False IngestChainHistory (tipAt tipBlock) (BlockNo tipBlock)
      `shouldBe` False

  it "is a no-op when the phase is PreparingForVolatileTail" $
    shouldFlipToTip False PreparingForVolatileTail (tipAt tipBlock) (BlockNo tipBlock)
      `shouldBe` False

-- | Arbitrary block number used for the synthetic chain tip.
tipBlock :: Word64
tipBlock = 100_000

-- | Build a 'Just' server tip at the given block number.
tipAt :: Word64 -> Maybe BlockNo
tipAt = Just . BlockNo
