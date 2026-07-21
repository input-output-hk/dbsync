{-# LANGUAGE OverloadedStrings #-}

-- | @block.size@ regression coverage (upstream cardano-db-sync
-- \#2145): the parser must report the header-declared byte size, so a
-- block carrying txs is measurably larger than an empty one.
module DbSync.Parser.BlockSpec
  ( spec
  ) where

import Cardano.Prelude

import Ouroboros.Consensus.Block (blockSlot)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

import DbSync.Parser.Dispatch (parseBlock)
import DbSync.Parser.Types (GenericBlock (..))
import DbSync.Test.E2E (conwayConfigDir)
import DbSync.Test.MockChain
  ( buildRealisticTxs
  , forgeNextBlock
  , mainnetAverageShape
  , withMockChain
  )
import DbSync.Test.Property.Invariants (syntheticSlotDetails)

spec :: Spec
spec = describe "DbSync.Parser.Block" $
  it "blkSize grows with tx content and is never zero" $
    withMockChain conwayConfigDir $ \mc -> do
      emptyBlk <- forgeNextBlock mc []
      txs      <- buildRealisticTxs mc mainnetAverageShape
      multiBlk <- forgeNextBlock mc txs

      let sizeOf blk =
            blkSize (parseBlock True (syntheticSlotDetails (blockSlot blk)) blk)
          emptySize = sizeOf emptyBlk
          multiSize = sizeOf multiBlk

      emptySize `shouldSatisfy` (> 0)
      -- Ten payment txs add well over 200 bytes; a size sourced from
      -- the wrong header field would collapse this gap.
      multiSize `shouldSatisfy` (>= emptySize + 200)
