{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @ada_pots@ COPY encoder: numeric fields land in the
-- right column index — guards against accidental field reordering
-- between the record, the encoder, and the table definition.
module DbSync.Schema.AdaPotsSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))

import qualified Data.ByteString.Char8 as BS8

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.AdaPots
  ( AdaPots (..)
  , encodeAdaPotsCopy
  )
import DbSync.Db.Schema.Ids (BlockId (..))
import DbSync.Db.Types (DbLovelace (..))

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "encodeAdaPotsCopy" $ do
    it "writes slot_no in field 0 and epoch_no in field 1" $ do
      let row = encodeAdaPotsCopy sampleAdaPots
          fields = BS8.split '\t' (BS8.init row)
      fields !! 0 `shouldBe` "123456"
      fields !! 1 `shouldBe` "500"

    it "writes the eight Lovelace pots in the documented field order" $ do
      let row = encodeAdaPotsCopy sampleAdaPots
          fields = BS8.split '\t' (BS8.init row)
      fields !! 2  `shouldBe` "1000000000000"   -- treasury
      fields !! 3  `shouldBe` "12000000000000"  -- reserves
      fields !! 4  `shouldBe` "5000000"         -- rewards
      fields !! 5  `shouldBe` "30000000000"     -- utxo
      fields !! 6  `shouldBe` "10000000"        -- deposits_stake
      fields !! 7  `shouldBe` "150000"          -- fees
      fields !! 9  `shouldBe` "200000"          -- deposits_drep
      fields !! 10 `shouldBe` "75000"           -- deposits_proposal

    it "writes block_id in field 8" $ do
      let row = encodeAdaPotsCopy sampleAdaPots
          fields = BS8.split '\t' (BS8.init row)
      fields !! 8 `shouldBe` "777"

    it "encodes zero-valued pots as 0 (not NULL)" $ do
      let row = encodeAdaPotsCopy zeroPots
          fields = BS8.split '\t' (BS8.init row)
      forM_ [2, 3, 4, 5, 6, 7, 9, 10] $ \i ->
        fields !! i `shouldBe` "0"

    it "round-trips a maximum-Word64-valued field" $ do
      let maxField = sampleAdaPots
            { adaPotsTreasury = DbLovelace 18446744073709551615
            }
          row = encodeAdaPotsCopy maxField
          fields = BS8.split '\t' (BS8.init row)
      fields !! 2 `shouldBe` "18446744073709551615"

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

-- | Realistic-looking values for an epoch boundary on a Conway-era
-- chain. All values distinguishable so reordering bugs surface
-- in 'fields !! i' assertions.
sampleAdaPots :: AdaPots
sampleAdaPots = AdaPots
  { adaPotsSlotNo            = 123456
  , adaPotsEpochNo           = 500
  , adaPotsTreasury          = DbLovelace  1000000000000
  , adaPotsReserves          = DbLovelace 12000000000000
  , adaPotsRewards           = DbLovelace        5000000
  , adaPotsUtxo              = DbLovelace    30000000000
  , adaPotsDepositsStake     = DbLovelace       10000000
  , adaPotsFees              = DbLovelace         150000
  , adaPotsBlockId           = BlockId 777
  , adaPotsDepositsDrep      = DbLovelace         200000
  , adaPotsDepositsProposal  = DbLovelace          75000
  }

-- | A row with every pot at zero. Used to verify that zeroes are
-- encoded as @0@ (not @\N@).
zeroPots :: AdaPots
zeroPots = AdaPots
  { adaPotsSlotNo            = 0
  , adaPotsEpochNo           = 0
  , adaPotsTreasury          = DbLovelace 0
  , adaPotsReserves          = DbLovelace 0
  , adaPotsRewards           = DbLovelace 0
  , adaPotsUtxo              = DbLovelace 0
  , adaPotsDepositsStake     = DbLovelace 0
  , adaPotsFees              = DbLovelace 0
  , adaPotsBlockId           = BlockId 1
  , adaPotsDepositsDrep      = DbLovelace 0
  , adaPotsDepositsProposal  = DbLovelace 0
  }
