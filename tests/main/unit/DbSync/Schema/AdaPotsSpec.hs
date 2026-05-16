{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @ada_pots@ table schema and COPY encoding.
--
-- Pure tests — no PostgreSQL required. Verify that:
--
-- * The 'TableDef' has the expected shape (UNLOGGED, no PK during
--   ingest, 12 columns in golden order).
-- * 'encodeAdaPotsCopy' produces correctly tab-separated,
--   newline-terminated COPY rows.
-- * Numeric fields land in the right column index — guards against
--   accidental field reordering between the record, the encoder,
--   and the table definition.
module DbSync.Schema.AdaPotsSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.AdaPots
  ( AdaPots (..)
  , adaPotsTableDef
  , encodeAdaPotsCopy
  )
import DbSync.Db.Schema.Ids (AdaPotsId (..), BlockId (..))
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Types (DbLovelace (..))

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "adaPotsTableDef" $ do
    it "is named ada_pots" $
      tdName adaPotsTableDef `shouldBe` "ada_pots"

    it "is UNLOGGED during ingest" $
      tdMode adaPotsTableDef `shouldBe` TableUnlogged

    it "has no PK during ingest (added in PreparingForVolatileTail)" $
      tdPrimaryKey adaPotsTableDef `shouldBe` Nothing

    it "carries no CHECK constraints" $
      tdChecks adaPotsTableDef `shouldBe` []

    it "carries no DEFAULT clauses" $
      tdColumnDefaults adaPotsTableDef `shouldBe` []

    it "has 12 columns in golden order" $
      map cdName (tdColumns adaPotsTableDef) `shouldBe`
        [ "id"
        , "slot_no"
        , "epoch_no"
        , "treasury"
        , "reserves"
        , "rewards"
        , "utxo"
        , "deposits_stake"
        , "fees"
        , "block_id"
        , "deposits_drep"
        , "deposits_proposal"
        ]

    it "uses BIGINT for id, slot_no, epoch_no, block_id" $ do
      cdType (tdColumns adaPotsTableDef !! 0) `shouldBe` PgBigInt
      cdType (tdColumns adaPotsTableDef !! 1) `shouldBe` PgBigInt
      cdType (tdColumns adaPotsTableDef !! 2) `shouldBe` PgBigInt
      cdType (tdColumns adaPotsTableDef !! 9) `shouldBe` PgBigInt

    it "uses NUMERIC for every Lovelace pot column" $ do
      -- treasury, reserves, rewards, utxo, deposits_stake, fees,
      -- deposits_drep, deposits_proposal — eight pots in total.
      let cols = tdColumns adaPotsTableDef
          potIndices = [3, 4, 5, 6, 7, 8, 10, 11]
      forM_ potIndices $ \i ->
        cdType (cols !! i) `shouldBe` PgNumeric

    it "marks every column NOT NULL" $
      all (not . cdNullable) (tdColumns adaPotsTableDef) `shouldBe` True

  describe "encodeAdaPotsCopy" $ do
    it "produces a row terminated with newline" $ do
      let row = encodeAdaPotsCopy (AdaPotsId 1) sampleAdaPots
      BS8.last row `shouldBe` '\n'

    it "produces 12 fields separated by 11 tabs" $ do
      let row = encodeAdaPotsCopy (AdaPotsId 1) sampleAdaPots
          tabCount = BS.count (fromIntegral (fromEnum '\t')) row
      tabCount `shouldBe` 11

    it "writes the assigned id in field 0" $ do
      let row = encodeAdaPotsCopy (AdaPotsId 42) sampleAdaPots
          fields = BS8.split '\t' (BS8.init row)
      fields !! 0 `shouldBe` "42"

    it "writes slot_no in field 1 and epoch_no in field 2" $ do
      let row = encodeAdaPotsCopy (AdaPotsId 1) sampleAdaPots
          fields = BS8.split '\t' (BS8.init row)
      fields !! 1 `shouldBe` "123456"
      fields !! 2 `shouldBe` "500"

    it "writes the eight Lovelace pots in the documented field order" $ do
      let row = encodeAdaPotsCopy (AdaPotsId 1) sampleAdaPots
          fields = BS8.split '\t' (BS8.init row)
      -- treasury, reserves, rewards, utxo, deposits_stake, fees, drep, proposal
      fields !! 3  `shouldBe` "1000000000000"   -- treasury
      fields !! 4  `shouldBe` "12000000000000"  -- reserves
      fields !! 5  `shouldBe` "5000000"         -- rewards
      fields !! 6  `shouldBe` "30000000000"     -- utxo
      fields !! 7  `shouldBe` "10000000"        -- deposits_stake
      fields !! 8  `shouldBe` "150000"          -- fees
      fields !! 10 `shouldBe` "200000"          -- deposits_drep
      fields !! 11 `shouldBe` "75000"           -- deposits_proposal

    it "writes block_id in field 9" $ do
      let row = encodeAdaPotsCopy (AdaPotsId 1) sampleAdaPots
          fields = BS8.split '\t' (BS8.init row)
      fields !! 9 `shouldBe` "777"

    it "encodes zero-valued pots as 0 (not NULL)" $ do
      let row = encodeAdaPotsCopy (AdaPotsId 1) zeroPots
          fields = BS8.split '\t' (BS8.init row)
      forM_ [3, 4, 5, 6, 7, 8, 10, 11] $ \i ->
        fields !! i `shouldBe` "0"

    it "round-trips a maximum-Word64-valued field" $ do
      let maxField = sampleAdaPots
            { adaPotsTreasury = DbLovelace 18446744073709551615
            }
          row = encodeAdaPotsCopy (AdaPotsId 1) maxField
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "18446744073709551615"

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
