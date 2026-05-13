{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @address@ table schema and COPY encoder.
--
-- Pure tests — no PostgreSQL required. Verifies the table-shape
-- invariants and the encoder behaviour for representative inputs:
-- a Shelley payment address (header byte set, payment cred extracted)
-- and a Byron-shaped one (no payment cred, no script bit).
module DbSync.Schema.AddressSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Address
  ( Address (..)
  , addressTableDef
  , encodeAddressCopy
  )
import DbSync.Db.Schema.Ids (AddressId (..), StakeAddressId (..))
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  )

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "addressTableDef" $ do
    it "is named address with 6 columns in golden order" $ do
      tdName addressTableDef `shouldBe` "address"
      map cdName (tdColumns addressTableDef) `shouldBe`
        ["id", "address", "raw", "has_script", "payment_cred", "stake_address_id"]

    it "uses the right column types" $ do
      let cols = tdColumns addressTableDef
      cdType (cols !! 0) `shouldBe` PgBigInt
      cdType (cols !! 1) `shouldBe` PgText
      cdType (cols !! 2) `shouldBe` PgBytea
      cdType (cols !! 3) `shouldBe` PgBoolean
      cdType (cols !! 4) `shouldBe` PgBytea
      cdType (cols !! 5) `shouldBe` PgBigInt

    it "marks payment_cred and stake_address_id as nullable" $ do
      let cols = tdColumns addressTableDef
      cdNullable (cols !! 4) `shouldBe` True
      cdNullable (cols !! 5) `shouldBe` True

    it "declares a unique constraint on raw" $
      tdUniqueConstraints addressTableDef `shouldBe` [pure "raw"]

  describe "encodeAddressCopy" $ do
    it "produces a 6-field tab-separated COPY row" $ do
      let row = encodeAddressCopy (AddressId 1) sampleAddress
          tabs = BS.count (fromIntegral (fromEnum '\t')) row
      BS8.last row `shouldBe` '\n'
      tabs `shouldBe` 5

    it "writes id, bech32 address, raw hex, has_script flag for a Shelley address" $ do
      let row = encodeAddressCopy (AddressId 7) sampleAddress
          fields = BS8.split '\t' (BS8.init row)
      fields !! 0 `shouldBe` "7"
      fields !! 1 `shouldBe` "addr_test1xyz"
      fields !! 2 `shouldBe` "\\\\x" <> BS8.replicate 2 '1' <> BS8.replicate 56 'a'
      fields !! 3 `shouldBe` "t"

    it "encodes payment_cred and stake_address_id as NULL when absent" $ do
      let row = encodeAddressCopy (AddressId 1)
                  sampleAddress
                    { addressPaymentCred    = Nothing
                    , addressStakeAddressId = Nothing
                    }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 4 `shouldBe` "\\N"
      fields !! 5 `shouldBe` "\\N"

    it "writes the 28-byte payment_cred as hex when present" $ do
      let row = encodeAddressCopy (AddressId 1)
                  sampleAddress { addressPaymentCred = Just (BS.replicate 28 0xab) }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 4 `shouldBe` "\\\\x" <> BS8.concat (replicate 28 "ab")

    it "writes stake_address_id as decimal int when present" $ do
      let row = encodeAddressCopy (AddressId 1)
                  sampleAddress { addressStakeAddressId = Just (StakeAddressId 99) }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 5 `shouldBe` "99"

    it "encodes has_script as f for non-script addresses" $ do
      let row = encodeAddressCopy (AddressId 1)
                  sampleAddress { addressHasScript = False }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "f"

-- ---------------------------------------------------------------------------
-- Fixture: a Shelley-shaped address with a 28-byte payment credential
-- ---------------------------------------------------------------------------

sampleAddress :: Address
sampleAddress = Address
  { addressAddress        = "addr_test1xyz"
  , addressRaw            = BS.pack (0x11 : replicate 28 0xaa)
  , addressHasScript      = True
  , addressPaymentCred    = Just (BS.replicate 28 0xaa)
  , addressStakeAddressId = Nothing
  }
