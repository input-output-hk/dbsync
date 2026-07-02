{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @address@ table schema and COPY encoder.
--
-- Pure tests — no PostgreSQL required. Verifies the table-shape
-- invariants and the encoder behaviour for representative inputs:
-- a Shelley payment address (header byte set, payment cred extracted)
-- and a Byron-shaped one (no payment cred, no script bit).
module DbSync.Schema.AddressSpec (spec) where

import Cardano.Prelude

import qualified Cardano.Chain.Common as Byron
import Cardano.Ledger.Binary (byronProtVer, serialize')

import Data.List ((!!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TextEnc

import Test.Cardano.Chain.Common.Example (exampleAddress, exampleAddress1, exampleAddress2)
import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Address
  ( Address (..)
  , addressFromRaw
  , addressTableDef
  , encodeAddressCopy
  , rawToDisplayText
  )
import DbSync.Db.Schema.Ids (AddressId (..), StakeAddressId (..))
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  )
import DbSync.Util.Bech32 (serialiseShelleyAddrToBech32)

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "addressTableDef" $ do
    it "is named address with 7 columns in golden order" $ do
      tdName addressTableDef `shouldBe` "address"
      map cdName (tdColumns addressTableDef) `shouldBe`
        ["id", "address", "raw", "has_script", "payment_cred", "stake_address_id", "raw_hash"]

    it "uses the right column types" $ do
      let cols = tdColumns addressTableDef
      cdType (cols !! 0) `shouldBe` PgBigInt
      cdType (cols !! 1) `shouldBe` PgText
      cdType (cols !! 2) `shouldBe` PgBytea
      cdType (cols !! 3) `shouldBe` PgBoolean
      cdType (cols !! 4) `shouldBe` PgBytea
      cdType (cols !! 5) `shouldBe` PgBigInt
      cdType (cols !! 6) `shouldBe` PgBytea

    it "marks payment_cred and stake_address_id as nullable" $ do
      let cols = tdColumns addressTableDef
      cdNullable (cols !! 4) `shouldBe` True
      cdNullable (cols !! 5) `shouldBe` True

    it "declares a unique constraint on raw_hash (md5(raw)) to skirt btree's row-size limit" $
      tdUniqueConstraints addressTableDef `shouldBe` [pure "raw_hash"]

    it "declares raw_hash as a GENERATED column computing decode(md5(raw), 'hex')" $
      tdGeneratedColumns addressTableDef
        `shouldBe` [("raw_hash", "decode(md5(raw), 'hex')")]

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

  describe "rawToDisplayText" $ do
    it "renders Shelley payment addresses (header high bit clear) as Bech32" $ do
      let mainnet = BS.cons 0x01 (BS.replicate 56 0xaa)
          testnet = BS.cons 0x00 (BS.replicate 56 0xaa)
      rawToDisplayText mainnet `shouldBe` serialiseShelleyAddrToBech32 mainnet
      rawToDisplayText testnet `shouldBe` serialiseShelleyAddrToBech32 testnet

    -- The stored raw is the address's byron-CBOR serialisation, so Base58
    -- of the raw must equal the ledger's 'addrToBase58' on the decoded
    -- address. This is the high-bit-set discrimination branch.
    it "renders Byron bootstrap addresses as Base58, matching addrToBase58" $
      forM_ [exampleAddress, exampleAddress1, exampleAddress2] $ \byronAddr ->
        rawToDisplayText (serialize' byronProtVer byronAddr)
          `shouldBe` TextEnc.decodeUtf8 (Byron.addrToBase58 byronAddr)

  describe "addressFromRaw" $
    it "derives address, has_script, and payment_cred from the raw bytes" $ do
      -- header 0x11: base address with a script payment credential (bit 4), mainnet
      let raw = BS.cons 0x11 (BS.replicate 56 0xaa)
          a   = addressFromRaw raw (Just (StakeAddressId 42))
      addressRaw a            `shouldBe` raw
      addressAddress a        `shouldBe` serialiseShelleyAddrToBech32 raw
      addressHasScript a      `shouldBe` True
      addressPaymentCred a    `shouldBe` Just (BS.replicate 28 0xaa)
      addressStakeAddressId a `shouldBe` Just (StakeAddressId 42)

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
