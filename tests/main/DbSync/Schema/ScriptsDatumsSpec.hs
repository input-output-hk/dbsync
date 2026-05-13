{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @scripts_datums@ schema and COPY encoders.
--
-- Pure tests — no PostgreSQL required. Coverage focuses on:
--
-- * 'TableDef' shape (column names, ordering, nullability, enum types).
-- * COPY encoder field counts and the per-enum string values that hit
--   the wire (drift between Haskell constructor and PG enum value would
--   otherwise corrupt data silently).
--
-- End-to-end correctness against forged transactions lands later
-- (commit 6, chain-gen mirroring).
module DbSync.Schema.ScriptsDatumsSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Ids
  ( DatumId (..)
  , ExtraKeyWitnessId (..)
  , RedeemerDataId (..)
  , RedeemerId (..)
  , ScriptId (..)
  , TxId (..)
  )
import DbSync.Db.Schema.ScriptsDatums
  ( Datum (..)
  , ExtraKeyWitness (..)
  , Redeemer (..)
  , RedeemerData (..)
  , Script (..)
  , datumTableDef
  , encodeDatumCopy
  , encodeExtraKeyWitnessCopy
  , encodeRedeemerCopy
  , encodeRedeemerDataCopy
  , encodeScriptCopy
  , extraKeyWitnessTableDef
  , redeemerDataTableDef
  , redeemerTableDef
  , scriptTableDef
  )
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  )
import DbSync.Db.Types
  ( DbLovelace (..)
  , ScriptPurpose (..)
  , ScriptType (..)
  )

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "datumTableDef" $ do
    it "is named datum with 5 columns including a JSONB value" $ do
      tdName datumTableDef `shouldBe` "datum"
      map cdName (tdColumns datumTableDef) `shouldBe`
        ["id", "hash", "tx_id", "value", "bytes"]
      cdType (tdColumns datumTableDef !! 3) `shouldBe` PgJsonb

    it "declares a unique constraint on hash" $
      tdUniqueConstraints datumTableDef `shouldBe` [pure "hash"]

  describe "encodeDatumCopy" $ do
    it "produces 5 fields, NULL value when datumValue is Nothing" $ do
      let row = encodeDatumCopy (DatumId 1) sampleDatum { datumValue = Nothing }
          fields = BS8.split '\t' (BS8.init row)
      length fields `shouldBe` 5
      fields !! 3 `shouldBe` "\\N"

    it "writes JSONB value as plain text when present" $ do
      let row = encodeDatumCopy (DatumId 1)
                  sampleDatum { datumValue = Just "{\"k\":1}" }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "{\"k\":1}"

  describe "scriptTableDef" $ do
    it "uses the scripttype enum for the type column" $
      cdType (tdColumns scriptTableDef !! 3) `shouldBe` PgEnum "scripttype"

    it "is unique on hash and has 7 columns total" $ do
      tdUniqueConstraints scriptTableDef `shouldBe` [pure "hash"]
      length (tdColumns scriptTableDef) `shouldBe` 7

  describe "encodeScriptCopy" $ do
    it "encodes every ScriptType enum value as the matching PG string" $
      forM_
        [ (MultiSig, "multisig")
        , (Timelock, "timelock")
        , (PlutusV1, "plutusV1")
        , (PlutusV2, "plutusV2")
        , (PlutusV3, "plutusV3")
        , (PlutusV4, "plutusV4")
        ] $ \(t, expected) -> do
          let row = encodeScriptCopy (ScriptId 1) sampleScript { scriptType = t }
              fields = BS8.split '\t' (BS8.init row)
          fields !! 3 `shouldBe` expected

    it "encodes optional bytes / json / serialised_size as NULL when absent" $ do
      let row = encodeScriptCopy (ScriptId 1)
                  sampleScript
                    { scriptJson = Nothing
                    , scriptBytes = Nothing
                    , scriptSerialisedSize = Nothing
                    }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 4 `shouldBe` "\\N"
      fields !! 5 `shouldBe` "\\N"
      fields !! 6 `shouldBe` "\\N"

  describe "redeemerTableDef" $ do
    it "uses scriptpurposetype for purpose and has 9 columns" $ do
      cdType (tdColumns redeemerTableDef !! 5) `shouldBe` PgEnum "scriptpurposetype"
      length (tdColumns redeemerTableDef) `shouldBe` 9

    it "has no unique constraints (a tx can carry many redeemers)" $
      tdUniqueConstraints redeemerTableDef `shouldBe` []

  describe "encodeRedeemerCopy" $ do
    it "encodes every ScriptPurpose enum value as the matching PG string" $
      forM_
        [ (Spend,   "spend")
        , (Mint,    "mint")
        , (Cert,    "cert")
        , (Rewrd,   "reward")
        , (Vote,    "vote")
        , (Propose, "propose")
        ] $ \(p, expected) -> do
          let row = encodeRedeemerCopy (RedeemerId 1) sampleRedeemer { redeemerPurpose = p }
              fields = BS8.split '\t' (BS8.init row)
          fields !! 5 `shouldBe` expected

    it "encodes optional fee and script_hash as NULL when absent" $ do
      let row = encodeRedeemerCopy (RedeemerId 1)
                  sampleRedeemer
                    { redeemerFee = Nothing
                    , redeemerScriptHash = Nothing
                    }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 4 `shouldBe` "\\N"
      fields !! 7 `shouldBe` "\\N"

    it "encodes Word64 unit_mem and unit_steps as decimal ASCII" $ do
      let row = encodeRedeemerCopy (RedeemerId 1)
                  sampleRedeemer { redeemerUnitMem = 12345, redeemerUnitSteps = 999_999_999 }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 2 `shouldBe` "12345"
      fields !! 3 `shouldBe` "999999999"

  describe "redeemerDataTableDef" $ do
    it "is named redeemer_data with the same shape as datum" $ do
      tdName redeemerDataTableDef `shouldBe` "redeemer_data"
      map cdName (tdColumns redeemerDataTableDef) `shouldBe`
        ["id", "hash", "tx_id", "value", "bytes"]
      tdUniqueConstraints redeemerDataTableDef `shouldBe` [pure "hash"]

  describe "encodeRedeemerDataCopy" $ do
    it "produces a 5-field row with the expected tab count" $ do
      let row = encodeRedeemerDataCopy (RedeemerDataId 1) sampleRedeemerData
          tabs = BS.count (fromIntegral (fromEnum '\t')) row
      BS8.last row `shouldBe` '\n'
      tabs `shouldBe` 4

  describe "extraKeyWitnessTableDef" $ do
    it "is the trivial hash + tx_id table" $ do
      tdName extraKeyWitnessTableDef `shouldBe` "extra_key_witness"
      map cdName (tdColumns extraKeyWitnessTableDef) `shouldBe`
        ["id", "hash", "tx_id"]
      tdUniqueConstraints extraKeyWitnessTableDef `shouldBe` []

  describe "encodeExtraKeyWitnessCopy" $ do
    it "produces 3 fields with the id, hex hash, and tx_id" $ do
      let row = encodeExtraKeyWitnessCopy (ExtraKeyWitnessId 7)
                  ExtraKeyWitness
                    { extraKeyWitnessHash = BS.pack [0xab, 0xcd]
                    , extraKeyWitnessTxId = TxId 42
                    }
          fields = BS8.split '\t' (BS8.init row)
      length fields `shouldBe` 3
      fields !! 0 `shouldBe` "7"
      fields !! 1 `shouldBe` "\\\\xabcd"
      fields !! 2 `shouldBe` "42"

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

sampleDatum :: Datum
sampleDatum = Datum
  { datumHash  = BS.replicate 32 0xaa
  , datumTxId  = TxId 100
  , datumValue = Just "{\"v\":42}"
  , datumBytes = BS.replicate 8 0xbb
  }

sampleScript :: Script
sampleScript = Script
  { scriptTxId           = TxId 100
  , scriptHash           = BS.replicate 28 0xcc
  , scriptType           = PlutusV2
  , scriptJson           = Just "{\"name\":\"example\"}"
  , scriptBytes          = Just (BS.replicate 16 0xdd)
  , scriptSerialisedSize = Just 1024
  }

sampleRedeemer :: Redeemer
sampleRedeemer = Redeemer
  { redeemerTxId           = TxId 100
  , redeemerUnitMem        = 1000
  , redeemerUnitSteps      = 200000
  , redeemerFee            = Just (DbLovelace 50000)
  , redeemerPurpose        = Spend
  , redeemerIndex          = 0
  , redeemerScriptHash     = Just (BS.replicate 28 0xee)
  , redeemerRedeemerDataId = RedeemerDataId 9
  }

sampleRedeemerData :: RedeemerData
sampleRedeemerData = RedeemerData
  { redeemerDataHash  = BS.replicate 32 0xff
  , redeemerDataTxId  = TxId 100
  , redeemerDataValue = Just "{\"d\":1}"
  , redeemerDataBytes = BS.replicate 8 0x11
  }
