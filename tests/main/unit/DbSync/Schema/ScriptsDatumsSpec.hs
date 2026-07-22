{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @scripts_datums@ COPY encoders: the per-enum string
-- values that hit the wire (drift between Haskell constructor and PG
-- enum value would otherwise corrupt data silently) and NULL encoding.
module DbSync.Schema.ScriptsDatumsSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Ids
  ( DatumId (..)
  , RedeemerDataId (..)
  , RedeemerId (..)
  , ScriptId (..)
  , TxId (..)
  )
import DbSync.Db.Schema.ScriptsDatums
  ( Datum (..)
  , ExtraKeyWitness (..)
  , Redeemer (..)
  , Script (..)
  , encodeDatumCopy
  , encodeExtraKeyWitnessCopy
  , encodeRedeemerCopy
  , encodeScriptCopy
  )
import DbSync.Db.Types
  ( DbLovelace (..)
  , ScriptPurpose (..)
  , ScriptType (..)
  )

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "encodeDatumCopy" $ do
    it "encodes value as NULL when datumValue is Nothing" $ do
      let row = encodeDatumCopy (DatumId 1) sampleDatum { datumValue = Nothing }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "\\N"

    it "writes JSONB value as plain text when present" $ do
      let row = encodeDatumCopy (DatumId 1)
                  sampleDatum { datumValue = Just "{\"k\":1}" }
          fields = BS8.split '\t' (BS8.init row)
      fields !! 3 `shouldBe` "{\"k\":1}"

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

  describe "encodeExtraKeyWitnessCopy" $ do
    it "produces 2 fields with the hex hash and tx_id (id is server-assigned)" $ do
      let row = encodeExtraKeyWitnessCopy
                  ExtraKeyWitness
                    { extraKeyWitnessHash = BS.pack [0xab, 0xcd]
                    , extraKeyWitnessTxId = TxId 42
                    }
          fields = BS8.split '\t' (BS8.init row)
      length fields `shouldBe` 2
      fields !! 0 `shouldBe` "\\\\xabcd"
      fields !! 1 `shouldBe` "42"

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
