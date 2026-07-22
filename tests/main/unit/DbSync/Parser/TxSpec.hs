{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- | Direct coverage for the era @from*Tx@ parser converters.
--
-- The extractor specs hand-build 'GenericTx' fixtures that assume a
-- particular parser output shape; these tests feed real ledger txs
-- through the converters and pin the fields those fixtures depend on —
-- fee selection, phase-2 collateral folding, withdrawals, and
-- certificate extraction. The phase-2 cases deliberately mirror the
-- @alonzoFailedTx@ / @babbageFailedTx@ fixtures in
-- "DbSync.Extractor.UTxOSpec" so a change to the folding rule turns
-- this spec red instead of letting those fixtures drift silently.
module DbSync.Parser.TxSpec (spec) where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.List ((!!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

import Lens.Micro ((.~))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Cardano.Ledger.Address (AccountAddress (..), AccountId (..), Addr (..), Withdrawals (..))
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (..))
import Cardano.Ledger.Babbage.Core (totalCollateralTxBodyL)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (ConwayTxCert, Delegatee (..))
import Cardano.Ledger.Credential (StakeReference (StakeRefNull))
import Cardano.Ledger.Mary.Value (valueFromList)
import Cardano.Ledger.Plutus.Data (Datum (NoDatum))
import Cardano.Ledger.TxIn (TxIn)

import Ouroboros.Consensus.Shelley.Eras (AlonzoEra, BabbageEra, ConwayEra)

import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Tx.Generic
  ( unregisteredAddresses
  , unregisteredPools
  , unregisteredStakeCredentials
  )

import DbSync.Parser.Tx (credToCredHash, fromAlonzoTx, fromBabbageTx, fromConwayTx)
import DbSync.Parser.Types
  ( CertAction (..)
  , CredHash
  , GenericTx (..)
  , GenericTxCertificate (..)
  , GenericTxIn (..)
  , GenericTxOut (..)
  , GenericTxWithdrawal (..)
  )

spec :: Spec
spec = describe "DbSync.Parser.Tx" $ do
  describe "fromBabbageTx" $ do
    it "valid tx reports body fee, output sum, and declared inputs/outputs" $ do
      let body =
            Babbage.consPaymentTxBody
              (Set.fromList [babbageIns !! 0])
              (Set.fromList [babbageIns !! 1]) -- collateral present but ignored while valid
              mempty
              (StrictSeq.fromList [babOut addr0 100, babOut addr1 250])
              (SJust (babOut addr1 40)) -- collateral return, surfaced only while valid
              (Coin 17)
              mempty
          gtx = fromBabbageTx (7, Babbage.mkSimpleTx True body)
      txBlockIndex gtx `shouldBe` 7
      txValidContract gtx `shouldBe` True
      txFee gtx `shouldBe` 17
      txOutSum gtx `shouldBe` 350
      map txOutValue (txOutputs gtx) `shouldBe` [100, 250]
      map txInIndex (txInputs gtx) `shouldBe` [0]
      map txInIndex (txCollateralInputs gtx) `shouldBe` [1]
      (txOutValue <$> txCollateralOutput gtx) `shouldBe` Just 40

    it "phase-2 invalid tx folds collateral into inputs/outputs and charges total collateral" $ do
      let body =
            Babbage.consPaymentTxBody
              (Set.fromList [babbageIns !! 0]) -- declared input, dropped when invalid
              (Set.fromList [babbageIns !! 1]) -- collateral input, becomes the input
              mempty
              (StrictSeq.fromList [babOut addr0 100]) -- declared output, dropped when invalid
              (SJust (babOut addr1 40)) -- collateral return, becomes the output
              (Coin 99)
              mempty
              & totalCollateralTxBodyL .~ SJust (Coin 30)
          gtx = fromBabbageTx (0, Babbage.mkSimpleTx False body)
      txValidContract gtx `shouldBe` False
      txFee gtx `shouldBe` 30 -- total collateral, not the body fee 99
      map txInIndex (txInputs gtx) `shouldBe` [1] -- folded to the collateral input
      map txOutValue (txOutputs gtx) `shouldBe` [40] -- folded to the collateral return
      map txOutIndex (txOutputs gtx) `shouldBe` [1] -- numbered at the regular-output count
      txOutSum gtx `shouldBe` 40
      map txInIndex (txCollateralInputs gtx) `shouldBe` [1]
      txCollateralOutput gtx `shouldSatisfy` isNothing

  describe "fromConwayTx" $ do
    it "extracts withdrawals as (reward-account, amount) pairs" $ do
      let wdrls =
            Withdrawals $
              Map.fromList
                [ (AccountAddress Testnet (AccountId (unregisteredStakeCredentials !! 0)), Coin 500)
                , (AccountAddress Testnet (AccountId (unregisteredStakeCredentials !! 1)), Coin 700)
                ]
          gtx = fromConwayTx (0, Conway.mkSimpleTx True (Conway.consCertTxBody Nothing [] wdrls))
      length (txWithdrawals gtx) `shouldBe` 2
      sort (map txwAmount (txWithdrawals gtx)) `shouldBe` [500, 700]
      map (BS.length . txwRewardAddress) (txWithdrawals gtx) `shouldBe` [29, 29]

    it "extracts delegation certificates in body order with deposits and credentials" $ do
      let certs :: [ConwayTxCert ConwayEra]
          certs =
            [ Conway.mkRegTxCert SNothing (unregisteredStakeCredentials !! 0)
            , Conway.mkRegTxCert (SJust (Coin 2_000_000)) (unregisteredStakeCredentials !! 1)
            , Conway.mkDelegTxCert (DelegStake (unregisteredPools !! 0)) (unregisteredStakeCredentials !! 2)
            , Conway.mkUnRegTxCert SNothing (unregisteredStakeCredentials !! 0)
            ]
          gtx = fromConwayTx (0, Conway.mkSimpleTx True (Conway.consCertTxBody Nothing certs (Withdrawals mempty)))
          actions = map txCertAction (txCertificates gtx)
      map txCertIndex (txCertificates gtx) `shouldBe` [0, 1, 2, 3]
      map certKind actions `shouldBe` ["reg", "reg", "deleg", "dereg"]
      map certDeposit actions `shouldBe` [Nothing, Just 2_000_000, Nothing, Nothing]
      -- Credential routed through credToCredHash with the key/script flag intact.
      certCred (headDef (panic "no cert") actions)
        `shouldBe` Just (credToCredHash (unregisteredStakeCredentials !! 0))
      -- Pool key hash surfaces as 28 raw bytes on the delegation cert.
      (BS.length <$> certPool (actions !! 2)) `shouldBe` Just 28

    it "carries the Conway treasury donation" $ do
      let body =
            Conway.consTxBody
              mempty mempty mempty mempty SNothing (Coin 0) mempty [] (Withdrawals mempty) (Coin 12345)
          gtx = fromConwayTx (0, Conway.mkSimpleTx True body)
      txTreasuryDonation gtx `shouldBe` 12345

  describe "phase-2 failure shape (pins the Extractor.UTxOSpec fixtures)" $ do
    it "Alonzo failure: inputs are the collateral, outputs empty, no collateral return" $ do
      let body =
            Alonzo.consPaymentTxBody
              (Set.fromList [alonzoIns !! 0])
              (Set.fromList [alonzoIns !! 1])
              (StrictSeq.fromList [aloOut addr0 100])
              (Coin 50)
              mempty
          gtx = fromAlonzoTx (0, Alonzo.mkSimpleTx False body)
      txValidContract gtx `shouldBe` False
      txFee gtx `shouldBe` 0 -- Alonzo has no total-collateral field; backfilled post-load
      map txInIndex (txInputs gtx) `shouldBe` [1]
      map txInIndex (txCollateralInputs gtx) `shouldBe` [1]
      txOutputs gtx `shouldSatisfy` null
      txCollateralOutput gtx `shouldSatisfy` isNothing

    it "Babbage failure: collateral return folds into a single tx_out" $ do
      let body =
            Babbage.consPaymentTxBody
              (Set.fromList [babbageIns !! 0])
              (Set.fromList [babbageIns !! 1])
              mempty
              (StrictSeq.fromList [babOut addr0 100])
              (SJust (babOut addr1 40))
              (Coin 0)
              mempty
          gtx = fromBabbageTx (0, Babbage.mkSimpleTx False body)
      txValidContract gtx `shouldBe` False
      map txInIndex (txInputs gtx) `shouldBe` [1]
      map txOutValue (txOutputs gtx) `shouldBe` [40]
      txCollateralOutput gtx `shouldSatisfy` isNothing

-- ---------------------------------------------------------------------------
-- * Ledger-value fixtures
-- ---------------------------------------------------------------------------

addr0, addr1 :: Addr
addr0 = Addr Testnet (unregisteredAddresses !! 0) StakeRefNull
addr1 = Addr Testnet (unregisteredAddresses !! 1) StakeRefNull

babOut :: Addr -> Integer -> BabbageTxOut BabbageEra
babOut a v = BabbageTxOut a (valueFromList (Coin v) []) NoDatum SNothing

aloOut :: Addr -> Integer -> AlonzoTxOut AlonzoEra
aloOut a v = AlonzoTxOut a (valueFromList (Coin v) []) SNothing

-- | Distinct 'TxIn's derived from a throwaway source tx's outputs, so
-- input-index assertions can tell the declared input (idx 0) from the
-- collateral input (idx 1).
babbageIns :: [TxIn]
babbageIns = fst <$> Babbage.mkUTxOBabbage src
  where
    src =
      Babbage.mkSimpleTx True $
        Babbage.consPaymentTxBody
          mempty mempty mempty
          (StrictSeq.fromList [babOut addr0 1, babOut addr0 2])
          SNothing
          (Coin 0)
          mempty

alonzoIns :: [TxIn]
alonzoIns = fst <$> Alonzo.mkUTxOAlonzo src
  where
    src =
      Alonzo.mkSimpleTx True $
        Alonzo.consPaymentTxBody
          mempty mempty
          (StrictSeq.fromList [aloOut addr0 1, aloOut addr0 2])
          (Coin 0)
          mempty

-- ---------------------------------------------------------------------------
-- * CertAction projections
-- ---------------------------------------------------------------------------

certKind :: CertAction -> Text
certKind = \case
  CertStakeRegistration {} -> "reg"
  CertStakeDeregistration {} -> "dereg"
  CertDelegation {} -> "deleg"
  _ -> "other"

certDeposit :: CertAction -> Maybe Word64
certDeposit = \case
  CertStakeRegistration _ md -> md
  _ -> Nothing

certCred :: CertAction -> Maybe CredHash
certCred = \case
  CertStakeRegistration c _ -> Just c
  CertStakeDeregistration c -> Just c
  CertDelegation c _ -> Just c
  _ -> Nothing

certPool :: CertAction -> Maybe ByteString
certPool = \case
  CertDelegation _ p -> Just p
  _ -> Nothing
