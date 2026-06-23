{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Follow-phase coverage of the @scripts_datums@ extractor.
--
-- Forges a two-tx Plutus workflow at tip:
--
--   * Lock block: payment tx that sends a UTxO to
--     @alwaysSucceedsScriptAddr@ with a datum hash.
--   * Unlock block: spends that script-locked UTxO. The witness set
--     carries the Plutus script + datum value + redeemer; the txbody
--     carries an extra required-signer hash.
--
-- The unlock block exercises all five 'scripts_datums' writers in a
-- single Follow transaction.
module DbSync.Phase.FollowScriptsDatumsSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Binary (sizedValue)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Tx (AlonzoTx (..), Tx (..))
import Cardano.Ledger.Conway.TxBody (TxBody (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness), coerceKeyRole)
import qualified Cardano.Ledger.Mary.Value as Mary
import Cardano.Ledger.TxIn (TxIn (..))
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)

import qualified Cardano.Mock.Forging.Interpreter as Mock
import qualified Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples as Scripts
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Generic as Generic
import qualified Cardano.Mock.Forging.Types as Mock

import DbSync.App.Config.Types
  ( SyncConfig (..)
  , OptionFlag (..)
  , DbSyncOptions (..)
  )
import DbSync.Db.Schema.Core (blockTableDef, txTableDef)
import DbSync.Db.Schema.MultiAsset (maTxMintTableDef)
import DbSync.Db.Schema.ScriptsDatums
  ( datumTableDef
  , extraKeyWitnessTableDef
  , redeemerDataTableDef
  , redeemerTableDef
  , scriptTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (collateralTxOutTableDef, txOutTableDef)
import DbSync.Test.AppHarness
  ( ledgerEnabledTestProfile
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E (conwayConfigDir, withAppSession)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockChain (MockChain (..))
import DbSync.Test.MockNode
  ( MockNode (..)
  , forgeAndPush
  , forgeAndPushBlocks
  , withMockNode
  )
import DbSync.Test.PgAssertions (countRows)

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Follow scripts/datums writes" $ do
  it "lands script, datum, redeemer, redeemer_data, extra_key_witness rows for a Plutus tx at tip" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-follow-scripts-datums" $ \ledgerDir -> do
        tracer <- quietTracer

        -- 250 empty blocks puts the tip past two epoch boundaries so
        -- Ingest exits cleanly before withAppSession enters Follow.
        _ <- forgeAndPushBlocks mn 250

        withAppSession tracer scriptsDatumsTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          baselineBlocks       <- countRows (tdName blockTableDef)
          baselineScripts      <- countRows (tdName scriptTableDef)
          baselineDatums       <- countRows (tdName datumTableDef)
          baselineRedeemers    <- countRows (tdName redeemerTableDef)
          baselineRedeemerData <- countRows (tdName redeemerDataTableDef)
          baselineExtraKW      <- countRows (tdName extraKeyWitnessTableDef)

          lockTxs <- buildLockTxs mn
          _ <- forgeAndPush mn lockTxs

          unlockTxs <- buildUnlockTxs mn
          _ <- forgeAndPush mn unlockTxs

          let expectedBlocks = baselineBlocks + 2
          waitFor
            (tdName blockTableDef <> " count reaches " <> show expectedBlocks)
            (do n <- countRows (tdName blockTableDef); pure (n >= expectedBlocks))
            60

          waitFor
            (tdName extraKeyWitnessTableDef <> " count grows")
            (do n <- countRows (tdName extraKeyWitnessTableDef); pure (n > baselineExtraKW))
            30

          followScripts      <- countRows (tdName scriptTableDef)
          followDatums       <- countRows (tdName datumTableDef)
          followRedeemers    <- countRows (tdName redeemerTableDef)
          followRedeemerData <- countRows (tdName redeemerDataTableDef)
          followExtraKW      <- countRows (tdName extraKeyWitnessTableDef)

          (followScripts      - baselineScripts)      `shouldSatisfy` (>= 1)
          (followDatums       - baselineDatums)       `shouldSatisfy` (>= 1)
          (followRedeemers    - baselineRedeemers)    `shouldSatisfy` (>= 1)
          (followRedeemerData - baselineRedeemerData) `shouldSatisfy` (>= 1)
          (followExtraKW      - baselineExtraKW)      `shouldSatisfy` (>= 1)

          -- Every redeemer row points at a redeemer_data row.
          nullFkRows <- T.strip <$> queryTestDb
            ( "SELECT COUNT(*)::text FROM " <> tdName redeemerTableDef
                <> " WHERE redeemer_data_id IS NULL"
            )
          nullFkRows `shouldBe` "0"

  it "lands one redeemer when lock and unlock share a block" $
    withScriptsDatumsSession "dbsync-test-scripts-same-block" $ \mn -> do
      baselineBlocks    <- countRows (tdName blockTableDef)
      baselineRedeemers <- countRows (tdName redeemerTableDef)
      baselineDatums    <- countRows (tdName datumTableDef)

      txs <- buildSameBlockLockUnlock mn
      _ <- forgeAndPush mn txs

      let expectedBlocks = baselineBlocks + 1
      waitFor
        (tdName blockTableDef <> " count reaches " <> show expectedBlocks)
        (do n <- countRows (tdName blockTableDef); pure (n >= expectedBlocks))
        60
      waitFor
        (tdName redeemerTableDef <> " count grows")
        (do n <- countRows (tdName redeemerTableDef); pure (n > baselineRedeemers))
        30

      followRedeemers <- countRows (tdName redeemerTableDef)
      followDatums    <- countRows (tdName datumTableDef)
      (followRedeemers - baselineRedeemers) `shouldBe` 1
      (followDatums    - baselineDatums)    `shouldSatisfy` (>= 1)

  it "records a phase-2 failure with valid_contract=false, folding the collateral return into tx_out" $
    withScriptsDatumsSession "dbsync-test-scripts-failed" $ \mn -> do
      baselineBlocks <- countRows (tdName blockTableDef)
      baselineTxOut  <- countRows (tdName txOutTableDef)
      baselineColOut <- countRows (tdName collateralTxOutTableDef)

      lockTxs <- buildLockTxs mn
      _ <- forgeAndPush mn lockTxs
      failTxs <- buildFailingUnlockTxs mn
      _ <- forgeAndPush mn failTxs

      let expectedBlocks = baselineBlocks + 2
      waitFor
        (tdName blockTableDef <> " count reaches " <> show expectedBlocks)
        (do n <- countRows (tdName blockTableDef); pure (n >= expectedBlocks))
        60
      -- The surviving collateral return lands in tx_out, not collateral_tx_out.
      waitFor
        (tdName txOutTableDef <> " count grows")
        (do n <- countRows (tdName txOutTableDef); pure (n > baselineTxOut))
        30

      latestValid <- T.strip <$> queryTestDb
        ( "SELECT valid_contract::text FROM " <> tdName txTableDef
            <> " ORDER BY id DESC LIMIT 1"
        )
      latestValid `shouldBe` "false"

      -- A failed tx writes no collateral_tx_out row.
      followColOut <- countRows (tdName collateralTxOutTableDef)
      (followColOut - baselineColOut) `shouldBe` 0

  it "writes one redeemer per spent script when several are unlocked together" $
    withScriptsDatumsSession "dbsync-test-scripts-multi" $ \mn -> do
      baselineBlocks    <- countRows (tdName blockTableDef)
      baselineRedeemers <- countRows (tdName redeemerTableDef)

      txs <- buildMultipleScriptsTxs mn
      _ <- forgeAndPush mn txs

      let expectedBlocks = baselineBlocks + 1
      waitFor
        (tdName blockTableDef <> " count reaches " <> show expectedBlocks)
        (do n <- countRows (tdName blockTableDef); pure (n >= expectedBlocks))
        60
      waitFor
        (tdName redeemerTableDef <> " reaches +2")
        (do n <- countRows (tdName redeemerTableDef); pure (n >= baselineRedeemers + 2))
        30

      followRedeemers <- countRows (tdName redeemerTableDef)
      (followRedeemers - baselineRedeemers) `shouldBe` 2

  it "writes a collateral_tx_out row for a valid tx that declares a collateral return" $
    withScriptsDatumsSession "dbsync-test-scripts-col" $ \mn -> do
      baselineBlocks <- countRows (tdName blockTableDef)
      baselineColOut <- countRows (tdName collateralTxOutTableDef)

      lockTxs <- buildLockTxs mn
      _ <- forgeAndPush mn lockTxs
      validTxs <- buildValidUnlockWithCollateralReturnTxs mn
      _ <- forgeAndPush mn validTxs

      let expectedBlocks = baselineBlocks + 2
      waitFor
        (tdName blockTableDef <> " count reaches " <> show expectedBlocks)
        (do n <- countRows (tdName blockTableDef); pure (n >= expectedBlocks))
        60
      waitFor
        (tdName collateralTxOutTableDef <> " count grows")
        (do n <- countRows (tdName collateralTxOutTableDef); pure (n > baselineColOut))
        30

      nullTxIds <- T.strip <$> queryTestDb
        ( "SELECT COUNT(*)::text FROM " <> tdName collateralTxOutTableDef
            <> " WHERE tx_id IS NULL"
        )
      nullTxIds `shouldBe` "0"

  it "lands a ma_tx_mint row when a Plutus minting policy mints a token" $
    withScriptsDatumsSession "dbsync-test-scripts-mint" $ \mn -> do
      baselineBlocks <- countRows (tdName blockTableDef)
      baselineMint   <- countRows (tdName maTxMintTableDef)

      txs <- buildMintTxs mn
      _ <- forgeAndPush mn txs

      let expectedBlocks = baselineBlocks + 1
      waitFor
        (tdName blockTableDef <> " count reaches " <> show expectedBlocks)
        (do n <- countRows (tdName blockTableDef); pure (n >= expectedBlocks))
        60
      waitFor
        (tdName maTxMintTableDef <> " count grows")
        (do n <- countRows (tdName maTxMintTableDef); pure (n > baselineMint))
        30

      followMint <- countRows (tdName maTxMintTableDef)
      (followMint - baselineMint) `shouldSatisfy` (>= 1)

-- ---------------------------------------------------------------------------
-- * Profile
-- ---------------------------------------------------------------------------

-- | 'ledgerEnabledTestProfile' with @pcScriptsDatums@ flipped on.
scriptsDatumsTestProfile :: SyncConfig
scriptsDatumsTestProfile =
  ledgerEnabledTestProfile
    { scOptions = (scOptions ledgerEnabledTestProfile)
        { pcScriptsDatums = OptionFlag True
        }
    }

-- ---------------------------------------------------------------------------
-- * Plutus tx builders
-- ---------------------------------------------------------------------------

-- | Lock a 500_000-lovelace UTxO at @alwaysSucceedsScriptAddr@ with a
-- datum hash. No witness data — this block doesn't write any
-- scripts/datums rows.
buildLockTxs :: MockNode -> IO [Mock.TxEra]
buildLockTxs mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' ->
    case Conway.mkLockByScriptTx
           (Mock.UTxOIndex 0)
           [Babbage.TxOutNoInline True]
           500_000
           1_000
           state' of
      Right tx -> pure [Mock.TxConway tx]
      Left err -> panic $ "buildLockTxs: " <> show err

-- | Spend the script-locked UTxO; the witness set carries the script,
-- datum, and redeemer. The required-signer hash drives the
-- @extra_key_witness@ writer.
buildUnlockTxs :: MockNode -> IO [Mock.TxEra]
buildUnlockTxs mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' ->
    case Conway.mkUnlockScriptTx
           [Mock.UTxOAddress Scripts.alwaysSucceedsScriptAddr]
           (Mock.UTxOIndex 0)
           (Mock.UTxOAddressNew 0)
           True
           499_000
           1_000
           state' of
       Right tx -> pure [Mock.TxConway (withRequiredSigner reqSigner tx)]
       Left err -> panic $ "buildUnlockTxs: " <> show err

-- | Inject a required-signer key hash into the Conway txbody's
-- @ctbReqSignerHashes@. The mock's 'consTxBody' hard-codes 'mempty';
-- this override is the only way to populate the field from the test.
-- 'coerceKeyRole' bridges the test fixture's @KeyHash Witness@ and
-- the ledger field's @KeyHash Guard@ tag.
withRequiredSigner
  :: KeyHash Witness
  -> Core.Tx Core.TopTx ConwayEra
  -> Core.Tx Core.TopTx ConwayEra
withRequiredSigner h (MkConwayTx atx) =
  MkConwayTx atx
    { atBody = (atBody atx)
        { ctbReqSignerHashes =
            Set.insert (coerceKeyRole h) (ctbReqSignerHashes (atBody atx))
        }
    }

-- | Shared bootstrap for every scripts/datums Follow scenario.
withScriptsDatumsSession :: Text -> (MockNode -> IO ()) -> IO ()
withScriptsDatumsSession tag body =
  withMockNode conwayConfigDir $ \mn ->
    withTempDir tag $ \ledgerDir -> do
      tracer <- quietTracer
      _ <- forgeAndPushBlocks mn 250
      withAppSession tracer scriptsDatumsTestProfile mn ledgerDir $ \_ -> do
        waitForSyncComplete 120
        body mn

-- | Reference an output of a forged Conway tx as a 'UTxOPair' so the
-- next tx can spend it without round-tripping through the ledger.
outputAsPair :: Core.Tx Core.TopTx ConwayEra -> Int -> Mock.UTxOIndex ConwayEra
outputAsPair (MkConwayTx atx) ix =
  let body = atBody atx
      txId = Core.txIdTxBody body
      out  = sizedValue (toList (ctbOutputs body) !! ix)
   in Mock.UTxOPair (TxIn txId (TxIx (fromIntegral ix)), out)

-- | The first required-signer key used by every unlock builder.
reqSigner :: KeyHash Witness
reqSigner = Generic.unregisteredWitnessKey !! 0

-- | Build a lock+unlock pair that share a single block via 'UTxOPair'.
buildSameBlockLockUnlock :: MockNode -> IO [Mock.TxEra]
buildSameBlockLockUnlock mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    lockTx <- Conway.mkLockByScriptTx
      (Mock.UTxOIndex 0)
      [Babbage.TxOutNoInline True]
      500_000 1_000 state'
    unlockTx <- Conway.mkUnlockScriptTx
      [outputAsPair lockTx 0]
      (Mock.UTxOIndex 1)
      (Mock.UTxOAddressNew 0)
      True 499_000 1_000 state'
    Right [Mock.TxConway lockTx, Mock.TxConway (withRequiredSigner reqSigner unlockTx)]

-- | Lock three script outputs, then in the same block spend two of them.
buildMultipleScriptsTxs :: MockNode -> IO [Mock.TxEra]
buildMultipleScriptsTxs mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    lockTx <- Conway.mkLockByScriptTx
      (Mock.UTxOIndex 0)
      [ Babbage.TxOutNoInline True
      , Babbage.TxOutNoInline True
      , Babbage.TxOutNoInline True
      ]
      100_000 1_000 state'
    unlockTx <- Conway.mkUnlockScriptTx
      [outputAsPair lockTx 0, outputAsPair lockTx 1]
      (Mock.UTxOIndex 1)
      (Mock.UTxOAddressNew 0)
      True 199_000 1_000 state'
    Right [Mock.TxConway lockTx, Mock.TxConway (withRequiredSigner reqSigner unlockTx)]

{-
-- | Lock a UTxO whose datum is inlined into the output.
buildInlineDatumLockTxs :: MockNode -> IO [Mock.TxEra]
buildInlineDatumLockTxs mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    tx <- Conway.mkLockByScriptTx
      (Mock.UTxOIndex 0)
      [Babbage.TxOutInline True Babbage.InlineDatum Babbage.NoReferenceScript]
      500_000 1_000 state'
    Right [Mock.TxConway tx]

-- | Indefinite-length CBOR encoding of @[1, 2]@. Distinct from the
-- canonical encoding so the round-trip assertion is meaningful.
nonCanonicalDatumBytes :: SBS.ShortByteString
nonCanonicalDatumBytes = SBS.pack [0x9f, 0x01, 0x02, 0xff]

-- | Lock a UTxO whose inline datum carries raw, non-canonical CBOR.
buildInlineDatumCBORLockTxs :: MockNode -> IO [Mock.TxEra]
buildInlineDatumCBORLockTxs mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    tx <- Conway.mkLockByScriptTx
      (Mock.UTxOIndex 0)
      [ Babbage.TxOutInline True
          (Babbage.InlineDatumCBOR nonCanonicalDatumBytes)
          Babbage.NoReferenceScript
      ]
      500_000 1_000 state'
    Right [Mock.TxConway tx]
-}

-- | Unlock a script-locked UTxO with the IsValid flag forced to False
-- and a collateral-return output emitted.
buildFailingUnlockTxs :: MockNode -> IO [Mock.TxEra]
buildFailingUnlockTxs mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    tx <- Conway.mkUnlockScriptTxBabbage
      [Mock.UTxOAddress Scripts.alwaysSucceedsScriptAddr]
      (Mock.UTxOIndex 0)
      (Mock.UTxOAddressNew 0)
      []
      True
      False
      499_000 1_000 state'
    Right [Mock.TxConway (withRequiredSigner reqSigner tx)]

-- | A valid (IsValid=True) script-spending tx that still declares a
-- collateral-return output. Only a valid tx routes its collateral
-- return to @collateral_tx_out@; a failed tx folds it into @tx_out@.
buildValidUnlockWithCollateralReturnTxs :: MockNode -> IO [Mock.TxEra]
buildValidUnlockWithCollateralReturnTxs mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    tx <- Conway.mkUnlockScriptTxBabbage
      [Mock.UTxOAddress Scripts.alwaysSucceedsScriptAddr]
      (Mock.UTxOIndex 0)
      (Mock.UTxOAddressNew 0)
      []
      True
      True
      499_000 1_000 state'
    Right [Mock.TxConway (withRequiredSigner reqSigner tx)]

{-
-- | Lock a UTxO whose output carries the always-succeeds script as a
-- reference script, then spend a separately-locked UTxO that points at
-- the reference output as its witness source.
buildSpendRefScriptTxs :: MockNode -> IO [Mock.TxEra]
buildSpendRefScriptTxs mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    lockTx <- Conway.mkLockByScriptTx
      (Mock.UTxOIndex 0)
      [ Babbage.TxOutNoInline True
      , Babbage.TxOutInline True Babbage.InlineDatum (Babbage.ReferenceScript True)
      ]
      300_000 1_000 state'
    unlockTx <- Conway.mkUnlockScriptTxBabbage
      [outputAsPair lockTx 0]
      (Mock.UTxOIndex 1)
      (Mock.UTxOAddressNew 0)
      [outputAsPair lockTx 1]
      False
      True
      299_000 1_000 state'
    Right [Mock.TxConway lockTx, Mock.TxConway (withRequiredSigner reqSigner unlockTx)]
-}

-- | Mint one token through the always-mint Plutus policy. The policy
-- script is supplied via the witness set.
buildMintTxs :: MockNode -> IO [Mock.TxEra]
buildMintTxs mn =
  Mock.withConwayLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    let policyId  = Mary.PolicyID Scripts.alwaysMintScriptHash
        assetName = Scripts.assetNames !! 0
        minted    = Mary.MultiAsset (Map.singleton policyId (Map.singleton assetName 1))
        outValue  = Mary.MaryValue (Coin 800_000) minted
    tx <- Conway.mkMultiAssetsScriptTx
      [Mock.UTxOIndex 0]
      (Mock.UTxOIndex 1)
      [(Mock.UTxOAddressNew 0, outValue)]
      []
      minted
      True 1_000 state'
    Right [Mock.TxConway tx]


