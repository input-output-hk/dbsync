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
import qualified Data.Set as Set
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Cardano.Ledger.Conway.Tx (AlonzoTx (..), Tx (..))
import Cardano.Ledger.Conway.TxBody (TxBody (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness), coerceKeyRole)
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)

import qualified Cardano.Mock.Forging.Interpreter as Mock
import qualified Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples as Scripts
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Generic as Generic
import qualified Cardano.Mock.Forging.Types as Mock

import DbSync.App.Config.Types
  ( SyncConfig (..)
  , SyncOption (..)
  , SyncOptions (..)
  )
import DbSync.Db.Schema.Core (blockTableDef)
import DbSync.Db.Schema.ScriptsDatums
  ( datumTableDef
  , extraKeyWitnessTableDef
  , redeemerDataTableDef
  , redeemerTableDef
  , scriptTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))
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
spec = describe "Follow scripts/datums writes" $
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

-- ---------------------------------------------------------------------------
-- * Profile
-- ---------------------------------------------------------------------------

-- | 'ledgerEnabledTestProfile' with @pcScriptsDatums@ flipped on.
scriptsDatumsTestProfile :: SyncConfig
scriptsDatumsTestProfile =
  ledgerEnabledTestProfile
    { scOptions = (scOptions ledgerEnabledTestProfile)
        { pcScriptsDatums = SyncOption True
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
  where
    reqSigner = Generic.unregisteredWitnessKey !! 0

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
