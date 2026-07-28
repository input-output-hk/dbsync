{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Alonzo phase-2 invalid-tx input handling.
--
-- A tx whose Plutus script fails validation is still recorded on-chain,
-- but it consumes its /collateral/ inputs rather than its declared
-- inputs. dbsync must therefore write the collateral inputs into
-- @tx_in@ (and nothing into @collateral_tx_in@) for such a tx.
--
-- The scenario forges one block with two UTxOs locked at the
-- always-fails script and a single failing unlock that spends both
-- (two declared inputs) with one collateral input. Post-fix the invalid
-- tx has exactly one @tx_in@ row (the collateral); the un-fixed parser
-- would have written the two declared inputs instead.
module DbSync.Phase.AlonzoInvalidTxSpec (spec) where

import Cardano.Prelude

import Data.List ((!!))
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe)

import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), Tx (..))
import Cardano.Ledger.BaseTypes (TxIx (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.TxIn (TxIn (..))
import Lens.Micro ((^.))
import Ouroboros.Consensus.Shelley.Eras (AlonzoEra)

import qualified Cardano.Mock.Forging.Interpreter as Mock
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import qualified Cardano.Mock.Forging.Types as Mock

import DbSync.Db.Schema.Core (txTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO (collateralTxInTableDef, txInTableDef)
import DbSync.Test.AppHarness
  ( ledgerEnabledTestConfig
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E (alonzoConfigDir, withAppSession)
import DbSync.Test.MockChain (MockChain (..))
import DbSync.Test.MockNode
  ( MockNode (..)
  , forgeAndPush
  , forgeAndPushBlocks
  , withMockNode
  )

-- ---------------------------------------------------------------------------
-- * Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Alonzo phase-2 invalid tx" $
  it "records collateral (not declared) inputs in tx_in" $
    withMockNode alonzoConfigDir $ \mn ->
      withTempDir "dbsync-test-alonzo-invalid" $ \ledgerDir -> do
        tracer <- quietTracer

        -- Lock + failing unlock in one block, then padding so Ingest
        -- processes them below the volatile tail.
        _ <- forgeAndPush mn =<< buildLockFailUnlock mn
        _ <- forgeAndPushBlocks mn 250

        withAppSession tracer ledgerEnabledTestConfig mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          invalidTxs <- queryText $
            "SELECT COUNT(*) FROM " <> tdName txTableDef <> " WHERE valid_contract = false"
          invalidTxs `shouldBe` "1"

          txInRows <- queryText $
            "SELECT COUNT(*) FROM " <> tdName txInTableDef <> " WHERE tx_in_id = " <> invalidTxIdSql
          txInRows `shouldBe` "1"

          collRows <- queryText $
            "SELECT COUNT(*) FROM " <> tdName collateralTxInTableDef <> " WHERE tx_in_id = " <> invalidTxIdSql
          collRows `shouldBe` "0"

-- The most recent phase-2 invalid tx.
invalidTxIdSql :: Text
invalidTxIdSql =
  "(SELECT id FROM " <> tdName txTableDef <> " WHERE valid_contract = false ORDER BY id DESC LIMIT 1)"

queryText :: Text -> IO Text
queryText = fmap T.strip . queryTestDb

-- ---------------------------------------------------------------------------
-- * Tx builders
-- ---------------------------------------------------------------------------

-- | Two always-fails locks (funded from separate genesis UTxOs so they
-- can share a block) and a failing unlock spending both, with genesis
-- UTxO 1 as collateral.
buildLockFailUnlock :: MockNode -> IO [Mock.TxEra]
buildLockFailUnlock mn =
  Mock.withAlonzoLedgerState (mcInterpreter (mnChain mn)) $ \state' -> do
    lockTx1 <- Alonzo.mkLockByScriptTx (Mock.UTxOIndex 0) [False] 200_000 1_000 state'
    lockTx2 <- Alonzo.mkLockByScriptTx (Mock.UTxOIndex 2) [False] 200_000 1_000 state'
    unlockTx <-
      Alonzo.mkUnlockScriptTx
        [outputAsPair lockTx1 0, outputAsPair lockTx2 0]
        (Mock.UTxOIndex 1)
        (Mock.UTxOAddressNew 0)
        False
        399_000
        1_000
        state'
    Right [Mock.TxAlonzo lockTx1, Mock.TxAlonzo lockTx2, Mock.TxAlonzo unlockTx]

-- | Reference an output of a forged Alonzo tx as a 'UTxOPair' so a
-- later tx in the same block can spend it. Alonzo outputs are unsized,
-- so the lens yields the 'TxOut' directly.
outputAsPair :: Core.Tx Core.TopTx AlonzoEra -> Int -> Mock.UTxOIndex AlonzoEra
outputAsPair (MkAlonzoTx atx) ix =
  let body = atBody atx
      txId = Core.txIdTxBody body
      out = toList (body ^. Core.outputsTxBodyL) !! ix
   in Mock.UTxOPair (TxIn txId (TxIx (fromIntegral ix)), out)
