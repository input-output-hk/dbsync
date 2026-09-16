{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end behaviour of @utxo.strategy: "from_ledger"@.
--
-- During catchup no @tx_out@ rows are written; at the end of Ingest
-- the live UTxO set is bulk-loaded from the pinned ledger state. The
-- spec drives tx-bearing blocks through the whole pipeline and then
-- asserts the strategy's observable contract: every output spent
-- before the handoff has no row at all, every loaded row has its
-- @address_id@ backfilled, and Follow keeps writing outputs normally
-- after the handoff.
module DbSync.Phase.UtxoFromLedgerSpec (spec) where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.App.Config.Types
  ( Extractors (..)
  , SyncConfig (..)
  , UtxoOption (..)
  , UtxoStrategy (..)
  )
import DbSync.Db.Schema.UTxO (txInTableDef, txOutTableDef)
import DbSync.Db.Schema.Core (txTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestConfig
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.E2E (conwayConfigDir, withAppSession)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockNode (forgeAndPushBlocksWith, withMockNode)
import DbSync.Test.MockNode.Workload (mainnetLikeWorkload)
import DbSync.Test.PgAssertions (countNulls, countRows, readInt)

-- | 'ledgerEnabledTestConfig' with the utxo strategy flipped to
-- from_ledger.
fromLedgerConfig :: SyncConfig
fromLedgerConfig = ledgerEnabledTestConfig
  { scExtractors = (scExtractors ledgerEnabledTestConfig)
      { exUtxo = (exUtxo (scExtractors ledgerEnabledTestConfig))
          { uoStrategy = StrategyFromLedger }
      }
  }

spec :: Spec
spec = describe "utxo.strategy from_ledger" $
  it "loads only unspent outputs at handoff and writes normally in Follow" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-utxo-from-ledger" $ \ledgerDir -> do
        tracer <- quietTracer

        -- Payment-tx blocks: each tx spends one UTxO, so the chain
        -- accumulates spends the strategy must leave rowless. 150
        -- blocks cross at least one epoch boundary.
        _ <- forgeAndPushBlocksWith mn 150 mainnetLikeWorkload

        withAppSession tracer fromLedgerConfig mn ledgerDir $ \_ -> do
          waitForSyncComplete 120

          -- Spends really happened, or the checks below pass vacuously.
          txIns <- countRows (tdName txInTableDef)
          txIns `shouldSatisfy` (> 0)

          -- The load wrote the live set.
          loaded <- countRows (tdName txOutTableDef)
          loaded `shouldSatisfy` (> 0)

          -- The strategy's core contract: an output known to be spent
          -- either has no tx_out row (spent before the handoff) or was
          -- spent by Follow, which stamps consumed_by in the same
          -- block transaction. A spent row with NULL consumed_by means
          -- the load wrote an already-spent output.
          spentUnstamped <- readInt
            ( "SELECT count(*) FROM " <> tdName txOutTableDef
                <> " JOIN " <> tdName txTableDef
                <> " ON " <> tdName txTableDef <> ".id = "
                <> tdName txOutTableDef <> ".tx_id"
                <> " JOIN " <> tdName txInTableDef
                <> " ON " <> tdName txInTableDef <> ".tx_out_hash = "
                <> tdName txTableDef <> ".hash"
                <> " AND " <> tdName txInTableDef <> ".tx_out_index = "
                <> tdName txOutTableDef <> ".index"
                <> " WHERE " <> tdName txOutTableDef
                <> ".consumed_by_tx_id IS NULL;"
            )
          spentUnstamped `shouldBe` 0

          -- The chunked address jobs covered every loaded row.
          addrNulls <- countNulls (tdName txOutTableDef) "address_id"
          addrNulls `shouldBe` 0

          -- Follow must write outputs normally after the handoff; the
          -- catchup-only writer silencing must not leak past Ingest.
          baseline <- countRows (tdName txOutTableDef)
          _ <- forgeAndPushBlocksWith mn 5 mainnetLikeWorkload
          waitFor
            (tdName txOutTableDef <> " grows in Follow")
            (do n <- countRows (tdName txOutTableDef); pure (n > baseline))
            60
