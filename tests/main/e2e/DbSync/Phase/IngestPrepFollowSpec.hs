{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Drive 'IngestChainHistory' → 'PreparingForVolatileTail' →
-- 'FollowingChainTip' through 'runApp' against the mock chainsync
-- server.
--
-- Forges enough empty blocks past the Conway test config's @k=10@
-- security parameter so the receiver sees a non-Nothing rollback
-- boundary, runs 'runApp' in a background async, waits for Prep to
-- mark @sync_complete=true@, then forges more blocks and asserts the
-- in-process Follow path inserts them.
module DbSync.Phase.IngestPrepFollowSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)

import DbSync.Test.AppHarness
  ( defaultTestProfile
  , profileExpectedIndexes
  , profileTableNames
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E
  ( conwayConfigDir
  , forgeAndWaitForBlocks
  , withAppSession
  )
import DbSync.Test.MockNode (forgeAndPushBlocks, withMockNode)
import DbSync.Test.PgAssertions
  ( countNonLoggedTables
  , countNulls
  , countRows
  , listMissingIndexes
  , readBlockMax
  , readSyncStateLast
  , sequenceAdvanced
  , waitForSchemaSettled
  )

spec :: Spec
spec = describe "IngestChainHistory \x2192 PreparingForVolatileTail \x2192 FollowingChainTip" $ do

  it "ingests forged blocks, completes Prep, and lands volatile blocks via Follow" $ do
    -- runApp wipes the schema itself via aaResyncFromGenesis=True;
    -- no need to drop the whole database around the test.
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-ledger" $ \ledgerDir -> do

        -- Forge well past k=10 (so the rollback boundary is
        -- non-empty) and past the Conway test config's epoch length
        -- of 500 slots (so the consumer crosses an epoch boundary
        -- and writes sync_state.last_committed_slot at least once).
        -- 'activeSlotsCoeff=0.2' = ~5 slots per forged block, so
        -- 150 blocks puts us well past slot 500.
        _ <- forgeAndPushBlocks mn 150

        -- Swap to 'verboseTracer Info' (or Debug) here when diagnosing.
        tracer <- quietTracer
        withAppSession tracer defaultTestProfile mn ledgerDir $ \_ -> do
          waitForSyncComplete 60

          syncComplete <-
            T.strip <$> queryTestDb
              "SELECT sync_complete FROM dbsync_sync_state LIMIT 1"
          syncComplete `shouldBe` "t"

          let expectedTables  = profileTableNames defaultTestProfile
              expectedIndexes = profileExpectedIndexes defaultTestProfile
          length expectedTables `shouldSatisfy` (> 20)

          -- The flip and index build commit asynchronously across a
          -- pool of backends, so even after @sync_complete=true@ the
          -- catalog updates can take a few hundred ms to be visible
          -- to a fresh psql connection. Settle first; then run the
          -- strict-equality asserts.
          waitForSchemaSettled expectedTables expectedIndexes 10

          nonLogged <- countNonLoggedTables expectedTables
          nonLogged `shouldBe` 0

          missingIdx <- listMissingIndexes expectedIndexes
          missingIdx `shouldBe` []

          -- FK-resolution backfills left no NULLs on the dependent
          -- columns. The Conway test chain produces no real inputs
          -- (genesis seed only) but the asserts still guard against
          -- future schema regressions.
          txInNulls  <- countNulls "tx_in"            "tx_out_id"
          colInNulls <- countNulls "collateral_tx_in" "tx_out_id"
          refInNulls <- countNulls "reference_tx_in"  "tx_out_id"
          txInNulls  `shouldBe` 0
          colInNulls `shouldBe` 0
          refInNulls `shouldBe` 0

          -- Every LOGGED table with a PK has its id sequence advanced
          -- to MAX(id) + 1 (or 1 on an empty table).
          forM_ pkSequenceTables $ \(table, seqName) -> do
            ok <- sequenceAdvanced table seqName
            ok `shouldBe` True

          -- sync_state has been populated and never sits ahead of the
          -- block table. Strict equality doesn't hold yet because
          -- Ingest commits sync_state at epoch boundaries with the
          -- /previous/ epoch's last block (the address-resolver lags
          -- by one epoch); the block table already contains rows past
          -- 'last_committed_slot' until Follow's per-block commits
          -- close the gap.
          (lastSlot, lastBlockNo) <- readSyncStateLast
          lastSlot    `shouldNotBe` Nothing
          lastBlockNo `shouldNotBe` Nothing
          (maxSlot, maxBlockNo) <- readBlockMax
          maxSlot    `shouldSatisfy` (>= lastSlot)
          maxBlockNo `shouldSatisfy` (>= lastBlockNo)

          -- Snapshot Ingest-era counts so the post-Follow assertions
          -- have a stable baseline.
          ingestBlock      <- countRows "block"
          ingestTx         <- countRows "tx"
          ingestTxOut      <- countRows "tx_out"
          ingestSlotLeader <- countRows "slot_leader"
          ingestBlock `shouldSatisfy` (>= 140)

          -- Forge more blocks. Follow's in-process receiver picks
          -- them up over the volatile path and inserts them per-block.
          let extraBlocks   = 20
              expectedTotal = ingestBlock + extraBlocks
          forgeAndWaitForBlocks mn extraBlocks expectedTotal 30

          -- Every Ingest-side count survives Follow. Block and
          -- slot-leader strictly grow; tx / tx_out at minimum hold
          -- steady because the Conway test chain forges empty blocks
          -- (no tx → no tx_out).
          followBlock      <- countRows "block"
          followTx         <- countRows "tx"
          followTxOut      <- countRows "tx_out"
          followSlotLeader <- countRows "slot_leader"
          followBlock      `shouldSatisfy` (> ingestBlock)
          followSlotLeader `shouldSatisfy` (>= ingestSlotLeader)
          followTx         `shouldSatisfy` (>= ingestTx)
          followTxOut      `shouldSatisfy` (>= ingestTxOut)

          -- With no new blocks in flight (mock node is quiescent at
          -- 'expectedTotal') Follow's per-block commits have brought
          -- sync_state up to the latest block.
          (followLastSlot, followLastBlock) <- readSyncStateLast
          (followMaxSlot,  followMaxBlock)  <- readBlockMax
          followLastSlot  `shouldBe` followMaxSlot
          followLastBlock `shouldBe` followMaxBlock

-- | Tables whose PK gets an @<table>_id_seq@ attached during Prep's
-- LOGGED flip, paired with the sequence name. Narrow on purpose: the
-- assertion checks both that the sequence advanced on populated
-- tables and that it sits at 1 on empty ones, so the list is limited
-- to tables the Conway test chain reliably populates.
pkSequenceTables :: [(Text, Text)]
pkSequenceTables =
  [ ("block",       "block_id_seq")
  , ("tx",          "tx_id_seq")
  , ("tx_out",      "tx_out_id_seq")
  , ("tx_in",       "tx_in_id_seq")
  , ("slot_leader", "slot_leader_id_seq")
  ]
