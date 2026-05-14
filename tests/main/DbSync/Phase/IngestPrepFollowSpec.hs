{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end: drive Ingest → Prep transition through 'runApp'
-- against the mock chain-sync server.
--
-- Forges enough empty blocks past the Conway test config's @k=10@
-- security parameter to make the receiver compute a non-Nothing
-- rollback boundary, then waits for 'runApp' to:
--
--   1. Connect to the mock socket and ingest the forged blocks.
--   2. Exit Ingest cleanly when the consumer reaches the rollback
--      boundary.
--   3. Run 'PreparingForChainTip' against the UNLOGGED tables.
--   4. Flip @dbsync_sync_state.sync_complete@ to true and return.
--
-- Asserts schema is LOGGED, sync_complete is set, and the consumer's
-- last-committed slot lines up with a forged block.
module DbSync.Phase.IngestPrepFollowSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T
import System.Timeout (timeout)

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)

import DbSync.App.Run (runApp)
import DbSync.Test.AppHarness
  ( defaultTestProfile
  , mkAppArgsFromMockNode
  , quietTracer
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.MockNode
  ( forgeAndPushBlocks
  , withMockNode
  )

spec :: Spec
spec = describe "Ingest \x2192 Prep" $

  it "boots, ingests forged blocks, runs Prep, marks sync_complete" $ do
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

        let args = mkAppArgsFromMockNode defaultTestProfile mn ledgerDir Nothing

        -- 'runApp' returns once Prep completes and sync_state is
        -- flipped. Gated with a 60s safety timeout so a stall
        -- surfaces as a test failure, not a hung CI job. Swap to
        -- 'verboseTracer Info' (or Debug) here when diagnosing.
        tracer <- quietTracer
        result <- timeout 60_000_000 (runApp tracer args)
        case result of
          Just () -> pure ()
          Nothing -> panic "runApp did not return within 60s"

        -- After 'runApp' returns the sync_state must be flipped.
        waitForSyncComplete 5

        syncComplete <-
          T.strip <$> queryTestDb
            "SELECT sync_complete FROM dbsync_sync_state LIMIT 1"
        syncComplete `shouldBe` "t"

        -- All extractor tables are LOGGED after Prep's schema flip.
        nonLogged <-
          T.strip <$> queryTestDb
            "SELECT count(*) FROM pg_class \
            \WHERE relkind = 'r' \
            \AND relname IN ('block', 'tx', 'tx_out', 'tx_in', 'slot_leader') \
            \AND relpersistence <> 'p';"
        nonLogged `shouldBe` "0"

        -- The block table should hold at least the 15 blocks below
        -- the rollback boundary (tip - k = 25 - 10 = 15).
        blockCount <-
          T.strip <$> queryTestDb "SELECT count(*) FROM block;"
        blockCount `shouldSatisfy` \n -> readNumber n >= 15

        -- Sanity: last_committed_slot is populated (Ingest reached
        -- at least one epoch boundary, which is what writes the
        -- sync_state row).
        slot <-
          T.strip <$> queryTestDb
            "SELECT last_committed_slot FROM dbsync_sync_state LIMIT 1;"
        slot `shouldNotBe` ""

  where
    readNumber :: Text -> Int
    readNumber t = fromMaybe 0 (readMaybe (T.unpack t))

conwayConfigDir :: FilePath
conwayConfigDir = "data/config-conway"
