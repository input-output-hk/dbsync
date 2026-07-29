{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Re-delivery safety across the Ingest → Follow handoff.
--
-- The handoff ends one chainsync session (the Ingest receiver is
-- cancelled with an async exception) and starts another that
-- re-intersects at the receiver's latest received point. If that
-- point ever lags the blocks already queued — the pre-atomic-delivery
-- bug — the node re-sends a block the consumer also has buffered,
-- historically a fatal @tx@ unique-constraint abort; the @block@
-- table (no unique constraint) would accept the duplicate row
-- silently. This spec drives the full app through the handoff with
-- tx-bearing blocks forged while Ingest and Prep are still running,
-- so the volatile tail is genuinely buffered and carries tx rows
-- that a double-apply would duplicate, then asserts the invariants
-- the schema cannot enforce on its own: no duplicate block or tx
-- rows, and the consumer's re-delivery guard stayed silent.
module DbSync.Phase.HandoffRedeliverySpec (spec) where

import Cardano.Prelude

import Data.IORef (newIORef, readIORef)
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Core (blockTableDef, txTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Trace.Backend (mkTestTracer)
import DbSync.Trace.Types (AppTracer, LogMsg (..))
import DbSync.Test.AppHarness
  ( ledgerEnabledTestConfig
  , waitForSyncComplete
  , withTempDir
  )
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.E2E
  ( conwayConfigDir
  , forgeAndWaitForBlocks
  , withAppSession
  )
import DbSync.Test.MockNode
  ( forgeAndPushBlocks
  , forgeAndPushBlocksWith
  , withMockNode
  )
import DbSync.Test.MockNode.Workload (mainnetLikeWorkload)
import DbSync.Test.PgAssertions (countRows, readInt)

spec :: Spec
spec = describe "Ingest \x2192 Follow handoff re-delivery safety" $
  it "hands off under a live block stream without duplicating rows" $
    withMockNode conwayConfigDir $ \mn ->
      withTempDir "dbsync-test-handoff-redelivery" $ \ledgerDir -> do
        -- Enough history for Ingest to run to the rollback boundary
        -- (k=10) and cross at least one 500-slot epoch.
        _ <- forgeAndPushBlocks mn 150

        logs <- newIORef []
        let tracer = mkTestTracer logs :: AppTracer

        withAppSession tracer ledgerEnabledTestConfig mn ledgerDir $ \_ -> do
          -- Forge while Ingest and Prep are still running so the
          -- handoff starts with a genuinely buffered volatile tail —
          -- the production shape in which the re-delivery bug fired.
          -- The blocks carry payment txs so a double-applied block
          -- has tx rows to duplicate; empty blocks would make the
          -- tx-duplication check below pass vacuously.
          _ <- forgeAndPushBlocksWith mn 30 mainnetLikeWorkload
          waitForSyncComplete 120

          baseline <- countRows (tdName blockTableDef)
          baseline `shouldSatisfy` (>= 150)

          -- Every workload tx must have landed exactly once: 30
          -- blocks times 10 payment txs. Genesis-distribution txs sit
          -- on the synthetic slotless genesis block, so the join
          -- filters them out. This both guards the dup check against
          -- vacuity and catches a lost/duplicated tx outright.
          txCount <- readInt
            ( "SELECT count(*) FROM " <> tdName txTableDef
                <> " JOIN " <> tdName blockTableDef
                <> " ON " <> tdName txTableDef <> ".block_id = "
                <> tdName blockTableDef <> ".id"
                <> " WHERE " <> tdName blockTableDef <> ".slot_no IS NOT NULL;"
            )
          txCount `shouldBe` 300

          -- Volatile blocks through the freshly handed-off Follow
          -- loop, past the tail buffered at the handoff.
          forgeAndWaitForBlocks mn 15 (baseline + 15) 60

          -- No duplicate rows. A double-applied block dies loudly on
          -- the tx unique index, but the block table has no unique
          -- constraint — a duplicate there is only visible by asking.
          dupBlocks <- duplicateHashCount (tdName blockTableDef)
          dupTxs    <- duplicateHashCount (tdName txTableDef)
          dupBlocks `shouldBe` "0"
          dupTxs    `shouldBe` "0"

          -- With atomic receiver-side delivery nothing is ever
          -- re-sent in the first place, so the consumer's guard must
          -- have stayed silent.
          msgs <- readIORef logs
          let drops = filter
                (T.isPrefixOf "dropping re-delivered block" . lmMessage)
                msgs
          map lmMessage drops `shouldBe` []

-- | Number of distinct @hash@ values with more than one row, as text
-- straight from psql.
duplicateHashCount :: Text -> IO Text
duplicateHashCount table =
  T.strip <$> queryTestDb
    ( "SELECT count(*) FROM (SELECT hash FROM " <> table
        <> " GROUP BY hash HAVING count(*) > 1) AS dups;"
    )
