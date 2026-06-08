{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the off-chain pool worker against a live
-- PostgreSQL test database.
--
-- The worker is driven cycle-by-cycle via 'runOnePoolCycle'. The
-- stub fetcher always returns an HTTP error, so each cycle should
-- write an 'off_chain_pool_fetch_error' row with a bumped
-- 'retry_count'.
module DbSync.Worker.OffChain.PoolSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.OffChainPool
  ( offChainPoolDataTableDef
  , offChainPoolFetchErrorTableDef
  )
import DbSync.Db.Schema.Pool
  ( delistedPoolTableDef
  , poolHashTableDef
  , poolMetadataRefTableDef
  , reservedPoolTickerTableDef
  )
import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef, txTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Test.Database
  ( execTestDb
  , queryTestDb
  , setupFollowTipSchema
  , teardownSchema
  , truncateAllTables
  )
import DbSync.Test.Hasql (withTestConnection)
import DbSync.Trace.Types (AppTracer)
import qualified Control.Tracer as Tracer
import DbSync.Worker.OffChain.Pool (runOnePoolCycle, stubPoolFetcher)

-- The worker needs pool_hash + pool_metadata_ref rows to exist;
-- pool_metadata_ref FKs to tx (registered_tx_id), so include the
-- core tables too. The off-chain result tables are owned by this
-- spec.
tables :: [TableDef]
tables =
  [ blockTableDef
  , slotLeaderTableDef
  , txTableDef
  , poolHashTableDef
  , poolMetadataRefTableDef
  , offChainPoolDataTableDef
  , offChainPoolFetchErrorTableDef
  , delistedPoolTableDef
  , reservedPoolTickerTableDef
  ]

extractorVersions :: [(Text, Int)]
extractorVersions = [("core", 1), ("pool", 1), ("off_chain_pools", 1)]

spec :: Spec
spec = describe "DbSync.Worker.OffChain.Pool" $
  beforeAll_ (setupFollowTipSchema tables extractorVersions) $
  afterAll_  (teardownSchema tables) $
  before_    (truncateAllTables (map tdName tables)) $ do

    describe "runOnePoolCycle with the stub fetcher" $ do
      it "writes off_chain_pool_fetch_error on the first cycle" $ do
        seedPendingPoolRef
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOnePoolCycle tracer conn 10 stubPoolFetcher

        errCount <- countRows offChainPoolFetchErrorTableDef
        errCount `shouldBe` 1

        dataCount <- countRows offChainPoolDataTableDef
        dataCount `shouldBe` 0

      it "records the stub's error message" $ do
        seedPendingPoolRef
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOnePoolCycle tracer conn 10 stubPoolFetcher

        msg <- T.strip <$> queryTestDb
          ( "SELECT fetch_error FROM "
              <> tdName offChainPoolFetchErrorTableDef
              <> " ORDER BY id DESC LIMIT 1;"
          )
        msg `shouldSatisfy` T.isInfixOf "http"
        msg `shouldSatisfy` T.isInfixOf "stub"

      it "starts retry_count at 0" $ do
        seedPendingPoolRef
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOnePoolCycle tracer conn 10 stubPoolFetcher

        rc <- T.strip <$> queryTestDb
          ( "SELECT retry_count FROM "
              <> tdName offChainPoolFetchErrorTableDef
              <> " ORDER BY id DESC LIMIT 1;"
          )
        rc `shouldBe` "0"

      it "bumps retry_count on a subsequent cycle" $ do
        seedPendingPoolRef
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOnePoolCycle tracer conn 10 stubPoolFetcher
          -- second cycle: the prior failure's retry timer fires
          -- immediately because the stub records 'now' for both
          -- the fetch time and the retry time on the first error.
          -- 'retryAgain' from a 0-count baseline schedules the
          -- next attempt 30+2 seconds later, so to exercise the
          -- bump we manually rewind the prior row's fetch_time.
          execTestDb $
            "UPDATE " <> tdName offChainPoolFetchErrorTableDef
              <> " SET fetch_time = NOW() - INTERVAL '7 days';"
          runOnePoolCycle tracer conn 10 stubPoolFetcher

        n <- countRows offChainPoolFetchErrorTableDef
        n `shouldBe` 2

        rc <- T.strip <$> queryTestDb
          ( "SELECT retry_count FROM "
              <> tdName offChainPoolFetchErrorTableDef
              <> " ORDER BY id DESC LIMIT 1;"
          )
        rc `shouldBe` "1"

      it "is a no-op when no pool_metadata_ref rows are pending" $ do
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOnePoolCycle tracer conn 10 stubPoolFetcher

        n <- countRows offChainPoolFetchErrorTableDef
        n `shouldBe` 0

-- ---------------------------------------------------------------------------
-- Seeding
-- ---------------------------------------------------------------------------

-- | Insert one slot_leader → block → tx → pool_hash → pool_metadata_ref
-- chain. Each table is identity-keyed; the worker's work-queue SQL
-- joins them.
seedPendingPoolRef :: IO ()
seedPendingPoolRef = do
  execTestDb $
    "INSERT INTO " <> tdName slotLeaderTableDef
      <> " (hash, description) VALUES "
      <> "(decode('aa', 'hex'), 'test-leader');"
  execTestDb $
    "INSERT INTO " <> tdName blockTableDef
      <> " (hash, epoch_no, slot_no, epoch_slot_no, block_no, previous_id, "
      <> "slot_leader_id, size, time, tx_count, proto_major, proto_minor, "
      <> "vrf_key, op_cert, op_cert_counter) VALUES "
      <> "(decode('bb', 'hex'), 0, 0, 0, 0, NULL, "
      <> "(SELECT id FROM slot_leader LIMIT 1), 0, NOW(), 1, 9, 0, "
      <> "NULL, NULL, NULL);"
  execTestDb $
    "INSERT INTO " <> tdName txTableDef
      <> " (hash, block_id, block_index, out_sum, fee, deposit, size, "
      <> "invalid_before, invalid_hereafter, valid_contract, script_size, "
      <> "treasury_donation) VALUES "
      <> "(decode('cc', 'hex'), (SELECT id FROM block LIMIT 1), 0, 0, 0, 0, "
      <> "0, NULL, NULL, TRUE, 0, 0);"
  execTestDb $
    "INSERT INTO " <> tdName poolHashTableDef
      <> " (hash_raw, view) VALUES "
      <> "(decode('"
      <> T.replicate 28 "a1"
      <> "', 'hex'), 'pool1test');"
  execTestDb $
    "INSERT INTO " <> tdName poolMetadataRefTableDef
      <> " (pool_id, url, hash, registered_tx_id) VALUES "
      <> "((SELECT id FROM pool_hash LIMIT 1), "
      <> "'https://example.test/metadata.json', "
      <> "decode('"
      <> T.replicate 32 "7e"
      <> "', 'hex'), "
      <> "(SELECT id FROM tx LIMIT 1));"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

countRows :: TableDef -> IO Int
countRows td = do
  out <- T.strip <$> queryTestDb ("SELECT COUNT(*) FROM " <> tdName td <> ";")
  case readMaybe (T.unpack out) of
    Just n  -> pure n
    Nothing -> panic ("countRows: bad count " <> out)

silentTracer :: IO AppTracer
silentTracer = pure (Tracer.Tracer $ \_ -> pure ())
