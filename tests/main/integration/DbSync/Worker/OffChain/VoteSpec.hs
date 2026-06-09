{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the off-chain vote worker against a live
-- PostgreSQL test database.
--
-- The worker is driven cycle-by-cycle via 'runOneVoteCycle'. The
-- stub fetcher always returns an HTTP error, so each cycle should
-- write an 'off_chain_vote_fetch_error' row with a bumped
-- 'retry_count'.
module DbSync.Worker.OffChain.VoteSpec (spec) where

import Cardano.Prelude

import qualified Data.Text as T

import Test.Hspec (Spec, afterAll_, beforeAll_, before_, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Core (blockTableDef, slotLeaderTableDef)
import DbSync.Db.Schema.Governance (votingAnchorTableDef)
import DbSync.Db.Schema.OffChainVote
  ( offChainVoteDataTableDef
  , offChainVoteFetchErrorTableDef
  )
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
import DbSync.Worker.OffChain.Vote (runOneVoteCycle, stubVoteFetcher)

-- The worker needs voting_anchor rows; voting_anchor FKs to block,
-- which FKs to slot_leader. The off-chain result tables are owned
-- by this spec.
tables :: [TableDef]
tables =
  [ blockTableDef
  , slotLeaderTableDef
  , votingAnchorTableDef
  , offChainVoteDataTableDef
  , offChainVoteFetchErrorTableDef
  ]

extractorVersions :: [(Text, Int)]
extractorVersions = [("core", 1), ("governance", 1), ("off_chain_votes", 1)]

spec :: Spec
spec = describe "DbSync.Worker.OffChain.Vote" $
  beforeAll_ (setupFollowTipSchema tables extractorVersions) $
  afterAll_  (teardownSchema tables) $
  before_    (truncateAllTables (map tdName tables)) $ do

    describe "runOneVoteCycle with the stub fetcher" $ do
      it "writes off_chain_vote_fetch_error on the first cycle" $ do
        seedPendingVoteAnchor "gov_action"
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOneVoteCycle tracer conn 10 stubVoteFetcher

        errCount <- countRows offChainVoteFetchErrorTableDef
        errCount `shouldBe` 1

        dataCount <- countRows offChainVoteDataTableDef
        dataCount `shouldBe` 0

      it "records the stub's error message" $ do
        seedPendingVoteAnchor "vote"
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOneVoteCycle tracer conn 10 stubVoteFetcher

        msg <- T.strip <$> queryTestDb
          ( "SELECT fetch_error FROM "
              <> tdName offChainVoteFetchErrorTableDef
              <> " ORDER BY id DESC LIMIT 1;"
          )
        msg `shouldSatisfy` T.isInfixOf "http"
        msg `shouldSatisfy` T.isInfixOf "stub"

      it "starts retry_count at 0" $ do
        seedPendingVoteAnchor "drep"
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOneVoteCycle tracer conn 10 stubVoteFetcher

        rc <- T.strip <$> queryTestDb
          ( "SELECT retry_count FROM "
              <> tdName offChainVoteFetchErrorTableDef
              <> " ORDER BY id DESC LIMIT 1;"
          )
        rc `shouldBe` "0"

      it "bumps retry_count on a subsequent cycle" $ do
        seedPendingVoteAnchor "gov_action"
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOneVoteCycle tracer conn 10 stubVoteFetcher
          -- Rewind the failure so the backoff schedule fires
          -- immediately on the second cycle.
          execTestDb $
            "UPDATE " <> tdName offChainVoteFetchErrorTableDef
              <> " SET fetch_time = NOW() - INTERVAL '7 days';"
          runOneVoteCycle tracer conn 10 stubVoteFetcher

        n <- countRows offChainVoteFetchErrorTableDef
        n `shouldBe` 2

        rc <- T.strip <$> queryTestDb
          ( "SELECT retry_count FROM "
              <> tdName offChainVoteFetchErrorTableDef
              <> " ORDER BY id DESC LIMIT 1;"
          )
        rc `shouldBe` "1"

      it "skips constitution anchors" $ do
        seedPendingVoteAnchor "constitution"
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOneVoteCycle tracer conn 10 stubVoteFetcher

        n <- countRows offChainVoteFetchErrorTableDef
        n `shouldBe` 0

      it "is a no-op when no voting_anchor rows are pending" $ do
        withTestConnection $ \conn -> do
          tracer <- silentTracer
          runOneVoteCycle tracer conn 10 stubVoteFetcher

        n <- countRows offChainVoteFetchErrorTableDef
        n `shouldBe` 0

-- ---------------------------------------------------------------------------
-- Seeding
-- ---------------------------------------------------------------------------

-- | Insert one slot_leader -> block -> voting_anchor chain with the
-- given anchor @type@. The worker queries voting_anchor for rows
-- lacking a result.
seedPendingVoteAnchor :: Text -> IO ()
seedPendingVoteAnchor anchorType = do
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
      <> "(SELECT id FROM slot_leader LIMIT 1), 0, NOW(), 0, 9, 0, "
      <> "NULL, NULL, NULL);"
  execTestDb $
    "INSERT INTO " <> tdName votingAnchorTableDef
      <> " (url, data_hash, type, block_id) VALUES "
      <> "('https://example.test/anchor.json', "
      <> "decode('" <> T.replicate 32 "5a" <> "', 'hex'), "
      <> "'" <> anchorType <> "', "
      <> "(SELECT id FROM block LIMIT 1));"

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
