{-# LANGUAGE OverloadedStrings #-}

-- | Pure (no DB) tests for the @dbsync_sync_state@ schema: the golden
-- CREATE TABLE DDL and the counter-column cross-check that the resume
-- cleanup depends on.
module DbSync.Schema.SyncStateSpec (spec) where

import Cardano.Prelude

import Data.Algorithm.Diff (Diff, PolyDiff (..), getDiff)
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe)

import DbSync.Db.Schema.Generate (generateCreateTable)
import DbSync.Db.Schema.SyncState
  ( idCounterByTable
  , syncStateTableDef
  )
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , TableDef (..)
  )

spec :: Spec
spec = describe "DbSync.Db.Schema.SyncState" $ do

  describe "idCounterByTable" $
    -- The table-name half of 'idCounterByTable' drives the resume
    -- cleanup's counter pass. Adding a counter column without a
    -- matching entry leaves rows past the recorded id on the next
    -- boot — the exact bug the cleanup exists to fix. Both
    -- directions are pinned so renaming a table without updating
    -- the counter list also fails.
    it "covers every _id_counter column on the table" $ do
      let tableCounters =
            filter (T.isSuffixOf "_id_counter")
              (map cdName (tdColumns syncStateTableDef))
          derived = map ((<> "_id_counter") . fst) idCounterByTable
      derived `shouldBe` tableCounters

  describe "generateCreateTable syncStateTableDef" $
    -- Line-by-line so a mismatch prints only the differing lines.
    it "matches the golden DDL line-by-line" $
      diffLines (generateCreateTable syncStateTableDef) goldenDdl `shouldBe` []

-- | The expected CREATE TABLE output. Updating this string is the
-- canonical way to change the on-disk schema: edit 'syncStateTableDef',
-- rerun this test, and copy the generated DDL here. Any drift between
-- the two is a failure — the sync-state schema is load-bearing for
-- crash recovery.
goldenDdl :: Text
goldenDdl = T.unlines
  [ "CREATE TABLE \"dbsync_sync_state\" ("
  , "  \"id\" SMALLINT NOT NULL DEFAULT 1,"
  , "  \"last_committed_slot\" BIGINT,"
  , "  \"last_committed_block_no\" BIGINT,"
  , "  \"last_committed_block_hash\" BYTEA,"
  , "  \"last_snapshot_slot\" BIGINT,"
  , "  \"block_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"tx_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"tx_out_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"slot_leader_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"address_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"stake_address_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"pool_hash_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"multi_asset_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"script_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"pool_update_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"pool_metadata_ref_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"cost_model_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"redeemer_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"collateral_tx_out_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"epoch_sync_stats_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"gov_action_proposal_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"param_proposal_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"committee_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"constitution_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"event_info_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"schema_version_applied\" INTEGER NOT NULL,"
  , "  \"ledger_enabled\" BOOLEAN NOT NULL,"
  , "  \"sync_complete\" BOOLEAN NOT NULL DEFAULT false,"
  , "  \"pending_rollback_slot\" BIGINT,"
  , "  \"schema_fingerprint\" TEXT NOT NULL,"
  , "  \"extractors\" TEXT[] NOT NULL,"
  , "  \"updated_at\" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),"
  , "  PRIMARY KEY (\"id\"),"
  , "  CHECK (\"id\" = 1)"
  , ");"
  ]

-- | Line-level diff. Drops common lines so a mismatch failure
-- prints only the differing ones.
diffLines :: Text -> Text -> [Diff Text]
diffLines actual expected =
  filter notCommon (getDiff (T.lines actual) (T.lines expected))
  where
    notCommon (Both _ _) = False
    notCommon _          = True
