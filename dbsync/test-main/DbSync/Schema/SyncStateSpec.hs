{-# LANGUAGE OverloadedStrings #-}

-- | Pure (no DB) tests for the @dbsync_sync_state@ schema definition.
--
-- These tests lock the DDL shape against a golden string and verify
-- the derived column helpers stay in sync with the 'TableDef'.
-- Anything that changes the on-disk schema layout will break one
-- of these tests loudly — which is what we want for a schema that
-- every ingestion commit depends on.
module DbSync.Schema.SyncStateSpec (spec) where

import Cardano.Prelude

import Data.List (last, lookup)
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import DbSync.Db.Schema.Generate (generateCreateTable)
import DbSync.Db.Schema.SyncState
  ( syncStateColumns
  , syncStateCounterColumns
  , syncStateTableDef
  , syncStateTableName
  )
import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , TableDef (..)
  , TableMode (..)
  )

spec :: Spec
spec = describe "DbSync.Db.Schema.SyncState" $ do

  describe "syncStateTableName" $
    it "is dbsync_sync_state" $
      syncStateTableName `shouldBe` "dbsync_sync_state"

  describe "syncStateTableDef shape" $ do
    it "uses the singleton table name" $
      tdName syncStateTableDef `shouldBe` syncStateTableName

    it "is LOGGED from day one (no UNLOGGED promotion dance)" $
      tdMode syncStateTableDef `shouldBe` TableLogged

    it "has a single-column primary key on id" $
      tdPrimaryKey syncStateTableDef `shouldBe` Just ["id"]

    it "enforces the single-row CHECK constraint" $
      tdChecks syncStateTableDef `shouldBe` [ "\"id\" = 1" ]

    it "counter columns all default to 1" $ do
      let defaults = tdColumnDefaults syncStateTableDef
      forM_ syncStateCounterColumns $ \col ->
        lookup col defaults `shouldBe` Just "1"

    it "id column defaults to 1" $
      lookup "id" (tdColumnDefaults syncStateTableDef) `shouldBe` Just "1"

    it "updated_at defaults to now()" $
      lookup "updated_at" (tdColumnDefaults syncStateTableDef) `shouldBe` Just "now()"

    it "last_committed_* and last_snapshot_slot columns are nullable" $ do
      let nullableByName =
            [ (cdName col, cdNullable col)
            | col <- tdColumns syncStateTableDef
            , cdName col `elem`
                [ "last_committed_slot"
                , "last_committed_block_no"
                , "last_committed_block_hash"
                , "last_snapshot_slot"
                ]
            ]
      nullableByName `shouldBe`
        [ ("last_committed_slot", True)
        , ("last_committed_block_no", True)
        , ("last_committed_block_hash", True)
        , ("last_snapshot_slot", True)
        ]

  describe "syncStateColumns" $ do
    it "matches the table's column order" $
      syncStateColumns `shouldBe` map cdName (tdColumns syncStateTableDef)

    it "contains 35 columns (id + 3 last_committed + last_snapshot_slot + 26 counters + 4 metadata)" $
      length syncStateColumns `shouldBe` 35

    it "starts with id" $
      head syncStateColumns `shouldBe` Just "id"

    it "ends with updated_at" $
      last syncStateColumns `shouldBe` "updated_at"

  describe "syncStateCounterColumns" $ do
    it "lists 26 counters — one per current IdCounters field" $
      length syncStateCounterColumns `shouldBe` 26

    it "is a subset of syncStateColumns" $
      all (`elem` syncStateColumns) syncStateCounterColumns `shouldBe` True

  describe "generateCreateTable syncStateTableDef" $ do
    let ddl = generateCreateTable syncStateTableDef

    it "produces a LOGGED CREATE TABLE (no UNLOGGED keyword)" $ do
      ddl `shouldSatisfy` T.isInfixOf "CREATE TABLE"
      ddl `shouldSatisfy` (not . T.isInfixOf "UNLOGGED")

    it "includes the table name" $
      ddl `shouldSatisfy` T.isInfixOf "\"dbsync_sync_state\""

    it "emits the PRIMARY KEY (id) clause" $
      ddl `shouldSatisfy` T.isInfixOf "PRIMARY KEY (\"id\")"

    it "emits the CHECK (id = 1) constraint" $
      ddl `shouldSatisfy` T.isInfixOf "CHECK (\"id\" = 1)"

    it "sets DEFAULT 1 on counter columns" $
      ddl `shouldSatisfy` T.isInfixOf "\"block_id_counter\" BIGINT NOT NULL DEFAULT 1"

    it "sets DEFAULT now() on updated_at" $
      ddl `shouldSatisfy` T.isInfixOf
        "\"updated_at\" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()"

    it "makes last_committed_slot nullable (no NOT NULL)" $ do
      let slotLine =
            T.unlines $
              filter (T.isInfixOf "last_committed_slot") (T.lines ddl)
      slotLine `shouldSatisfy` (not . T.isInfixOf "NOT NULL")

    it "matches the golden DDL byte-for-byte" $
      ddl `shouldBe` goldenDdl

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
  , "  \"tx_in_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"collateral_tx_in_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"reference_tx_in_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"tx_metadata_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"ma_tx_mint_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"ma_tx_out_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"slot_leader_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"stake_address_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"pool_hash_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"multi_asset_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"script_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"stake_registration_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"stake_deregistration_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"delegation_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"withdrawal_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"pool_update_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"pool_metadata_ref_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"pool_owner_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"pool_retire_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"pool_relay_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"tx_cbor_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"epoch_sync_stats_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"ada_pots_id_counter\" BIGINT NOT NULL DEFAULT 1,"
  , "  \"schema_version_applied\" INTEGER NOT NULL,"
  , "  \"ledger_enabled\" BOOLEAN NOT NULL,"
  , "  \"sync_complete\" BOOLEAN NOT NULL DEFAULT false,"
  , "  \"updated_at\" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),"
  , "  PRIMARY KEY (\"id\"),"
  , "  CHECK (\"id\" = 1)"
  , ");"
  ]
