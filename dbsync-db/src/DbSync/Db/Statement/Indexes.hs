{-# LANGUAGE OverloadedStrings #-}

-- | DDL builders for the index-creation pass.
--
-- During @IngestChainHistory@ tables are UNLOGGED with no constraints
-- or indexes — the COPY path must run flat-out. Once data has
-- finished loading, this module produces the @CREATE INDEX
-- CONCURRENTLY@ statements that enforce uniqueness and back the
-- query patterns 'FollowingChainTip' depends on.
--
-- Two sources feed the output:
--
--   * 'tdPrimaryKey' on a 'TableDef' becomes a unique btree index
--     named @\<table\>_pkey_idx@. None of the extractor data tables
--     declare a PK during ingest (only @dbsync_sync_state@ does), so
--     this branch is mostly empty.
--   * Each entry in 'tdUniqueConstraints' becomes a unique btree
--     index named @\<table\>_unique_\<n\>_idx@.
--
-- @CREATE INDEX CONCURRENTLY IF NOT EXISTS@ is used everywhere so
-- the pass is restartable after a partial failure.
--
-- Performance-only indexes (FK lookup speedups) are not produced
-- here; they will be added in a follow-up that extends 'TableDef'
-- with explicit index metadata.
module DbSync.Db.Statement.Indexes
  ( tableIndexStatements
  ) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Sql (quoteIdent)

-- | Produce the @CREATE INDEX CONCURRENTLY@ statements that should
-- be run for the given table. One element per index. Empty list
-- if the table declares no PK and no unique constraints.
tableIndexStatements :: TableDef -> [Text]
tableIndexStatements td =
  pkStatement <> uniqueStatements
  where
    pkStatement = case tdPrimaryKey td of
      Nothing   -> []
      Just cols ->
        [createUniqueIndex (tdName td <> "_pkey_idx") (tdName td) cols]

    uniqueStatements =
      zipWith
        (\n cols ->
           createUniqueIndex
             (tdName td <> "_unique_" <> show (n :: Int) <> "_idx")
             (tdName td)
             (NE.toList cols))
        [1 ..]
        (tdUniqueConstraints td)

createUniqueIndex :: Text -> Text -> [Text] -> Text
createUniqueIndex idxName tableName cols =
  T.unwords
    [ "CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS"
    , quoteIdent idxName
    , "ON"
    , quoteIdent tableName
    , "(" <> T.intercalate ", " (map quoteIdent cols) <> ")"
    ]
