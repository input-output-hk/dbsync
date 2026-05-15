{-# LANGUAGE OverloadedStrings #-}

-- | DDL builders for the post-load index passes.
--
-- During @IngestChainHistory@ tables are UNLOGGED with no constraints
-- or indexes — the COPY path must run flat-out. Once data has
-- finished loading, two passes build the indexes the resolves and
-- the FollowingChainTip query patterns need:
--
--   * 'preResolveIndexStatements' (this module): the minimum set
--     emitted as @CREATE [UNIQUE] INDEX IF NOT EXISTS@ before the
--     post-load UPDATEs run. Tables are still UNLOGGED so the build
--     skips the WAL writes and second-pass scan that @CONCURRENTLY@
--     would force. Without these, the resolves and CTE backfills
--     hash-join the @tx@ / @tx_out@ / @tx_in@ heaps in their entirety.
--
--   * 'tableIndexStatements': the full schema-driven set, driven by
--     'tdPrimaryKey' and 'tdUniqueConstraints' on each 'TableDef'.
--     The caller chooses 'NonConcurrent' or 'Concurrent' via the
--     'Concurrency' argument. Prep runs while no other session is
--     touching the DB and so passes 'NonConcurrent' — this unlocks
--     @max_parallel_maintenance_workers@ (which @CONCURRENTLY@
--     effectively disables for the validation scan) and avoids the
--     second heap scan. A future Follow-time path that adds indexes
--     against a live database can call the same builder with
--     'Concurrent'. The @IF NOT EXISTS@ clause makes any index
--     already built by the pre-resolve pass a no-op here.
--
-- Performance-only indexes (lookup speedups that aren't enforcing
-- uniqueness) are not produced by 'tableIndexStatements'; they will
-- be added in a follow-up that extends 'TableDef' with explicit
-- index metadata. The pre-resolve set hand-rolls them in the meantime.
module DbSync.Db.Statement.Indexes
  ( tableIndexStatements
  , preResolveIndexStatements
  , uniqueConstraintIndexName
  , columnRef
  , Concurrency (..)
  ) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import DbSync.Db.Schema.Core (txTableDef)
import DbSync.Db.Schema.StakeDelegation (withdrawalTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Db.Sql (quoteIdent)
import DbSync.Db.Sql.Refs (columnRef)

-- | Produce the @CREATE INDEX@ statements for the given table. One
-- element per index. Empty list if the table declares no PK and no
-- unique constraints. The 'Concurrency' argument chooses between
-- @CREATE INDEX@ (callers with no concurrent writers; full parallel
-- maintenance worker support) and @CREATE INDEX CONCURRENTLY@
-- (callers running against a live database that cannot tolerate
-- @ShareLock@).
tableIndexStatements :: Concurrency -> TableDef -> [Text]
tableIndexStatements conc td =
  pkStatement <> uniqueStatements
  where
    pkStatement = case tdPrimaryKey td of
      Nothing   -> []
      Just cols ->
        [renderIndex conc Unique (tdName td <> "_pkey_idx") (tdName td) cols]

    uniqueStatements =
      zipWith
        (\n cols ->
           renderIndex conc Unique
             (uniqueConstraintIndexName td n)
             (tdName td)
             (NE.toList cols))
        [1 ..]
        (tdUniqueConstraints td)

-- | The index name 'tableIndexStatements' will emit for the @n@-th
-- entry (1-based) in @td.tdUniqueConstraints@. Used by callers that
-- hand-roll a non-concurrent build whose @IF NOT EXISTS@ clause
-- must match a later concurrent re-build.
uniqueConstraintIndexName :: TableDef -> Int -> Text
uniqueConstraintIndexName td n =
  tdName td <> "_unique_" <> show n <> "_idx"

-- | Indexes built before the post-load resolves and CTE backfills.
-- Non-@CONCURRENTLY@ because the tables are still UNLOGGED at this
-- point: a one-pass build skips both the WAL writes and the
-- second-pass scan that @CONCURRENTLY@ would force.
--
-- The first entry is named to match what 'tableIndexStatements'
-- will later emit from @txTableDef.tdUniqueConstraints@, so the
-- post-flip concurrent re-build becomes an @IF NOT EXISTS@ no-op.
-- The others are non-unique perf indexes with no schema-level
-- declaration; only this pass emits them.
--
-- The four @_id_idx@ entries support the rewritten post-load
-- backfill UPDATEs: each backfill drives off a small (or at least
-- bounded) filtered set of @tx@ rows and looks up each row's
-- collateral / inputs / withdrawals via these indexes, replacing
-- the previous "aggregate everything, then filter to a handful"
-- pattern that scaled with input count rather than output count.
preResolveIndexStatements :: [Text]
preResolveIndexStatements =
  [ renderIndex NonConcurrent Unique
      (uniqueConstraintIndexName txTableDef 1)
      (tdName txTableDef)
      [columnRef txTableDef "hash"]
  , renderIndex NonConcurrent NonUnique
      "tx_out_tx_id_index_idx"
      (tdName txOutTableDef)
      [ columnRef txOutTableDef "tx_id"
      , columnRef txOutTableDef "index"
      ]
  , renderIndex NonConcurrent NonUnique
      "tx_in_tx_out_idx"
      (tdName txInTableDef)
      [ columnRef txInTableDef "tx_out_id"
      , columnRef txInTableDef "tx_out_index"
      ]
  , renderIndex NonConcurrent NonUnique
      "collateral_tx_in_tx_in_id_idx"
      (tdName collateralTxInTableDef)
      [columnRef collateralTxInTableDef "tx_in_id"]
  , renderIndex NonConcurrent NonUnique
      "collateral_tx_out_tx_id_idx"
      (tdName collateralTxOutTableDef)
      [columnRef collateralTxOutTableDef "tx_id"]
  , renderIndex NonConcurrent NonUnique
      "tx_in_tx_in_id_idx"
      (tdName txInTableDef)
      [columnRef txInTableDef "tx_in_id"]
  , renderIndex NonConcurrent NonUnique
      "withdrawal_tx_id_idx"
      (tdName withdrawalTableDef)
      [columnRef withdrawalTableDef "tx_id"]
  ]

-- ---------------------------------------------------------------------------
-- * Internals
-- ---------------------------------------------------------------------------

-- | Whether the index DDL should use @CONCURRENTLY@. Concurrent
-- builds are required when the table is LOGGED and being written
-- to. Non-concurrent builds avoid the second validation scan and
-- get full @max_parallel_maintenance_workers@ parallelism, so they
-- are preferred when neither of those constraints holds (UNLOGGED
-- tables, or LOGGED tables with no concurrent writers).
data Concurrency = Concurrent | NonConcurrent

-- | Whether the index enforces uniqueness.
data Uniqueness  = Unique | NonUnique

renderIndex :: Concurrency -> Uniqueness -> Text -> Text -> [Text] -> Text
renderIndex conc uniq idxName tableName cols =
  T.unwords $ filter (not . T.null)
    [ "CREATE"
    , case uniq of Unique -> "UNIQUE INDEX"; NonUnique -> "INDEX"
    , case conc of Concurrent -> "CONCURRENTLY"; NonConcurrent -> ""
    , "IF NOT EXISTS"
    , quoteIdent idxName
    , "ON"
    , quoteIdent tableName
    , "(" <> T.intercalate ", " (map quoteIdent cols) <> ")"
    ]
