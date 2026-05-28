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
  , ingestResolveIndexStatements
  , preResolveIndexStatements
  , postResolveIndexStatements
  , uniqueConstraintIndexName
  , columnRef
  , Concurrency (..)
  ) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import DbSync.Db.Schema.Address (addressTableDef)
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
-- entry for the primary key (defaulting to @id@ when 'tdPrimaryKey'
-- is 'Nothing') plus one per unique constraint. The 'Concurrency'
-- argument chooses between @CREATE INDEX@ (callers with no
-- concurrent writers; full parallel maintenance worker support) and
-- @CREATE INDEX CONCURRENTLY@ (callers running against a live
-- database that cannot tolerate @ShareLock@).
tableIndexStatements :: Concurrency -> TableDef -> [Text]
tableIndexStatements conc td =
  pkStatement : uniqueStatements
  where
    pkCols = fromMaybe ["id"] (tdPrimaryKey td)
    pkStatement =
      renderIndex conc Unique (tdName td <> "_pkey_idx") (tdName td) pkCols

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

-- | Indexes the @IngestChainHistory@ per-epoch address-resolver
-- worker ('DbSync.Worker.TxOut') needs to avoid hash-joining the
-- unindexed @tx_out@ / @address@ / @collateral_tx_out@ heaps once
-- per epoch.
--
-- Built once at the start of @IngestChainHistory@ (see
-- 'DbSync.Phase.Ingest.IngestIndexes.createIngestResolveIndexes') on
-- still-UNLOGGED tables, so the build skips WAL writes. Index names
-- match what 'tableIndexStatements' would emit later, so the
-- schema-driven Prep pass dedupes them via @IF NOT EXISTS@.
--
-- Without these, every per-epoch resolve scans the full @tx_out@
-- heap; cost grows linearly with chain history and is the visible
-- cause of the long @awaitTxOutDrained (epoch N-1)@ stalls an
-- operator sees at epoch boundaries late in @IngestChainHistory@.
ingestResolveIndexStatements :: [Text]
ingestResolveIndexStatements =
  [ -- 'bulkUpdateTxOutAddressIdsStmt' and 'bulkUpdateConsumedByTxIdStmt'
    -- both match by 'tx_out.id' (PK). 'tx_out.id' is assigned
    -- monotonically by the worker counter so the btree insert during
    -- COPY is at the right edge: cheap.
    renderIndex NonConcurrent Unique
      (tdName txOutTableDef <> "_pkey_idx")
      (tdName txOutTableDef)
      [columnRef txOutTableDef "id"]
    -- 'bulkUpdateCollateralTxOutAddressIdsStmt' matches by
    -- 'collateral_tx_out.id'. Same shape as @tx_out@; the table is
    -- much smaller but the per-epoch UPDATE still hash-joins without
    -- the index.
  , renderIndex NonConcurrent Unique
      (tdName collateralTxOutTableDef <> "_pkey_idx")
      (tdName collateralTxOutTableDef)
      [columnRef collateralTxOutTableDef "id"]
    -- 'bulkSelectAddressIdsStmt' joins on 'address.raw_hash'. The
    -- column is @GENERATED ALWAYS AS (md5(raw))@; indexing it
    -- @UNIQUE@ matches the Prep-pass shape and gives the worker an
    -- index nested-loop instead of a full @address@ heap scan.
  , renderIndex NonConcurrent Unique
      (uniqueConstraintIndexName addressTableDef 1)
      (tdName addressTableDef)
      [columnRef addressTableDef "raw_hash"]
  ]

-- | Indexes built before the CTAS resolve runs.
--
-- Only the indexes the CTAS @LEFT JOIN@ depends on and the indexes
-- on tables the CTAS does /not/ rebuild — building them here saves
-- a second pass.
--
-- Non-@CONCURRENTLY@ because the tables are still UNLOGGED at this
-- point: a one-pass build skips both the WAL writes and the
-- second-pass scan that @CONCURRENTLY@ would force.
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
      "collateral_tx_out_tx_id_idx"
      (tdName collateralTxOutTableDef)
      [columnRef collateralTxOutTableDef "tx_id"]
  , renderIndex NonConcurrent NonUnique
      "withdrawal_tx_id_idx"
      (tdName withdrawalTableDef)
      [columnRef withdrawalTableDef "tx_id"]
  ]

-- | Perf indexes built /after/ the CTAS rebuilds. The CTAS DROPs and
-- replaces @tx_in@ and @collateral_tx_in@, so any index on those
-- tables built before the rebuild would be lost.
postResolveIndexStatements :: [Text]
postResolveIndexStatements =
  [ renderIndex NonConcurrent NonUnique
      "tx_in_tx_in_id_idx"
      (tdName txInTableDef)
      [columnRef txInTableDef "tx_in_id"]
  , renderIndex NonConcurrent NonUnique
      "collateral_tx_in_tx_in_id_idx"
      (tdName collateralTxInTableDef)
      [columnRef collateralTxInTableDef "tx_in_id"]
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
