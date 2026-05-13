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
--   * 'tableIndexStatements': the full schema-driven set, emitted as
--     @CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS@ once the
--     tables have been flipped to LOGGED. Driven by 'tdPrimaryKey'
--     and 'tdUniqueConstraints' on each 'TableDef'. The @IF NOT
--     EXISTS@ clause makes any index already built by the pre-resolve
--     pass a no-op here.
--
-- Performance-only indexes (lookup speedups that aren't enforcing
-- uniqueness) are not produced by 'tableIndexStatements'; they will
-- be added in a follow-up that extends 'TableDef' with explicit
-- index metadata. The pre-resolve set hand-rolls two such indexes
-- in the meantime.
module DbSync.Db.Statement.Indexes
  ( tableIndexStatements
  , preResolveIndexStatements
  , uniqueConstraintIndexName
  , columnRef
  ) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import DbSync.Db.Schema.Core (txTableDef)
import DbSync.Db.Schema.Types (ColumnDef (..), TableDef (..))
import DbSync.Db.Schema.UTxO (txInTableDef, txOutTableDef)
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
        [renderIndex Concurrent Unique (tdName td <> "_pkey_idx") (tdName td) cols]

    uniqueStatements =
      zipWith
        (\n cols ->
           renderIndex Concurrent Unique
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

-- | Look up a column by name on a 'TableDef'. Returns the name if
-- the column is declared; panics at evaluation time if not. Lets
-- hand-rolled SQL fragments refer to columns through the schema so
-- a rename of 'cdName' in a 'ColumnDef' surfaces as a load-time
-- error rather than a silent runtime query failure.
columnRef :: TableDef -> Text -> Text
columnRef td name
  | name `elem` map cdName (tdColumns td) = name
  | otherwise = panic $
      "DbSync.Db.Statement.Indexes.columnRef: column "
        <> name <> " not declared on table " <> tdName td

-- | Indexes built before the post-load resolves and CTE backfills.
-- Non-@CONCURRENTLY@ because the tables are still UNLOGGED at this
-- point: a one-pass build skips both the WAL writes and the
-- second-pass scan that @CONCURRENTLY@ would force.
--
-- The first entry is named to match what 'tableIndexStatements'
-- will later emit from @txTableDef.tdUniqueConstraints@, so the
-- post-flip concurrent re-build becomes an @IF NOT EXISTS@ no-op.
-- The other two are non-unique perf indexes with no schema-level
-- declaration; only this pass emits them.
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
  ]

-- ---------------------------------------------------------------------------
-- * Internals
-- ---------------------------------------------------------------------------

-- | Whether the index DDL should use @CONCURRENTLY@. Concurrent
-- builds are required when the table is LOGGED and being written
-- to; non-concurrent builds are dramatically faster when neither
-- of those holds (UNLOGGED tables, or LOGGED tables with no
-- concurrent writers).
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
