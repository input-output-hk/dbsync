{-# LANGUAGE OverloadedStrings #-}

-- | DDL builders for the post-load index passes.
--
-- During @IngestChainHistory@ tables are UNLOGGED with no indexes —
-- the COPY path runs flat-out. Three passes build the indexes the
-- resolves and Follow query patterns need:
--
--   * 'ingestResolveIndexStatements' — built at the start of Ingest
--     so the per-epoch address-resolver worker doesn't hash-join
--     the unindexed @tx_out@ / @address@ heaps.
--   * 'preResolveIndexStatements' — the minimum set before the
--     post-load UPDATEs run. Tables are still UNLOGGED so the build
--     skips WAL writes and the second-pass scan that @CONCURRENTLY@
--     would force.
--   * 'tableIndexStatements' — full schema-driven set, driven by
--     'tdPrimaryKey' and 'tdUniqueConstraints'. @IF NOT EXISTS@
--     dedupes against the pre-resolve pass.
--
-- Performance-only (non-uniqueness) indexes are hand-rolled in the
-- pre-resolve set until 'TableDef' grows explicit index metadata.
module DbSync.Db.Statement.Indexes
  ( tableIndexStatements
  , ingestResolveIndexStatements
  , preResolveIndexStatements
  , postResolveIndexStatements
  , uniqueConstraintIndexName
  , Concurrency (..)
  ) where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import DbSync.Db.Schema.Address (AddressCols (..), addressCols, addressTableDef)
import DbSync.Db.Schema.Core (TxCols (..), txCols, txTableDef)
import DbSync.Db.Schema.StakeDelegation
  ( WithdrawalCols (..)
  , withdrawalCols
  , withdrawalTableDef
  )
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Schema.UTxO
  ( CollateralTxInCols (..)
  , CollateralTxOutCols (..)
  , TxInCols (..)
  , TxOutCols (..)
  , collateralTxInCols
  , collateralTxInTableDef
  , collateralTxOutCols
  , collateralTxOutTableDef
  , txInCols
  , txInTableDef
  , txOutCols
  , txOutTableDef
  )
import DbSync.Db.Sql (quoteIdent)

-- | One entry for the primary key (defaulting to @id@ when
-- 'tdPrimaryKey' is 'Nothing') plus one per unique constraint.
-- 'Concurrent' = @CREATE INDEX CONCURRENTLY@ (live database, no
-- @ShareLock@); 'NonConcurrent' = full parallel maintenance worker
-- support.
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

-- | The name 'tableIndexStatements' emits for the @n@-th (1-based)
-- entry in @td.tdUniqueConstraints@. Callers that hand-roll a
-- non-concurrent build use this so a later concurrent re-build's
-- @IF NOT EXISTS@ matches.
uniqueConstraintIndexName :: TableDef -> Int -> Text
uniqueConstraintIndexName td n =
  tdName td <> "_unique_" <> show n <> "_idx"

-- | Built at the start of Ingest. Index names match what
-- 'tableIndexStatements' would emit later, so the schema-driven Prep
-- pass dedupes via @IF NOT EXISTS@.
--
-- Without these, every per-epoch resolve scans the full @tx_out@
-- heap — the visible cause of @awaitTxOutDrained (epoch N-1)@ stalls
-- late in Ingest.
ingestResolveIndexStatements :: [Text]
ingestResolveIndexStatements =
  [ -- 'bulkUpdateTxOutAddressIdsStmt' and 'bulkUpdateConsumedByTxIdStmt'
    -- match by 'tx_out.id' (PK). The worker counter assigns ids
    -- monotonically so the btree insert during COPY is right-edge.
    renderIndex NonConcurrent Unique
      (tdName txOutTableDef <> "_pkey_idx")
      (tdName txOutTableDef)
      [txOutCols.tocId.tcName]
    -- 'bulkUpdateCollateralTxOutAddressIdsStmt' matches by id.
  , renderIndex NonConcurrent Unique
      (tdName collateralTxOutTableDef <> "_pkey_idx")
      (tdName collateralTxOutTableDef)
      [collateralTxOutCols.ctocId.tcName]
    -- 'bulkSelectAddressIdsStmt' joins on 'address.raw_hash'
    -- (@GENERATED ALWAYS AS (md5(raw))@). @UNIQUE@ matches the Prep
    -- shape and gives the worker an index nested-loop instead of a
    -- full heap scan.
  , renderIndex NonConcurrent Unique
      (uniqueConstraintIndexName addressTableDef 1)
      (tdName addressTableDef)
      [addressCols.acRawHash.tcName]
  ]

-- | Indexes the CTAS resolve @LEFT JOIN@ depends on, plus indexes on
-- tables the CTAS does not rebuild (so they survive). Non-concurrent
-- because tables are still UNLOGGED: skips WAL and the second-pass
-- scan.
preResolveIndexStatements :: [Text]
preResolveIndexStatements =
  [ renderIndex NonConcurrent Unique
      (uniqueConstraintIndexName txTableDef 1)
      (tdName txTableDef)
      [txCols.tcHash.tcName]
  , renderIndex NonConcurrent NonUnique
      "tx_out_tx_id_index_idx"
      (tdName txOutTableDef)
      [txOutCols.tocTxId.tcName, txOutCols.tocIndex.tcName]
  , renderIndex NonConcurrent NonUnique
      "collateral_tx_out_tx_id_idx"
      (tdName collateralTxOutTableDef)
      [collateralTxOutCols.ctocTxId.tcName]
  , renderIndex NonConcurrent NonUnique
      "withdrawal_tx_id_idx"
      (tdName withdrawalTableDef)
      [withdrawalCols.wcTxId.tcName]
  ]

-- | Built /after/ the CTAS rebuilds. The CTAS DROPs and replaces
-- @tx_in@ and @collateral_tx_in@, so any index on those tables built
-- earlier would be lost.
postResolveIndexStatements :: [Text]
postResolveIndexStatements =
  [ renderIndex NonConcurrent NonUnique
      "tx_in_tx_in_id_idx"
      (tdName txInTableDef)
      [txInCols.ticTxInId.tcName]
  , renderIndex NonConcurrent NonUnique
      "collateral_tx_in_tx_in_id_idx"
      (tdName collateralTxInTableDef)
      [collateralTxInCols.cticTxInId.tcName]
  ]

-- ---------------------------------------------------------------------------
-- * Internals
-- ---------------------------------------------------------------------------

-- | Concurrent builds are required when the table is LOGGED and
-- being written to. Non-concurrent avoids the second validation
-- scan and gets full @max_parallel_maintenance_workers@ — preferred
-- for UNLOGGED tables or LOGGED tables without concurrent writers.
data Concurrency = Concurrent | NonConcurrent

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
