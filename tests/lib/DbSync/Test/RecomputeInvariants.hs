{-# LANGUAGE OverloadedStrings #-}

-- | Recompute-invariant checks against the live test database. Each asserts
-- that a stored, derived value equals a fresh recomputation from its source
-- tables, catching the data drift a schema fingerprint cannot (the
-- cardano-db-sync \#2118 class). Every action returns the number of violating
-- rows, so @0@ means the invariant holds.
module DbSync.Test.RecomputeInvariants
  ( epochFinalizedDriftCount
  , blockTxCountDriftCount
  , txOutSumDriftCount
  , duplicateEpochRowGroupCount
  , epochContiguityGapCount
  , consumedByDriftCount
  ) where

import Cardano.Prelude

import qualified Data.Text as T

import DbSync.Db.Schema.AdaPots (adaPotsTableDef)
import DbSync.Db.Schema.Core (TxCols (..), blockTableDef, txCols)
import DbSync.Db.Schema.EpochBoundary (epochParamTableDef, epochStateTableDef)
import DbSync.Db.Schema.EpochSyncStats (epochSyncStatsTableDef)
import DbSync.Db.Schema.Governance (drepDistrTableDef)
import DbSync.Db.Schema.Pool (poolStatTableDef)
import DbSync.Db.Schema.StakeDelegation (epochStakeProgressTableDef)
import DbSync.Db.Schema.Types (TableColumn (..), TableDef (..))
import DbSync.Db.Schema.UTxO (TxInCols (..), TxOutCols (..), txInCols, txOutCols)
import DbSync.Test.Database (queryTestDb)
import DbSync.Test.PgAssertions (tableColumn)

-- ---------------------------------------------------------------------------
-- * Invariant checks
-- ---------------------------------------------------------------------------

-- | @epoch_finalized@ rows whose stored aggregates diverge from a fresh
-- @block@+@tx@ recomputation (the body of the @epoch_current@ view), or whose
-- synthesised @id@ is not @no + 1@.
epochFinalizedDriftCount :: IO Int
epochFinalizedDriftCount =
  countViolations $
    T.unwords
      [ "SELECT ef.no FROM epoch_finalized ef"
      , "JOIN ("
      , "  SELECT b.epoch_no AS no,"
      , "         COALESCE(SUM(tx.out_sum), 0)::numeric AS out_sum,"
      , "         COALESCE(SUM(tx.fee), 0) AS fees,"
      , "         COUNT(tx.id)::bigint AS tx_count,"
      , "         COUNT(DISTINCT b.id)::bigint AS blk_count,"
      , "         MIN(b.time) AS start_time,"
      , "         MAX(b.time) AS end_time"
      , "  FROM block b LEFT JOIN tx ON tx.block_id = b.id"
      , "  WHERE b.epoch_no IS NOT NULL"
      , "  GROUP BY b.epoch_no"
      , ") r ON r.no = ef.no"
      , "WHERE ef.out_sum <> r.out_sum"
      , "   OR ef.fees <> r.fees"
      , "   OR ef.tx_count <> r.tx_count"
      , "   OR ef.blk_count <> r.blk_count"
      , "   OR ef.start_time <> r.start_time"
      , "   OR ef.end_time <> r.end_time"
      , "   OR ef.id <> ef.no + 1"
      ]

-- | @block@ rows whose stored @tx_count@ differs from the actual tx count.
blockTxCountDriftCount :: IO Int
blockTxCountDriftCount =
  countViolations $
    T.unwords
      [ "SELECT b.id FROM block b"
      , "LEFT JOIN (SELECT block_id, COUNT(*) AS c FROM tx GROUP BY block_id) t"
      , "  ON t.block_id = b.id"
      , "WHERE b.tx_count <> COALESCE(t.c, 0)"
      ]

-- | Valid @tx@ rows whose stored @out_sum@ differs from the sum of their
-- outputs. Scoped to @valid_contract@ because phase-2-failed txs record
-- @collateral_tx_out@ rather than @tx_out@.
txOutSumDriftCount :: IO Int
txOutSumDriftCount =
  countViolations $
    T.unwords
      [ "SELECT t.id FROM tx t"
      , "LEFT JOIN (SELECT tx_id, SUM(value) AS s FROM tx_out GROUP BY tx_id) o"
      , "  ON o.tx_id = t.id"
      , "WHERE t.valid_contract = true"
      , "  AND t.out_sum <> COALESCE(o.s, 0)"
      ]

-- | Tables Follow writes at most once per natural key; a boundary
-- crossed twice without rollback cleanup shows up as a duplicate
-- group. Keys derive from each table's sole unique constraint so the
-- check tracks schema edits.
epochKeyedTables :: [TableDef]
epochKeyedTables =
  [ epochStateTableDef
  , adaPotsTableDef
  , epochParamTableDef
  , poolStatTableDef
  , drepDistrTableDef
  , epochSyncStatsTableDef
  , epochStakeProgressTableDef
  ]

-- | Natural-key groups holding more than one row, summed across every
-- 'epochKeyedTables' member present in the schema (profiles disable
-- some extractors, so a missing table counts as clean).
duplicateEpochRowGroupCount :: IO Int
duplicateEpochRowGroupCount = do
  counts <- for epochKeyedTables $ \td -> do
    present <- tablePresent (tdName td)
    if present then countViolations (dupGroupsQuery td) else pure 0
  pure (sum counts)
  where
    dupGroupsQuery td =
      let keyCols = case tdUniqueConstraints td of
            [k] -> T.intercalate ", " (toList k)
            _   -> panic $
              "duplicateEpochRowGroupCount: " <> tdName td
                <> " must declare exactly one unique constraint"
      in T.unwords
        [ "SELECT", keyCols, "FROM", tdName td
        , "GROUP BY", keyCols
        , "HAVING COUNT(*) > 1"
        ]

-- | Disagreements between @tx_in@ and @tx_out.consumed_by_tx_id@,
-- in both directions: a spent output whose mark is missing or names
-- the wrong spender, and a marked output that no @tx_in@ row claims.
-- The producer is located through @tx.hash@ so rows whose
-- @tx_in.tx_out_id@ is still unresolved are covered too. Only valid
-- for profiles with @utxo.consumed_by_tx_id@ enabled.
consumedByDriftCount :: IO Int
consumedByDriftCount = do
  present <- tablePresent txIn
  if not present
    then pure 0
    else do
      wrongOrMissing <- countViolations $ T.unwords
        [ "SELECT ti." <> name txInCols.ticId
        , "FROM " <> txIn <> " ti"
        , "JOIN " <> tx <> " t"
        , "  ON t." <> name txCols.tcHash <> " = ti." <> name txInCols.ticTxOutHash
        , "JOIN " <> txOut <> " o"
        , "  ON o." <> name txOutCols.tocTxId <> " = t." <> name txCols.tcId
        , " AND o." <> name txOutCols.tocIndex <> " = ti." <> name txInCols.ticTxOutIndex
        , "WHERE o." <> consumedBy <> " IS DISTINCT FROM ti." <> name txInCols.ticTxInId
        ]
      phantom <- countViolations $ T.unwords
        [ "SELECT o." <> name txOutCols.tocId
        , "FROM " <> txOut <> " o"
        , "WHERE o." <> consumedBy <> " IS NOT NULL"
        , "  AND NOT EXISTS ("
        , "    SELECT 1 FROM " <> txIn <> " ti"
        , "    JOIN " <> tx <> " t"
        , "      ON t." <> name txCols.tcHash <> " = ti." <> name txInCols.ticTxOutHash
        , "    WHERE t." <> name txCols.tcId <> " = o." <> name txOutCols.tocTxId
        , "      AND ti." <> name txInCols.ticTxOutIndex <> " = o." <> name txOutCols.tocIndex
        , "  )"
        ]
      pure (wrongOrMissing + phantom)
  where
    name       = tcName
    txIn       = tdName (tcTable txInCols.ticId)
    txOut      = tdName (tcTable txOutCols.tocId)
    tx         = tdName (tcTable txCols.tcId)
    consumedBy = name txOutCols.tocConsumedByTxId

-- | Epoch numbers inside @[MIN .. MAX]@ of @block.epoch_no@ that no
-- block row carries — a hole means a rollback/re-advance lost an
-- epoch's blocks.
epochContiguityGapCount :: IO Int
epochContiguityGapCount =
  countViolations $
    T.unwords
      [ "SELECT s.no FROM ("
      , "  SELECT generate_series(MIN(" <> epochNo <> "), MAX(" <> epochNo <> ")) AS no"
      , "  FROM " <> blockTbl <> " WHERE " <> epochNo <> " IS NOT NULL"
      , ") s"
      , "LEFT JOIN (SELECT DISTINCT " <> epochNo <> " FROM " <> blockTbl <> ") b"
      , "  ON b." <> epochNo <> " = s.no"
      , "WHERE b." <> epochNo <> " IS NULL"
      ]
  where
    blockTbl = tdName blockTableDef
    epochNo  = tableColumn blockTableDef "epoch_no"

-- ---------------------------------------------------------------------------
-- * Internal
-- ---------------------------------------------------------------------------

tablePresent :: Text -> IO Bool
tablePresent table = do
  t <- T.strip <$> queryTestDb ("SELECT to_regclass('" <> table <> "') IS NOT NULL;")
  pure (t == "t")

-- Count the rows a mismatch query returns; -1 if the count fails to parse.
countViolations :: Text -> IO Int
countViolations q = do
  t <- T.strip <$> queryTestDb ("SELECT count(*) FROM (" <> q <> ") AS v;")
  pure (fromMaybe (-1) (readMaybe (T.unpack t)))
