{-# LANGUAGE OverloadedStrings #-}

-- | One-time post-load pass between 'IngestChainHistory' and
-- 'FollowingChainTip'.
--
-- The bulk-load phase leaves three things in a transitional state:
--
--   * Several FK columns are NULL because the rows they point at
--     hadn't been written yet at COPY time. The hash + index
--     pair on @tx_in@ / @collateral_tx_in@ / @reference_tx_in@
--     identifies the producing tx; @tx_out.consumed_by_tx_id@
--     similarly waits for its consumer.
--   * Three @tx@ columns (the phase-2 fee, the phase-2 deposit, and
--     the ledger-disabled valid-contract deposit) plus
--     @redeemer.script_hash@ for spend redeemers cannot be filled
--     from the body alone. The parser leaves them as a sentinel or
--     NULL.
--   * Tables are UNLOGGED with no sequences attached and only the
--     resolve-support indexes — the COPY pipeline ran flat-out.
--
-- 'run' walks all of that in a single pass. The order matters:
--
--   * The pre-resolve index build runs first so the resolve and
--     backfill UPDATEs use index lookups rather than hash-joining
--     the @tx@ and @tx_out@ heaps in their entirety.
--   * Foreign-key resolution comes before the backfill UPDATEs that
--     rely on @tx_in.tx_out_id@ being populated.
--   * Every index is dropped before the UNLOGGED → LOGGED flip.
--     @ALTER TABLE … SET LOGGED@ rewrites the heap /and rebuilds
--     every index on the table inside the ALTER/, so an index alive
--     at flip time is built twice. Bare heaps flip fastest, and the
--     rewrite compacts the dead tuples the backfill UPDATEs left
--     behind.
--   * The production index build runs after the flip, building each
--     Follow-facing index exactly once, fanned out per index across
--     the pool.
--   * The ownership foreign keys go on after that build and the final
--     ANALYZE, so each validation scan can probe the parent's PK index
--     and plan against current statistics.
--   * Sequence reset runs last: it needs the flip to have attached
--     the sequences and the PK indexes for the @MAX(id)@ lookups.
--
-- Every step is bracketed by the uniform @Starting | Completed@
-- log lines of 'DbSync.Phase.Preparing.Step', emitted at 'Info'.
module DbSync.Phase.Preparing.Run
  ( run
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.List (sortOn)
import qualified Hasql.Connection.Settings as ConnSettings
import qualified Hasql.Session as Sess

import DbSync.App.Env (HasConfig)
import DbSync.Db.Pool (forPooled_, usePool, withPrepPool)
import DbSync.Db.Run (useConn)
import DbSync.Db.Schema.Address (addressTableDef)
import DbSync.Db.Schema.Core (blockTableDef, txTableDef)
import DbSync.Db.Schema.EpochView (epochFinalizedTableName)
import DbSync.Db.Schema.Init
  ( analyzeSql
  , perTableSchemaForFollowTipSql
  )
import DbSync.Db.Schema.ScriptsDatums (redeemerTableDef)
import DbSync.Db.Schema.StakeDelegation (withdrawalTableDef)
import DbSync.Db.Schema.Types (TableDef (..), TableMode (..))
import DbSync.Db.Schema.UTxO
  ( collateralTxOutTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Db.Statement.Indexes
  ( dropIndexSql
  , resolveScaffoldingIndexNames
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import qualified DbSync.Phase.Preparing.Backfill as Backfill
import qualified DbSync.Phase.Preparing.Constraints as Constraints
import qualified DbSync.Phase.Preparing.Indexes as Indexes
import qualified DbSync.Phase.Preparing.PreResolveIndexes as PreResolveIndexes
import qualified DbSync.Phase.Preparing.Resolve as Resolve
import qualified DbSync.Phase.Preparing.Sequences as Sequences
import DbSync.Phase.Preparing.Step (StepKind (..), step)
import DbSync.Phase.Preparing.Tuning
  ( PrepTuning (..)
  , setPrepSessionGUCs
  )
import DbSync.Trace (HasTracer (..))

-- | Run the full post-load sequence against the env's connection.
--
-- See the module Haddock for the step ordering and rationale.
run
  :: ( HasTracer env
     , HasHasqlConnection env
     , HasConfig env
     , MonadReader env m
     , MonadUnliftIO m
     )
  => ConnSettings.Settings
  -- ^ Settings for opening additional backends in the
  -- parallel-capable steps. Must connect to the same database as
  -- the env's 'Conn.Connection'.
  -> PrepTuning
  -> [TableDef]
  -> m ()
run connSettings tuning tables = step PhaseStep "post-load pass" $ do
  -- Session-level GUCs (maintenance_work_mem,
  -- max_parallel_maintenance_workers, synchronous_commit) applied
  -- once at the top so every subsequent index build / ANALYZE on
  -- the control connection picks them up. Pool backends get the
  -- same GUCs via their initSession hook.
  step TuningStep "session GUCs" $
    setPrepSessionGUCs tuning

  -- Scaffolding for the resolves + backfills. The input-table
  -- indexes that the CTAS rebuilds would drop are built afterwards
  -- ('createPostResolveIndexes').
  PreResolveIndexes.createPreResolveIndexes

  Resolve.resolveInputTxOutIds

  PreResolveIndexes.createPostResolveIndexes

  -- Refresh planner statistics for every table the backfills read.
  -- Autovacuum runs on UNLOGGED tables but its last sample was
  -- taken mid-ingest; without this pass the planner picks Nested
  -- Loop plans whose outer-side estimate is off by orders of
  -- magnitude. The three CTAS-rebuilt input tables are ANALYZEd
  -- inside 'Resolve.resolveInputTxOutIds'.
  step AnalyzeStep "backfill input tables" $
    for_ (filter (hasTable . tdName) backfillAnalyzeTables) $ \td ->
      runDdl (analyzeSql (tdName td))

  _ <- Backfill.backfillTxColumns
  -- Needs both tables: the hash comes off the spent output that
  -- @tx_in@ points at, and lands on a @redeemer@ row.
  when (hasTable (tdName redeemerTableDef) && hasTable (tdName txInTableDef)) $
    void Backfill.backfillSpendScriptHash
  _ <- Backfill.applyDepositPending
  step CleanupStep "truncate epoch_param_pending"
    Backfill.truncateDepositPending

  -- Everything after this point is schema shaping: no step below
  -- reads via the scaffolding indexes, so they come off before the
  -- flip pays to rebuild them inside each ALTER.
  step IndexStep "drop resolve-support indexes" $
    for_ resolveScaffoldingIndexNames (runDdl . dropIndexSql)

  step FlipStep "all UNLOGGED tables" $
    withPrepPool connSettings tuning (ptPoolSize tuning) $
      forPooled_ (ptPoolSize tuning) (flipOrder tables) $ \td ->
        step FlipStep (tdName td) $
          usePool ("flip " <> tdName td) $
            traverse_ Sess.script (perTableSchemaForFollowTipSql td)

  step IndexStep "production index build" $
    withPrepPool connSettings tuning (ptPoolSize tuning) $
      Indexes.createIndexes (ptPoolSize tuning) tables

  -- After the index build: the INSERT upserts via
  -- @ON CONFLICT ("no")@, which needs the unique index on
  -- @epoch_finalized.no@. Before the sequence reset: it inserts
  -- explicit ids that the reset must see.
  when (any ((== epochFinalizedTableName) . tdName) tables)
    Backfill.backfillEpochFinalized

  step AnalyzeStep "all tables" $
    for_ tables $ \td -> runDdl (analyzeSql (tdName td))

  step ConstraintStep "add ownership foreign keys" $
    Constraints.addConstraints tables
  step ConstraintStep "validate ownership foreign keys" $
    withPrepPool connSettings tuning (ptPoolSize tuning) $
      Constraints.validateConstraints (ptPoolSize tuning) tables

  step SequenceStep "reset id sequences to MAX(id) + 1" $
    Sequences.resetSequences tables
  where
    hasTable name = any ((== name) . tdName) tables

-- | UNLOGGED tables, biggest heap first: the flip is one rewrite
-- per table on a bounded pool, so the largest rewrites must start
-- while the pool still has spare backends.
flipOrder :: [TableDef] -> [TableDef]
flipOrder =
  sortOn (Indexes.tableSizeRank . tdName)
    . filter ((== TableUnlogged) . tdMode)

-- | Tables the backfill UPDATEs read or write, minus the three
-- CTAS-rebuilt input tables (ANALYZEd right after their rebuild).
-- Listed once here so the ANALYZE pass and the backfill writers
-- agree on which tables need fresh statistics. Filtered against the
-- enabled set at the call site — a disabled extractor's tables were
-- never created.
backfillAnalyzeTables :: [TableDef]
backfillAnalyzeTables =
  [ blockTableDef
  , txTableDef
  , txOutTableDef
  , collateralTxOutTableDef
  , withdrawalTableDef
  , addressTableDef
  , redeemerTableDef
  ]

runDdl
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Text -> m ()
runDdl ddl = do
  conn <- asks getHasqlConnection
  useConn "Phase.Preparing.Run.runDdl" conn (Sess.script ddl)
