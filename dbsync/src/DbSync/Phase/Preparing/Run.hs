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
--     the ledger-disabled valid-contract deposit) cannot be filled
--     from the body alone. The parser leaves them as a sentinel or
--     NULL.
--   * Tables are UNLOGGED with no sequences attached and no
--     indexes — the COPY pipeline ran flat-out.
--
-- 'run' walks all of that in a single pass against the env's hasql
-- connection. The order matters: the pre-resolve index build must
-- run first so the subsequent join-on-hash UPDATEs use index
-- lookups rather than hash-joining the @tx@ and @tx_out@ heaps in
-- their entirety; foreign-key resolution must come before the
-- backfill UPDATEs that rely on @tx_in.tx_out_id@ being populated;
-- the schema-mode flip must come before sequence reset (the
-- sequence has to exist before we can @setval@ it).
--
-- Per-step timings are emitted at 'Debug' severity via
-- 'DbSync.Trace.Timing'. Operators that need to see which step is
-- running raise their profile's @logging.level@ to @debug@; the
-- production default of @info@ shows only the outer "started" and
-- "complete" markers.
module DbSync.Phase.Preparing.Run
  ( run
  ) where

import Cardano.Prelude

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Tracer (traceWith)
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as ConnSettings
import qualified Hasql.Session as Sess

import DbSync.AppM (LoggingM)
import DbSync.Db.Pool (usePool, withPrepPool)
import DbSync.Db.Schema.Core (blockTableDef, txTableDef)
import DbSync.Db.Schema.Init
  ( analyzeSql
  , perTableSchemaForFollowTipSql
  , vacuumSql
  )
import DbSync.Db.Schema.StakeDelegation (withdrawalTableDef)
import DbSync.Db.Schema.Types (TableDef (..), TableMode (..))
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , txInTableDef
  , txOutTableDef
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import qualified DbSync.Phase.Preparing.Backfill as Backfill
import qualified DbSync.Phase.Preparing.Indexes as Indexes
import qualified DbSync.Phase.Preparing.PreResolveIndexes as PreResolveIndexes
import qualified DbSync.Phase.Preparing.Resolve as Resolve
import qualified DbSync.Phase.Preparing.Sequences as Sequences
import DbSync.Phase.Preparing.Tuning
  ( PrepTuning (..)
  , setPrepSessionGUCs
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Timing (timedTrace_)
import DbSync.Trace.Types (LogMsg (..), Severity (..))

-- | Component name used in trace lines emitted by this module and
-- its sub-modules.
prepComponent :: Text
prepComponent = "PreparingForVolatileTail"

-- | Run the full post-load sequence against the env's connection.
--
-- See the module Haddock for the step ordering and rationale.
run
  :: (LoggingM env m, HasHasqlConnection env)
  => ConnSettings.Settings
  -- ^ Settings for opening additional backends in the
  -- parallel-capable steps. Must connect to the same database as
  -- the env's 'Conn.Connection'.
  -> PrepTuning
  -> [TableDef]
  -> m ()
run connSettings tuning tables = do
  tracer <- asks getTracer
  liftIO $ traceWith tracer $ LogMsg Info prepComponent "post-load pass: started" Nothing

  -- Apply session-level GUCs (maintenance_work_mem,
  -- max_parallel_maintenance_workers, synchronous_commit) once at
  -- the top so every subsequent index build / VACUUM / ANALYZE in
  -- this pass picks them up.
  timedTrace_ prepComponent "session GUCs" $
    setPrepSessionGUCs tuning

  -- The four UPDATEs and two CTE backfills that follow all join
  -- through tx.hash or tx_out (tx_id, index). Building those indexes
  -- here, before any UPDATE runs, lets PG pick Nested Loop / Index
  -- Scan plans instead of hash-joining the whole heaps.
  timedTrace_ prepComponent "pre-resolve indexes"
    PreResolveIndexes.createPreResolveIndexes

  _ <- Resolve.resolveForeignKeys

  -- Refresh planner statistics for every table the backfills read.
  -- Autovacuum runs on UNLOGGED tables but its last sample was
  -- taken mid-ingest, before the resolve UPDATEs rewrote the row
  -- shape. Without this pass the planner sees pre-resolve
  -- cardinalities for tx_out / tx_in / collateral_tx_in and picks
  -- Nested Loop plans whose outer-side estimate is off by orders
  -- of magnitude.
  timedTrace_ prepComponent "ANALYZE for backfill planner stats" $
    for_ backfillAnalyzeTables $ \td ->
      runDdl (analyzeSql (tdName td))

  _ <- Backfill.backfillTxColumns
  _ <- Backfill.applyDepositPending
  timedTrace_ prepComponent "truncate epoch_param_pending"
    Backfill.truncateDepositPending

  -- Resolve UPDATEs leave tx_out and tx_in at roughly 50 % dead
  -- tuples. Reclaiming them now keeps the step-9 heap rewrite from
  -- carrying them across.
  timedTrace_ prepComponent "VACUUM tx_out / tx_in" $
    for_ preFlipVacuumTables $ \td ->
      runDdl (vacuumSql (tdName td))

  withPrepPool connSettings tuning (ptPoolSize tuning) $
    Indexes.createIndexes tables

  timedTrace_ prepComponent "flip UNLOGGED \x2192 LOGGED + attach sequences" $
    withPrepPool connSettings tuning (ptPoolSize tuning) $
      withRunInIO $ \runM ->
        forConcurrently_ (filter ((== TableUnlogged) . tdMode) tables) $ \td ->
          runM $ usePool ("flip " <> tdName td) $
            traverse_ Sess.script (perTableSchemaForFollowTipSql td)

  timedTrace_ prepComponent "ANALYZE per table" $
    for_ tables $ \td -> runDdl (analyzeSql (tdName td))

  timedTrace_ prepComponent "reset sequences" $
    Sequences.resetSequences tables

  liftIO $ traceWith tracer $ LogMsg Info prepComponent "post-load pass: complete" Nothing

-- | Tables the four backfill UPDATEs read or write. Listed once
-- here so the post-resolve ANALYZE pass and the backfill writers
-- agree on which tables need fresh statistics.
backfillAnalyzeTables :: [TableDef]
backfillAnalyzeTables =
  [ blockTableDef
  , txTableDef
  , txInTableDef
  , txOutTableDef
  , collateralTxInTableDef
  , collateralTxOutTableDef
  , withdrawalTableDef
  ]

-- | Tables the resolve UPDATEs leave heavily bloated. VACUUMed
-- between backfill and the LOGGED flip so the heap rewrite doesn't
-- copy dead tuples.
preFlipVacuumTables :: [TableDef]
preFlipVacuumTables = [txOutTableDef, txInTableDef]

runDdl
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Text -> m ()
runDdl ddl = do
  conn <- asks getHasqlConnection
  result <- liftIO $ Conn.use conn (Sess.script ddl)
  case result of
    Right () -> pure ()
    Left  e  -> panic $
      "Phase.Preparing.Run.runDdl: " <> show e <> " for " <> ddl
