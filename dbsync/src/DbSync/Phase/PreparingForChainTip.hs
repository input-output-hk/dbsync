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
-- 'run' walks all of that in a single pass against an open hasql
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
module DbSync.Phase.PreparingForChainTip
  ( run
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Schema.Core (blockTableDef, txTableDef)
import DbSync.Db.Schema.Init
  ( analyzeSql
  , prepareSchemaForFollowTipSql
  )
import DbSync.Db.Schema.StakeDelegation (withdrawalTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Schema.UTxO
  ( collateralTxInTableDef
  , collateralTxOutTableDef
  , txInTableDef
  , txOutTableDef
  )
import qualified DbSync.Phase.PreparingForChainTip.Backfill as Backfill
import qualified DbSync.Phase.PreparingForChainTip.Indexes as Indexes
import qualified DbSync.Phase.PreparingForChainTip.PreResolveIndexes as PreResolveIndexes
import qualified DbSync.Phase.PreparingForChainTip.Resolve as Resolve
import qualified DbSync.Phase.PreparingForChainTip.Sequences as Sequences
import DbSync.Trace.Timing (timedTrace_)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- | Component name used in trace lines emitted by this module and
-- its sub-modules.
prepComponent :: Text
prepComponent = "PreparingForChainTip"

-- | Run the full post-load sequence against the supplied connection.
--
-- Steps, in order:
--
--   1. Build the minimum index set on the tables the backfill UPDATEs
--      probe (tx, tx_out, tx_in, collateral_tx_in, collateral_tx_out,
--      withdrawal). Done non-@CONCURRENTLY@ while the tables are still
--      UNLOGGED so the build is one-pass and WAL-free.
--   2. Resolve @tx_in@ / @collateral_tx_in@ / @reference_tx_in@'s
--      @tx_out_id@ columns and @tx_out.consumed_by_tx_id@ — these
--      are the joins the step-1 indexes accelerate.
--   3. @ANALYZE@ the tables touched by step 2 so the backfill UPDATEs
--      see post-resolve cardinalities. Stale stats here mean the
--      planner picks Nested Loop with a tiny outer estimate and the
--      UPDATEs take hours.
--   4. Backfill @tx.fee@ on phase-2 failures and Byron txs, then
--      @tx.deposit@ on both phase-2 failures and valid-contract txs
--      (the ledger-disabled fallback). Phase-2 fee and Byron fee
--      drive off the small filtered set and look up inputs via the
--      step-1 indexes; the valid-contract deposit retains the
--      aggregate-then-join shape because every valid tx needs the
--      computation in ledger-disabled mode.
--   5. Apply the per-epoch protocol-param deposits accumulated
--      during ingest to @pool_update.deposit@ (first registrations)
--      and @stake_registration.deposit@ (Shelley-Babbage rows);
--      then TRUNCATE the staging table.
--   6. Flip every UNLOGGED table to LOGGED and attach an
--      @<table>_id_seq@.
--   7. @CREATE INDEX CONCURRENTLY@ for every PK and unique
--      constraint declared on the schema. The step-1 indexes are
--      deduped here via @IF NOT EXISTS@ on a matching name.
--   8. @ANALYZE@ each table so the planner picks up the new shape.
--   9. @setval@ each @<table>_id_seq@ to @MAX(id) + 1@.
--
-- Each step is a single SQL operation per table or per concern;
-- there is no application-level batching or per-epoch chunking
-- here. The per-epoch variant is a future option behind a feature
-- flag.
run :: AppTracer -> Conn.Connection -> [TableDef] -> IO ()
run tracer conn tables = do
  traceWith tracer $ LogMsg Info prepComponent "post-load pass: started" Nothing

  -- The four UPDATEs and two CTE backfills that follow all join
  -- through tx.hash or tx_out (tx_id, index). Building those indexes
  -- here, before any UPDATE runs, lets PG pick Nested Loop / Index
  -- Scan plans instead of hash-joining the whole heaps.
  timedTrace_ tracer prepComponent "pre-resolve indexes" $
    PreResolveIndexes.createPreResolveIndexes tracer conn

  _ <- Resolve.resolveForeignKeys tracer conn

  -- Refresh planner statistics for every table the backfills read.
  -- Autovacuum runs on UNLOGGED tables but its last sample was
  -- taken mid-ingest, before the four resolve UPDATEs each rewrote
  -- tens of millions of rows. Without this pass the planner sees
  -- pre-resolve cardinalities for tx_out / tx_in / collateral_tx_in
  -- and picks Nested Loop plans whose outer-side estimate is off by
  -- orders of magnitude, which translates into hour-scale UPDATEs.
  timedTrace_ tracer prepComponent "ANALYZE for backfill planner stats" $
    for_ backfillAnalyzeTables $ \td ->
      runDdl conn (analyzeSql (tdName td))

  _ <- Backfill.backfillTxColumns tracer conn
  _ <- Backfill.applyDepositPending tracer conn
  timedTrace_ tracer prepComponent "truncate epoch_param_pending" $
    Backfill.truncateDepositPending conn

  timedTrace_ tracer prepComponent "flip UNLOGGED \x2192 LOGGED + attach sequences" $
    for_ (prepareSchemaForFollowTipSql tables) (runDdl conn)

  Indexes.createIndexes tracer conn tables

  timedTrace_ tracer prepComponent "ANALYZE per table" $
    for_ tables $ \td -> runDdl conn (analyzeSql (tdName td))

  timedTrace_ tracer prepComponent "reset sequences" $
    Sequences.resetSequences conn tables

  traceWith tracer $ LogMsg Info prepComponent "post-load pass: complete" Nothing

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

runDdl :: Conn.Connection -> Text -> IO ()
runDdl conn ddl = do
  result <- Conn.use conn (Sess.script ddl)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "PreparingForChainTip: " <> show e <> " for " <> ddl
