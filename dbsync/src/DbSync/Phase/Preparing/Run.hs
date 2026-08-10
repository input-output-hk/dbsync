{-# LANGUAGE OverloadedStrings #-}

-- | One-time post-load pass between 'IngestChainHistory' and
-- 'FollowingChainTip'. Ingest leaves NULL FK columns, sentinel @tx@
-- columns, and UNLOGGED tables carrying only the resolve-support
-- indexes. 'run' resolves, backfills and reshapes all of it in one
-- ordered pass.
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
-- Each step logs through 'DbSync.Phase.Preparing.Step'.
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
  -- Set first, so every later index build and ANALYZE on the control
  -- connection picks them up. Pool backends set the same GUCs in
  -- their initSession hook.
  step TuningStep "session GUCs" $
    setPrepSessionGUCs tuning

  -- Scaffolding, so the resolve and backfill UPDATEs use index
  -- lookups instead of hash-joining the whole @tx@ and @tx_out@ heaps.
  PreResolveIndexes.createPreResolveIndexes

  Resolve.resolveInputTxOutIds

  -- The CTAS rebuild above drops the input-table indexes.
  PreResolveIndexes.createPostResolveIndexes

  -- Autovacuum samples UNLOGGED tables, but its last sample dates
  -- from mid-ingest. Without fresh statistics the planner picks
  -- Nested Loop plans on badly wrong estimates.
  -- 'Resolve.resolveInputTxOutIds' ANALYZEs the input tables it
  -- rebuilds.
  step AnalyzeStep "backfill input tables" $
    for_ (filter (hasTable . tdName) backfillAnalyzeTables) $ \td ->
      runDdl (analyzeSql (tdName td))

  _ <- Backfill.backfillTxColumns tables
  -- Needs both tables: the hash comes off the spent output that
  -- @tx_in@ points at, and lands on a @redeemer@ row.
  when (hasTable (tdName redeemerTableDef) && hasTable (tdName txInTableDef))
    Backfill.rebuildSpendScriptHash
  _ <- Backfill.applyDepositPending
  step CleanupStep "truncate epoch_param_pending"
    Backfill.truncateDepositPending

  -- No step below reads through the scaffolding indexes. Drop them
  -- before the flip: @ALTER TABLE … SET LOGGED@ rewrites the heap and
  -- rebuilds every index on the table inside the ALTER, so an index
  -- left alive gets built twice.
  step IndexStep "drop resolve-support indexes" $
    for_ resolveScaffoldingIndexNames (runDdl . dropIndexSql)

  step FlipStep "all UNLOGGED tables" $
    withPrepPool connSettings tuning (ptPoolSize tuning) $
      forPooled_ (ptPoolSize tuning) (flipOrder tables) $ \td ->
        step FlipStep (tdName td) $
          usePool ("flip " <> tdName td) $
            traverse_ Sess.script (perTableSchemaForFollowTipSql td)

  -- After the flip, so each Follow-facing index builds exactly once.
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

  -- After the index build and the ANALYZE, so each validation scan
  -- probes the parent's PK index and plans on current statistics.
  step ConstraintStep "add ownership foreign keys" $
    Constraints.addConstraints tables
  step ConstraintStep "validate ownership foreign keys" $
    withPrepPool connSettings tuning (ptPoolSize tuning) $
      Constraints.validateConstraints (ptPoolSize tuning) tables

  -- Last: the flip attaches the sequences, and @MAX(id)@ needs the PK
  -- indexes.
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

-- | Tables the backfill UPDATEs read or write, minus the CTAS-rebuilt
-- input tables. The call site filters this against the enabled set,
-- because a disabled extractor never creates its tables.
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
