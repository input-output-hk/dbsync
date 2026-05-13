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
module DbSync.Phase.PreparingForChainTip
  ( run
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Schema.Init
  ( analyzeSql
  , prepareSchemaForFollowTipSql
  )
import DbSync.Db.Schema.Types (TableDef (..))
import qualified DbSync.Phase.PreparingForChainTip.Backfill as Backfill
import qualified DbSync.Phase.PreparingForChainTip.Indexes as Indexes
import qualified DbSync.Phase.PreparingForChainTip.PreResolveIndexes as PreResolveIndexes
import qualified DbSync.Phase.PreparingForChainTip.Resolve as Resolve
import qualified DbSync.Phase.PreparingForChainTip.Sequences as Sequences

-- | Run the full post-load sequence against the supplied connection.
--
-- Steps, in order:
--
--   1. Build the minimum index set on @tx (hash)@, @tx_out (tx_id,
--      index)@ and @tx_in (tx_out_id, tx_out_index)@. Done
--      non-@CONCURRENTLY@ while the tables are still UNLOGGED so the
--      build is one-pass and WAL-free.
--   2. Resolve @tx_in@ / @collateral_tx_in@ / @reference_tx_in@'s
--      @tx_out_id@ columns and @tx_out.consumed_by_tx_id@ — these
--      are the joins the step-1 indexes accelerate.
--   3. Backfill @tx.fee@ on phase-2 failures, then @tx.deposit@ on
--      both phase-2 failures and valid-contract txs (the
--      ledger-disabled fallback). The CTE-driven shape of the
--      deposit fallback also benefits from the step-1 indexes.
--   4. Apply the per-epoch protocol-param deposits accumulated
--      during ingest to @pool_update.deposit@ (first registrations)
--      and @stake_registration.deposit@ (Shelley-Babbage rows);
--      then TRUNCATE the staging table.
--   5. Flip every UNLOGGED table to LOGGED and attach an
--      @<table>_id_seq@.
--   6. @CREATE INDEX CONCURRENTLY@ for every PK and unique
--      constraint declared on the schema. The step-1 indexes are
--      deduped here via @IF NOT EXISTS@ on a matching name.
--   7. @ANALYZE@ each table so the planner picks up the new shape.
--   8. @setval@ each @<table>_id_seq@ to @MAX(id) + 1@.
--
-- Each step is a single SQL operation per table or per concern;
-- there is no application-level batching or per-epoch chunking
-- here. The per-epoch variant is a future option behind a feature
-- flag.
run :: Conn.Connection -> [TableDef] -> IO ()
run conn tables = do
  -- The four UPDATEs and two CTE backfills that follow all join
  -- through tx.hash or tx_out (tx_id, index). Building those indexes
  -- here, before any UPDATE runs, lets PG pick Nested Loop / Index
  -- Scan plans instead of hash-joining the whole heaps.
  PreResolveIndexes.createPreResolveIndexes conn

  _ <- Resolve.resolveForeignKeys conn
  _ <- Backfill.backfillTxColumns conn
  _ <- Backfill.applyDepositPending conn
  Backfill.truncateDepositPending conn
  for_ (prepareSchemaForFollowTipSql tables) (runDdl conn)
  Indexes.createIndexes conn tables
  for_ tables $ \td -> runDdl conn (analyzeSql (tdName td))
  Sequences.resetSequences conn tables

runDdl :: Conn.Connection -> Text -> IO ()
runDdl conn ddl = do
  result <- Conn.use conn (Sess.script ddl)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "PreparingForChainTip: " <> show e <> " for " <> ddl
