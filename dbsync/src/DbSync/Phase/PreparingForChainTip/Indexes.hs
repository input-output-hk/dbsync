{-# LANGUAGE OverloadedStrings #-}

-- | Run @CREATE INDEX@ for every PK and unique constraint declared
-- on the supplied tables.
--
-- Builds are non-concurrent: this pass runs between Ingest exiting
-- and Follow starting, so no other session is touching the tables
-- and @ShareLock@ is free. Non-concurrent builds get the full
-- @max_parallel_maintenance_workers@ parallelism on every scan and
-- avoid the second validation scan that @CONCURRENTLY@ forces.
--
-- The pass is fanned out across tables via a 'Hasql.Pool.Pool':
-- each table's index DDL runs on its own backend, up to the pool's
-- bound. Per-index timing logs survive because each 'timedTrace_'
-- wrapper sits outside its 'usePool' call.
--
-- DDL builders live in 'DbSync.Db.Statement.Indexes'.
module DbSync.Phase.PreparingForChainTip.Indexes
  ( createIndexes
  ) where

import Cardano.Prelude

import Control.Concurrent.Async (forConcurrently_)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Sess

import DbSync.Db.Pool (usePool)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Indexes (Concurrency (..), tableIndexStatements)
import DbSync.Trace.Timing (timedTrace_)
import DbSync.Trace.Types (AppTracer)

-- | One log line per index so an operator can see which build is
-- in flight. Tables build in parallel via the supplied pool; the
-- per-table index DDL inside each thread runs in sequence on
-- whatever backend the pool hands it.
createIndexes :: AppTracer -> Pool.Pool -> [TableDef] -> IO ()
createIndexes tracer pool tables =
  forConcurrently_ tables $ \td ->
    for_ (zip [1 :: Int ..] (tableIndexStatements NonConcurrent td)) $ \(i, ddl) ->
      timedTrace_ tracer "PreparingForChainTip"
        ("index " <> tdName td <> " #" <> show i)
        (usePool pool ("index " <> tdName td) (Sess.script ddl))
