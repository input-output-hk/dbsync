-- | Run @CREATE INDEX@ for every PK and unique constraint declared
-- on the supplied tables.
--
-- Builds are non-concurrent: this pass runs between Ingest exiting
-- and Follow starting, so no other session is touching the tables
-- and @ShareLock@ is free. Non-concurrent builds get the full
-- @max_parallel_maintenance_workers@ parallelism on every scan and
-- avoid the second validation scan that @CONCURRENTLY@ forces.
--
-- Fanned out across tables via the pool on env: each table's index
-- DDL runs on its own backend, up to the pool's bound. Per-index
-- timing logs survive because each 'timedTrace_' wrapper sits
-- outside its 'usePool' call.
module DbSync.Phase.Preparing.Indexes
  ( createIndexes
  ) where

import Cardano.Prelude

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.IO.Unlift (withRunInIO)
import qualified Hasql.Session as Sess

import DbSync.Db.Pool (PoolM, usePool)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Indexes (Concurrency (..), tableIndexStatements)
import DbSync.Trace.Timing (timedTrace_)

-- | One log line per index so an operator can see which build is
-- in flight. Tables build in parallel via the pool on env; the
-- per-table index DDL inside each thread runs in sequence on
-- whatever backend the pool hands it.
createIndexes :: [TableDef] -> PoolM ()
createIndexes tables = withRunInIO $ \run ->
  forConcurrently_ tables $ \td ->
    for_ (zip [1 :: Int ..] (tableIndexStatements NonConcurrent td)) $ \(i, ddl) ->
      run $
        timedTrace_ "PreparingForVolatileTail"
          ("index " <> tdName td <> " #" <> show i)
          (usePool ("index " <> tdName td) (Sess.script ddl))
