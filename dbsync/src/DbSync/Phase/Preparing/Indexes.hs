{-# LANGUAGE OverloadedStrings #-}

-- | Run @CREATE INDEX@ for every PK, unique constraint, and FK/scope
-- index declared on the supplied tables.
--
-- Builds are non-concurrent: this pass runs between Ingest exiting
-- and Follow starting, so no other session is touching the tables
-- and @ShareLock@ is free. Non-concurrent builds get the full
-- @max_parallel_maintenance_workers@ parallelism on every scan and
-- avoid the second validation scan that @CONCURRENTLY@ forces.
--
-- The fan-out is per /index/, not per table: concurrent
-- @CREATE INDEX@ on the same table is legal (@ShareLock@ is
-- self-compatible), and per-table fan-out would serialise the
-- largest table's indexes into the pass's critical path. Statements
-- are ordered biggest-table-first so the long builds start while
-- the pool still has spare backends.
module DbSync.Phase.Preparing.Indexes
  ( createIndexes
  , tableSizeRank
  ) where

import Cardano.Prelude

import Data.List (sortOn)
import qualified Hasql.Session as Sess

import DbSync.Db.Pool (PoolM, forPooled_, usePool)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Indexes
  ( Concurrency (..)
  , IndexStatement (..)
  , tableIndexStatements
  )
import DbSync.Phase.Preparing.Step (StepKind (..), step)

-- | One 'step' log pair per index so an operator can see which
-- build is in flight and how long each took.
createIndexes :: Int -> [TableDef] -> PoolM ()
createIndexes poolSize tables =
  forPooled_ poolSize prioritised $ \(tbl, ix) ->
    step IndexStep (isName ix) $
      usePool ("index " <> tbl) (Sess.script (isSql ix))
  where
    prioritised =
      sortOn (tableSizeRank . fst)
        [ (tdName td, ix)
        | td <- tables
        , ix <- tableIndexStatements NonConcurrent td
        ]

-- | Heuristic size ordering for makespan scheduling: the tables
-- whose builds typically dominate the pass go first so they overlap
-- with everything else instead of trailing behind it. @tx_cbor@
-- leads because it stores every transaction's raw bytes, making its
-- heap rewrite and index builds the longest single items. Unlisted
-- tables keep their relative order at the back.
tableSizeRank :: Text -> Int
tableSizeRank name = case name of
  "tx_cbor"     -> 0
  "tx_out"      -> 1
  "tx_in"       -> 2
  "ma_tx_out"   -> 3
  "address"     -> 4
  "tx"          -> 5
  "reward"      -> 6
  "epoch_stake" -> 7
  "tx_metadata" -> 8
  "datum"       -> 9
  "delegation"  -> 10
  _             -> 100
