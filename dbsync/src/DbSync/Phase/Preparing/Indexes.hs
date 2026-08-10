{-# LANGUAGE OverloadedStrings #-}

-- | Run @CREATE INDEX@ for every PK, unique constraint, and FK or
-- scope index the supplied tables declare.
--
-- The builds are non-concurrent. This pass runs between Ingest and
-- Follow, so no other session holds the tables and @ShareLock@ is
-- free. A non-concurrent build gets the full
-- @max_parallel_maintenance_workers@ parallelism and skips the
-- second validation scan @CONCURRENTLY@ forces.
--
-- The fan-out is per index, not per table: @ShareLock@ is
-- self-compatible, and a per-table fan-out would serialise the
-- largest table's indexes into the critical path.
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

-- | One 'step' log pair per index, so an operator sees which build
-- runs and how long each one takes. 'tableSizeRank' orders the
-- statements biggest-table-first, so the long builds start while the
-- pool still has spare backends.
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

-- | Heuristic size ordering for makespan scheduling. The tables that
-- dominate the pass go first, so they overlap everything else
-- instead of trailing it. @tx_cbor@ leads because it stores every
-- transaction's raw bytes. An unlisted table keeps its relative
-- order at the back.
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
