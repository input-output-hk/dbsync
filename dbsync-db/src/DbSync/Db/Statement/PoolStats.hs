-- | Hasql 'Statement' bindings for the @pool_stats@ extractor table:
-- @pool_stat@.
module DbSync.Db.Statement.PoolStats
  ( insertPoolStatRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Pool (PoolStat, poolStatEncoder, poolStatTableDef)
import DbSync.Db.Statement.Common (upsertRowSql)

-- ---------------------------------------------------------------------------
-- * pool_stat
-- ---------------------------------------------------------------------------

-- | Upserts on @(pool_hash_id, epoch_no)@ so a rollback that
-- re-crosses the boundary refreshes the pool's row for that epoch.
insertPoolStatRowStmt :: Stmt.Statement PoolStat ()
insertPoolStatRowStmt =
  Stmt.preparable (upsertRowSql poolStatTableDef) poolStatEncoder D.noResult
