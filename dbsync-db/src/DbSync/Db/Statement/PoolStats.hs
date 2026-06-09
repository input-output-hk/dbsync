-- | Hasql 'Statement' bindings for the @pool_stats@ extractor table:
-- @pool_stat@.
module DbSync.Db.Statement.PoolStats
  ( insertPoolStatRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Pool (PoolStat, poolStatEncoder, poolStatTableDef)
import DbSync.Db.Statement.Common (insertRowSql)

-- ---------------------------------------------------------------------------
-- * pool_stat
-- ---------------------------------------------------------------------------

insertPoolStatRowStmt :: Stmt.Statement PoolStat ()
insertPoolStatRowStmt =
  Stmt.preparable (insertRowSql poolStatTableDef) poolStatEncoder D.noResult
