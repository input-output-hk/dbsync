-- | Hasql 'Statement' binding for the @pool_stat@ table.
--
-- IDENTITY leaf: PostgreSQL allocates the id from the backing
-- sequence at INSERT time, so the encoder takes the bare row.
module DbSync.Db.Statement.PoolStat
  ( insertPoolStatRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Pool
  ( PoolStat
  , poolStatEncoder
  , poolStatTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertPoolStatRowStmt :: Stmt.Statement PoolStat ()
insertPoolStatRowStmt =
  Stmt.preparable (insertRowSql poolStatTableDef) poolStatEncoder D.noResult
