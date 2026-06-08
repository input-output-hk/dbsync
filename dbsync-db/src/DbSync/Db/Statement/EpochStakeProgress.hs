-- | Hasql 'Statement' binding for the @epoch_stake_progress@ table.
--
-- IDENTITY leaf: PostgreSQL allocates the id from the backing
-- sequence at INSERT time.
module DbSync.Db.Statement.EpochStakeProgress
  ( insertEpochStakeProgressRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.StakeDelegation
  ( EpochStakeProgress
  , epochStakeProgressEncoder
  , epochStakeProgressTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertEpochStakeProgressRowStmt :: Stmt.Statement EpochStakeProgress ()
insertEpochStakeProgressRowStmt =
  Stmt.preparable
    (insertRowSql epochStakeProgressTableDef)
    epochStakeProgressEncoder
    D.noResult
