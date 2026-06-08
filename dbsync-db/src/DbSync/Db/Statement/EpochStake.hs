-- | Hasql 'Statement' binding for the @epoch_stake@ table.
--
-- IDENTITY leaf: PostgreSQL allocates the id from the backing
-- sequence at INSERT time.
module DbSync.Db.Statement.EpochStake
  ( insertEpochStakeRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.StakeDelegation
  ( EpochStake
  , epochStakeEncoder
  , epochStakeTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertEpochStakeRowStmt :: Stmt.Statement EpochStake ()
insertEpochStakeRowStmt =
  Stmt.preparable (insertRowSql epochStakeTableDef) epochStakeEncoder D.noResult
