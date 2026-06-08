-- | Hasql 'Statement' binding for the @pot_reward@ table.
--
-- IDENTITY leaf: PostgreSQL allocates the id from the backing
-- sequence at INSERT time. @earned_epoch@ is a generated column
-- computed by PG from @spendable_epoch@.
module DbSync.Db.Statement.PotReward
  ( insertPotRewardRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.StakeDelegation
  ( PotReward
  , potRewardEncoder
  , potRewardTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertPotRewardRowStmt :: Stmt.Statement PotReward ()
insertPotRewardRowStmt =
  Stmt.preparable (insertRowSql potRewardTableDef) potRewardEncoder D.noResult
