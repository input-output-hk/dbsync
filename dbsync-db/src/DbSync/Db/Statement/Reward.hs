-- | Hasql 'Statement' binding for the @reward@ table.
--
-- IDENTITY leaf: PostgreSQL allocates the id from the backing
-- sequence at INSERT time. @earned_epoch@ is a generated column
-- computed by PG from @spendable_epoch@ + @type@.
module DbSync.Db.Statement.Reward
  ( insertRewardRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.StakeDelegation
  ( Reward
  , rewardEncoder
  , rewardTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

insertRewardRowStmt :: Stmt.Statement Reward ()
insertRewardRowStmt =
  Stmt.preparable (insertRowSql rewardTableDef) rewardEncoder D.noResult
