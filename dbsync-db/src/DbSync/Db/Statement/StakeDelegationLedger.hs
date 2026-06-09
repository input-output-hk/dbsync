-- | Hasql 'Statement' bindings for the @stake_delegation_ledger@
-- extractor tables: @reward@, @pot_reward@, @epoch_stake@,
-- @epoch_stake_progress@.
--
-- All four are IDENTITY leaves — PostgreSQL allocates the id.
-- @reward.earned_epoch@ and @pot_reward.earned_epoch@ are generated
-- columns computed by PG from @spendable_epoch@ (and @type@ for
-- @reward@).
module DbSync.Db.Statement.StakeDelegationLedger
  ( -- * reward
    insertRewardRowStmt

    -- * pot_reward
  , insertPotRewardRowStmt

    -- * epoch_stake
  , insertEpochStakeRowStmt

    -- * epoch_stake_progress
  , insertEpochStakeProgressRowStmt
  ) where

import qualified Hasql.Decoders as D
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.StakeDelegation
  ( EpochStake
  , EpochStakeProgress
  , PotReward
  , Reward
  , epochStakeEncoder
  , epochStakeProgressEncoder
  , epochStakeProgressTableDef
  , epochStakeTableDef
  , potRewardEncoder
  , potRewardTableDef
  , rewardEncoder
  , rewardTableDef
  )
import DbSync.Db.Statement.Common (insertRowSql)

-- ---------------------------------------------------------------------------
-- * reward
-- ---------------------------------------------------------------------------

insertRewardRowStmt :: Stmt.Statement Reward ()
insertRewardRowStmt =
  Stmt.preparable (insertRowSql rewardTableDef) rewardEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * pot_reward
-- ---------------------------------------------------------------------------

insertPotRewardRowStmt :: Stmt.Statement PotReward ()
insertPotRewardRowStmt =
  Stmt.preparable (insertRowSql potRewardTableDef) potRewardEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * epoch_stake
-- ---------------------------------------------------------------------------

insertEpochStakeRowStmt :: Stmt.Statement EpochStake ()
insertEpochStakeRowStmt =
  Stmt.preparable (insertRowSql epochStakeTableDef) epochStakeEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * epoch_stake_progress
-- ---------------------------------------------------------------------------

insertEpochStakeProgressRowStmt :: Stmt.Statement EpochStakeProgress ()
insertEpochStakeProgressRowStmt =
  Stmt.preparable
    (insertRowSql epochStakeProgressTableDef)
    epochStakeProgressEncoder
    D.noResult
