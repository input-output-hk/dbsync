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
import DbSync.Db.Statement.Common (insertIgnoreRowSql, insertRowSql)

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

-- | @ON CONFLICT DO NOTHING@: a rollback replay re-emits slices of
-- the same frozen snapshot, so retried rows are byte-identical.
insertEpochStakeRowStmt :: Stmt.Statement EpochStake ()
insertEpochStakeRowStmt =
  Stmt.preparable (insertIgnoreRowSql epochStakeTableDef) epochStakeEncoder D.noResult

-- ---------------------------------------------------------------------------
-- * epoch_stake_progress
-- ---------------------------------------------------------------------------

-- | Same replay tolerance as 'insertEpochStakeRowStmt'.
insertEpochStakeProgressRowStmt :: Stmt.Statement EpochStakeProgress ()
insertEpochStakeProgressRowStmt =
  Stmt.preparable
    (insertIgnoreRowSql epochStakeProgressTableDef)
    epochStakeProgressEncoder
    D.noResult
