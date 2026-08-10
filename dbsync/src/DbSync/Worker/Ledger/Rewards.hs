-- | Era-agnostic reward value types.
--
-- A 'Reward' is a plain @(source, pool, amount)@ triple, not the
-- era-specific @Cardano.Ledger.Reward@. 'RewardSource' comes from
-- 'DbSync.Db.Types' so the worker and the schema share one enum.
module DbSync.Worker.Ledger.Rewards
  ( -- * Reward source tag
    RewardSource (..)

    -- * Reward values
  , Reward (..)
  , Rewards (..)
  , PotReward (..)
  , PotRewards (..)

    -- * Helpers
  , rewardsCount
  , rewardTypeToSource
  ) where

import Cardano.Prelude

import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Rewards as Ledger
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import DbSync.Db.Types (RewardSource (..))
import DbSync.Worker.Ledger.Keys (PoolKeyHash, StakeCred)

-- ---------------------------------------------------------------------------
-- * Reward values
-- ---------------------------------------------------------------------------

-- | One reward entry. The same record in every era.
data Reward = Reward
  { rewardSource :: !RewardSource
  , rewardPool   :: !PoolKeyHash
  , rewardAmount :: !Word64
  }
  deriving stock (Eq, Ord, Show)

-- | Rewards keyed by stake credential.
--
-- Multiple 'Reward' entries per credential are possible (e.g. both a
-- member and a leader reward in the same epoch), hence the 'Set'.
newtype Rewards = Rewards
  { unRewards :: Map StakeCred (Set Reward)
  }
  deriving stock (Eq, Show)

-- | A pot-sourced payment: reserves, treasury or refund. Carries a
-- raw ledger 'Coin', not the 'Word64' that 'Reward' uses.
data PotReward = PotReward
  { prSource :: !RewardSource
  , prAmount :: !Coin
  }
  deriving stock (Eq, Ord, Show)

-- | Companion to 'Rewards' for the pot-payment flow.
--
-- The inner set carries 'Reward' values (not 'PotReward'): the
-- boundary converts before collecting, so the shapes match.
newtype PotRewards = PotRewards
  { unPotRewards :: Map StakeCred (Set Reward)
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * NFData instances
-- ---------------------------------------------------------------------------

instance NFData Reward where
  rnf (Reward a b c) = rnf (a, b, c)

instance NFData Rewards where
  rnf (Rewards m) = rnf m

instance NFData PotReward where
  rnf (PotReward a b) = rnf (a, b)

instance NFData PotRewards where
  rnf (PotRewards m) = rnf m

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Total 'Reward' entries across all stake credentials.
rewardsCount :: Rewards -> Int
rewardsCount = sum . map Set.size . Map.elems . unRewards

-- | Leader and member are the only two shapes the ledger emits through
-- its reward events. Reserves, treasury and refund sources arrive on
-- other paths (MIR, deposit refunds) and are built there.
rewardTypeToSource :: Ledger.RewardType -> RewardSource
rewardTypeToSource = \case
  Ledger.LeaderReward -> RwdLeader
  Ledger.MemberReward -> RwdMember
