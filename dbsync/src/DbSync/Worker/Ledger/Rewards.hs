{- |
Module      : DbSync.Worker.Ledger.Rewards
Description : Era-agnostic reward value types.

These are the era-collapsed shapes produced by the per-era
converters: a @'Reward'@ here is a plain @(source, pool, amount)@
triple, not the era-specific @'Cardano.Ledger.Reward'@ that comes out
of the ledger. Extractors and the event pipeline consume these
unified values without caring which era produced them.

@'RewardSource'@ is re-exported from 'DbSync.Db.Types' so the worker
and the schema agree on the single canonical enum.
-}
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

-- | A single reward entry: amount, earning pool, and origin.
--
-- Era-collapsed: this is the same record regardless of whether the
-- reward was produced in Shelley, Alonzo, or Conway.
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

-- | A pot-sourced payment — reserves \/ treasury \/ refund. Carries
-- a 'Coin' (raw ledger value) rather than the 'Word64' used by
-- 'Reward'.
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

-- | Total number of 'Reward' entries across all stake credentials.
rewardsCount :: Rewards -> Int
rewardsCount = sum . map Set.size . Map.elems . unRewards

-- | Map a @cardano-ledger@ 'Ledger.RewardType' onto our 'RewardSource'.
--
-- Leader and member rewards are the only two shapes the ledger emits
-- through its reward events; reserves \/ treasury \/ refund sources
-- appear via different code paths (MIR, deposit refunds, etc.) and
-- are constructed directly there.
rewardTypeToSource :: Ledger.RewardType -> RewardSource
rewardTypeToSource = \case
  Ledger.LeaderReward -> RwdLeader
  Ledger.MemberReward -> RwdMember
