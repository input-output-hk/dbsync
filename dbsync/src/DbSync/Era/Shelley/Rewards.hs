{- |
Module      : DbSync.Era.Shelley.Rewards
Description : Era-agnostic reward value types.

These are the era-collapsed shapes produced by the per-era
converters: a @'Reward'@ here is a plain @(source, pool, amount)@
triple, not the era-specific @'Cardano.Ledger.Reward'@ that comes out
of the ledger. Extractors and the event pipeline consume these
unified values without caring which era produced them.

@'RewardSource'@ also lives here rather than in the DB package: the
ledger code only uses it for building
'DbSync.Ledger.Event.LedgerEvent' values, and the database encoding
(added when the @reward@ projection is wired up) can depend on this
module just as easily as the other way round.
-}
module DbSync.Era.Shelley.Rewards
  ( -- * Reward source tag
    RewardSource (..)

    -- * Reward values
  , Reward (..)
  , Rewards (..)
  , RewardRest (..)
  , RewardRests (..)

    -- * Helpers
  , rewardsCount
  , rewardTypeToSource
  ) where

import Cardano.Prelude

import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Rewards as Ledger
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import DbSync.Ledger.Keys (PoolKeyHash, StakeCred)

-- ---------------------------------------------------------------------------
-- * Reward source tag
-- ---------------------------------------------------------------------------

-- | The origin of a reward entry, carried by every 'Reward' and
-- 'RewardRest' value and ultimately written to the @reward@ table.
--
-- The DB enum encoding uses the @leader@ \/ @member@ \/ ... labels
-- derived from each constructor.
data RewardSource
  = RwdLeader
  | RwdMember
  | RwdReserves
  | RwdTreasury
  | RwdDepositRefund
  | RwdProposalRefund
  deriving stock (Bounded, Enum, Eq, Ord, Show)

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

-- | A \"reward rest\" payment — reserves \/ treasury \/ MIR
-- distributions. Carries a 'Coin' (raw ledger value) rather than the
-- 'Word64' used by 'Reward'.
data RewardRest = RewardRest
  { irSource :: !RewardSource
  , irAmount :: !Coin
  }
  deriving stock (Eq, Ord, Show)

-- | Companion to 'Rewards' for the instantaneous-reward flow.
--
-- The inner set carries 'Reward' values (not 'RewardRest'): the
-- boundary converts before collecting, so the shapes match.
newtype RewardRests = RewardRests
  { unIRewards :: Map StakeCred (Set Reward)
  }
  deriving stock (Eq, Show)

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
