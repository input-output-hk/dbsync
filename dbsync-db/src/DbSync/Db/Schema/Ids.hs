-- | Newtype wrappers for database primary keys.
--
-- Ported from @Cardano.Db.Schema.Ids@ in the original cardano-db-sync.
-- Each table's primary key gets its own newtype around 'Int64', providing
-- type safety so that a 'BlockId' cannot accidentally be used where a
-- 'TxId' is expected.
--
-- Only the IDs needed by the Core extractor tables are defined here.
-- Additional IDs (for UTxO, MultiAsset, etc.) are added as their
-- corresponding extractors are implemented.
module DbSync.Db.Schema.Ids
  ( -- * Core table IDs
    BlockId (..)
  , TxId (..)
  , SlotLeaderId (..)

    -- * Referenced by Core tables
  , PoolHashId (..)
  ) where

import Cardano.Prelude

-- ---------------------------------------------------------------------------
-- * Core table IDs
-- ---------------------------------------------------------------------------

-- | Primary key for the @block@ table.
newtype BlockId = BlockId { getBlockId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @tx@ table.
newtype TxId = TxId { getTxId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- | Primary key for the @slot_leader@ table.
newtype SlotLeaderId = SlotLeaderId { getSlotLeaderId :: Int64 }
  deriving stock (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- * Referenced by Core tables
-- ---------------------------------------------------------------------------

-- | Primary key for the @pool_hash@ table.
-- Referenced by 'SlotLeader.slotLeaderPoolHashId'.
-- The @pool_hash@ table itself is owned by the StakeDelegation extractor.
newtype PoolHashId = PoolHashId { getPoolHashId :: Int64 }
  deriving stock (Eq, Ord, Show)
