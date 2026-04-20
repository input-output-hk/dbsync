-- | Sync phase types.
--
-- Defines the three phases of the sync lifecycle:
-- 'IngestChainHistory' (bulk COPY), 'PreparingForChainTip' (indexes/constraints),
-- and 'FollowingChainTip' (INSERT-based live following).
module DbSync.Phase
  ( -- * Types
    SyncPhase (..)
  ) where

import Cardano.Prelude

-- * Types

-- | The three phases of the cardano-db-sync lifecycle.
--
-- During 'IngestChainHistory', finalized blocks are bulk-loaded via COPY
-- with UNLOGGED tables, no indexes, and epoch-aligned commits.
--
-- 'PreparingForChainTip' is a one-time transition that builds indexes,
-- constraints, enables WAL, and runs ANALYZE.
--
-- 'FollowingChainTip' processes volatile and new blocks via INSERT
-- with per-block commits and rollback support.
data SyncPhase
  = IngestChainHistory
  | PreparingForChainTip
  | FollowingChainTip
  deriving stock (Eq, Show, Bounded, Enum)
