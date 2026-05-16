-- | The runtime sync-phase value.
--
-- Shared between the orchestrator FSM and the
-- @epoch_sync_stats.phase@ column so they cannot drift.
module DbSync.Db.Phase
  ( -- * Phase
    SyncPhase (..)

    -- * Predicates
  , isFollowPath
  , isIngestPath

    -- * Rendering
  , renderSyncPhase
  , parseSyncPhase
  ) where

import Cardano.Prelude

-- | The four lifecycle phases.
--
-- 'FollowingVolatileTail' and 'FollowingChainTip' share the same code
-- path; the split exists so the log makes clear whether the consumer
-- has caught up with the receiver yet.
data SyncPhase
  = IngestChainHistory
  | PreparingForVolatileTail
  | FollowingVolatileTail
  | FollowingChainTip
  deriving stock (Eq, Show, Bounded, Enum)

-- | True for phases where extractors write via INSERT (so inline
-- collateral diffs and ledger-disabled deposit identities are
-- computed per block rather than deferred to a post-load backfill).
isFollowPath :: SyncPhase -> Bool
isFollowPath FollowingVolatileTail = True
isFollowPath FollowingChainTip     = True
isFollowPath _                     = False

-- | True for the bulk-load COPY pipeline.
isIngestPath :: SyncPhase -> Bool
isIngestPath IngestChainHistory = True
isIngestPath _                  = False

-- | Used as the log component name and as the @epoch_sync_stats.phase@
-- column value.
renderSyncPhase :: SyncPhase -> Text
renderSyncPhase = \case
  IngestChainHistory       -> "IngestChainHistory"
  PreparingForVolatileTail -> "PreparingForVolatileTail"
  FollowingVolatileTail    -> "FollowingVolatileTail"
  FollowingChainTip        -> "FollowingChainTip"

-- | Inverse of 'renderSyncPhase'.
parseSyncPhase :: Text -> Maybe SyncPhase
parseSyncPhase = \case
  "IngestChainHistory"       -> Just IngestChainHistory
  "PreparingForVolatileTail" -> Just PreparingForVolatileTail
  "FollowingVolatileTail"    -> Just FollowingVolatileTail
  "FollowingChainTip"        -> Just FollowingChainTip
  _                          -> Nothing
