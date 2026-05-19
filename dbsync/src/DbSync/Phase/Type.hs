-- | The runtime sync-phase value used by the orchestrator FSM, log
-- component names, and the @epoch_sync_stats.phase@ column.
module DbSync.Phase.Type
  ( -- * Phase
    SyncPhase (..)

    -- * Predicates
  , isFollowPath
  , isIngestPath

    -- * Rendering
  , renderPhase
  , parsePhase
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
renderPhase :: SyncPhase -> Text
renderPhase = \case
  IngestChainHistory       -> "IngestChainHistory"
  PreparingForVolatileTail -> "PreparingForVolatileTail"
  FollowingVolatileTail    -> "FollowingVolatileTail"
  FollowingChainTip        -> "FollowingChainTip"

-- | Inverse of 'renderPhase'.
parsePhase :: Text -> Maybe SyncPhase
parsePhase = \case
  "IngestChainHistory"       -> Just IngestChainHistory
  "PreparingForVolatileTail" -> Just PreparingForVolatileTail
  "FollowingVolatileTail"    -> Just FollowingVolatileTail
  "FollowingChainTip"        -> Just FollowingChainTip
  _                          -> Nothing
