{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : DbSync.Env
Description : Environment records for the three sync phases.

Defines 'CoreEnv' (shared configuration), 'IngestEnv' (bulk COPY phase),
and 'FollowEnv' (live chain-following phase). Orphan instances for
'HasTracer', 'HasMetrics', and 'HasConfig' are defined here to avoid
circular imports between the class-defining modules and the concrete
environment types.
-}
module DbSync.Env
  ( -- * Environment types
    CoreEnv (..)
  , IngestEnv (..)
  , FollowEnv (..)

    -- * Placeholder types
  , LedgerState

    -- * Accessor classes
  , HasConfig (..)
  ) where

import Cardano.Prelude

import Cardano.Slotting.Slot (EpochNo)
import Control.Concurrent.STM (TBQueue, TMVar, TVar)
import Data.IORef (IORef)

import DbSync.Block.Types (GenericBlock)
import DbSync.Config.Types (NodeConfig, SyncConfig)
import DbSync.Id.Counter (IdCounters)
import DbSync.Id.DedupMap (DedupMaps)
import DbSync.Metrics (HasMetrics (..), Metrics)
import DbSync.Projection (ProjectionDef)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer)
import DbSync.Db.Writer.Copy (CopyConnections)

-- ---------------------------------------------------------------------------
-- * Placeholder types
-- ---------------------------------------------------------------------------

-- | Placeholder for the full ledger state.
-- Will be replaced with the real Ouroboros ledger state type once the
-- ledger worker is implemented.
type LedgerState = ()

-- ---------------------------------------------------------------------------
-- * Accessor classes
-- ---------------------------------------------------------------------------

-- | Access the sync configuration from any environment.
class HasConfig env where
  getConfig :: env -> SyncConfig

-- ---------------------------------------------------------------------------
-- * Environment types
-- ---------------------------------------------------------------------------

-- | Shared core environment available in every phase.
--
-- Contains the tracer, metrics handles, configuration, and the list
-- of active projections. Constructed once at startup.
data CoreEnv = CoreEnv
  { ceTracer      :: !AppTracer
      -- ^ Structured logging tracer (contra-tracer)
  , ceMetrics     :: !Metrics
      -- ^ Prometheus metrics handles
  , ceConfig      :: !SyncConfig
      -- ^ Parsed db-sync configuration
  , ceNodeConfig  :: !NodeConfig
      -- ^ Extracted cardano-node configuration
  , ceProjections :: ![ProjectionDef]
      -- ^ Active projection definitions
  }

-- | Environment for the 'IngestChainHistory' phase.
--
-- Extends 'CoreEnv' with mutable state needed for bulk COPY ingestion:
-- block queues, COPY connections, ID counters, dedup maps, and an
-- epoch-boundary signal.
data IngestEnv = IngestEnv
  { ieCore        :: !CoreEnv
      -- ^ Shared core environment
  , ieBlockQueue  :: !(TBQueue GenericBlock)
      -- ^ Blocks received from the node, awaiting extraction
  , ieLedgerQueue :: !(TBQueue GenericBlock)
      -- ^ Blocks forwarded to the ledger state worker
  , ieCopyConns   :: !CopyConnections
      -- ^ Per-table COPY protocol connections
  , ieDedupMaps   :: !(IORef DedupMaps)
      -- ^ Mutable deduplication maps (updated each block)
  , ieIdCounters  :: !(IORef IdCounters)
      -- ^ Mutable ID counters (updated each block)
  , ieEpochSignal :: !(TMVar EpochNo)
      -- ^ Signal for epoch-boundary commits
  }

-- | Environment for the 'FollowingChainTip' phase.
--
-- Lighter than 'IngestEnv' — no COPY connections or dedup maps.
-- Uses per-block INSERT with rollback support.
data FollowEnv = FollowEnv
  { feCore        :: !CoreEnv
      -- ^ Shared core environment
  , feLedgerState :: !(TVar LedgerState)
      -- ^ Current ledger state (for epoch calculations, rewards, etc.)
  }

-- ---------------------------------------------------------------------------
-- * HasTracer instances (orphan — class defined in DbSync.Trace)
-- ---------------------------------------------------------------------------

instance HasTracer CoreEnv where
  getTracer = ceTracer

instance HasTracer IngestEnv where
  getTracer = getTracer . ieCore

instance HasTracer FollowEnv where
  getTracer = getTracer . feCore

-- ---------------------------------------------------------------------------
-- * HasMetrics instances (orphan — class defined in DbSync.Metrics)
-- ---------------------------------------------------------------------------

instance HasMetrics CoreEnv where
  getMetrics = ceMetrics

instance HasMetrics IngestEnv where
  getMetrics = getMetrics . ieCore

instance HasMetrics FollowEnv where
  getMetrics = getMetrics . feCore

-- ---------------------------------------------------------------------------
-- * HasConfig instances
-- ---------------------------------------------------------------------------

instance HasConfig CoreEnv where
  getConfig = ceConfig

instance HasConfig IngestEnv where
  getConfig = getConfig . ieCore

instance HasConfig FollowEnv where
  getConfig = getConfig . feCore
