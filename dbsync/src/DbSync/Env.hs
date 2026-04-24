{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : DbSync.Env
Description : Environment records for the three sync phases.

Defines 'CoreEnv' (shared configuration), 'IngestEnv' (bulk COPY
phase), and 'FollowEnv' (live chain-following phase). Orphan
instances for 'HasTracer', 'HasMetrics', and 'HasConfig' are defined
here to avoid circular imports between the class-defining modules
and the concrete environment types.

The ledger feature is represented as a sum type 'HasLedgerEnv'
carried on 'IngestEnv' (see 'DbSync.Ledger.Types'); the block queue
and epoch-coordination 'TMVar's for the 'LedgerWorker' thread live
inside that type, so the ledger-disabled path allocates none of it.
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

import Control.Concurrent.STM (TBQueue, TVar)
import Data.IORef (IORef)

import DbSync.Block.Types (GenericBlock)
import DbSync.Config.Types (NodeConfig, SyncConfig)
import DbSync.Copy.Writer (CopyWriter)
import DbSync.Extractor (ExtractorDef)
import DbSync.Id.Counter (IdCounters)
import DbSync.Id.DedupMap (DedupMaps)
import DbSync.Ledger.Types (HasLedgerEnv)
import DbSync.Metrics (HasMetrics (..), Metrics)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer)

-- NOTE: DedupMaps is internally mutable (BasicHashTable + IORef counters).
-- No IORef wrapper needed — the hash tables are mutated in-place.

-- ---------------------------------------------------------------------------
-- * Placeholder types
-- ---------------------------------------------------------------------------

-- | Placeholder for the 'FollowingChainTip' ledger state.
--
-- This will be replaced with a proper
-- 'DbSync.Ledger.Types.DbSyncStateRef' reference once the FCT
-- transition wires the inline-apply path.
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
-- of active extractors. Constructed once at startup.
data CoreEnv = CoreEnv
  { ceTracer     :: !AppTracer
    -- ^ Structured logging tracer (contra-tracer)
  , ceMetrics    :: !Metrics
    -- ^ Prometheus metrics handles
  , ceConfig     :: !SyncConfig
    -- ^ Parsed db-sync configuration
  , ceNodeConfig :: !NodeConfig
    -- ^ Extracted cardano-node configuration
  , ceExtractors :: ![ExtractorDef]
    -- ^ Active extractor definitions
  }

-- | Environment for the 'IngestChainHistory' phase.
--
-- Extends 'CoreEnv' with mutable state needed for bulk COPY
-- ingestion: block queue, COPY connections, ID counters, mutable
-- dedup hash tables, plus the ledger subsystem 'HasLedgerEnv'
-- which is either @LedgerEnabled !LedgerEnv@ (carrying its own
-- block queue + epoch-coordination 'TMVar's + snapshot queue) or
-- @LedgerDisabled !NoLedgerEnv@ (allocating nothing ledger-stateful).
data IngestEnv = IngestEnv
  { ieCore         :: !CoreEnv
    -- ^ Shared core environment
  , ieBlockQueue   :: !(TBQueue GenericBlock)
    -- ^ Blocks received from the node, awaiting extraction
  , ieCopyWriter   :: !CopyWriter
    -- ^ Multi-threaded COPY writer (per-table TBQueues + worker threads)
  , ieDedupMaps    :: !DedupMaps
    -- ^ Mutable deduplication maps (internally mutable hash tables)
  , ieIdCounters   :: !(IORef IdCounters)
    -- ^ Mutable ID counters (updated each block)
  , ieHasLedgerEnv :: !HasLedgerEnv
    -- ^ Ledger subsystem — either enabled (carrying its own queues,
    -- 'LedgerDB', and snapshot machinery) or disabled (minimal).
  }

-- | Environment for the 'FollowingChainTip' phase.
--
-- Lighter than 'IngestEnv' — no COPY connections or dedup maps.
-- Uses per-block INSERT with rollback support.
data FollowEnv = FollowEnv
  { feCore        :: !CoreEnv
    -- ^ Shared core environment
  , feLedgerState :: !(TVar LedgerState)
    -- ^ Current ledger state (placeholder until the inline-apply path lands).
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
