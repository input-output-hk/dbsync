{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : DbSync.Env
Description : Environment records for the three sync phases.

Defines 'CoreEnv' (shared configuration), 'IngestEnv' (bulk COPY
phase), and 'FollowEnv' (live chain-following phase). Orphan
instances for 'HasTracer', 'HasMetrics', 'HasConfig', 'HasExtractors',
'HasResolver', and 'HasWriter' are defined here to avoid circular
imports between the class-defining modules and the concrete
environment types.

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

import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import DbSync.Config.Types (NodeConfig, SyncConfig)
import DbSync.Copy.Writer (CopyWriter)
import DbSync.Extractor (ExtractState, ExtractorDef, HasExtractors (..))
import DbSync.Id.DedupMap (DedupMaps)
import DbSync.Ingest.ReceiverStats (ReceiverStats)
import DbSync.Ledger.Types (HasLedgerEnv, LedgerEnv (..))
import DbSync.Metrics (HasMetrics (..), Metrics)
import DbSync.Resolver (HasResolver (..), IdResolver)
import DbSync.StateQuery.Types (StateQueryVar)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer)
import DbSync.Writer (HasWriter (..), Writer)

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
-- Extends 'CoreEnv' with the runtime state needed for bulk COPY
-- ingestion: the raw block queue, COPY connections, ID counters,
-- mutable dedup hash tables, the resolver and writer adapters,
-- the state-query interpreter handle, the system start, and the
-- ledger subsystem 'HasLedgerEnv' which is either
-- @LedgerEnabled !LedgerEnv@ (carrying its own block queue +
-- epoch-coordination 'TMVar's + snapshot queue) or
-- @LedgerDisabled !NoLedgerEnv@ (allocating nothing ledger-stateful).
--
-- The block queue carries raw 'CardanoBlock' values; parsing into
-- 'DbSync.Block.Types.GenericBlock' happens inside the consumer so
-- the receiver thread doesn't pay parsing cost on the hot path.
data IngestEnv = IngestEnv
  { ieCore          :: !CoreEnv
    -- ^ Shared core environment
  , ieBlockQueue    :: !(TBQueue (CardanoBlock StandardCrypto))
    -- ^ Blocks received from the node, awaiting parse + extraction
  , ieCopyWriter    :: !CopyWriter
    -- ^ Multi-threaded COPY writer (per-table TBQueues + worker threads)
  , ieDedupMaps     :: !DedupMaps
    -- ^ Mutable deduplication maps (internally mutable hash tables)
  , ieHasLedgerEnv  :: !HasLedgerEnv
    -- ^ Ledger subsystem — either enabled (carrying its own queues,
    -- 'LedgerDB', and snapshot machinery) or disabled (minimal).
  , ieStateQueryVar :: !StateQueryVar
    -- ^ Handle for the LocalStateQuery 'Interpreter' used to compute
    -- 'SlotDetails' (epoch number, slot-within-epoch, slot time) on
    -- the consumer thread.
  , ieSystemStart   :: !SystemStart
    -- ^ Network system-start time, sourced from the Shelley genesis.
    -- Required by the state-query interpreter to compute slot times.
  , ieResolver      :: !(IdResolver IO)
    -- ^ Ingest-phase ID resolver (DedupMaps + IdCounters under the hood).
    -- Built once from 'ieDedupMaps' and 'ieExtractState' at startup.
  , ieWriter        :: !(Writer IO)
    -- ^ Ingest-phase writer (the COPY adapter). Built once from
    -- 'ieCopyWriter' at startup.
  , ieExtractState  :: !(IORef ExtractState)
    -- ^ Per-block extraction state — carries the 'IdCounters' through
    -- 'atomicModifyIORef'' so the resolver can hand out fresh IDs.
  , ieReceiverStats :: !ReceiverStats
    -- ^ Receiver-thread statistics (blocks received, writes blocked).
    -- Mutated by the chainsync receiver, read+reset per epoch by the
    -- consumer for the @Ingest:@ log line. See
    -- 'DbSync.Ingest.ReceiverStats' for rationale.
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

instance HasTracer LedgerEnv where
  getTracer = leTracer

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

-- ---------------------------------------------------------------------------
-- * HasExtractors instances (orphan — class defined in DbSync.Extractor)
-- ---------------------------------------------------------------------------

instance HasExtractors CoreEnv where
  getExtractors = ceExtractors

instance HasExtractors IngestEnv where
  getExtractors = getExtractors . ieCore

instance HasExtractors FollowEnv where
  getExtractors = getExtractors . feCore

-- ---------------------------------------------------------------------------
-- * HasResolver instances (orphan — class defined in DbSync.Resolver)
-- ---------------------------------------------------------------------------

instance HasResolver IngestEnv where
  getResolver = ieResolver

-- NOTE: 'FollowEnv' will gain a 'HasResolver' instance once the
-- 'FollowingChainTip' SELECT/INSERT resolver lands.

-- ---------------------------------------------------------------------------
-- * HasWriter instances (orphan — class defined in DbSync.Writer)
-- ---------------------------------------------------------------------------

instance HasWriter IngestEnv where
  getWriter = ieWriter

-- NOTE: 'FollowEnv' will gain a 'HasWriter' instance once the
-- 'FollowingChainTip' INSERT writer lands.
