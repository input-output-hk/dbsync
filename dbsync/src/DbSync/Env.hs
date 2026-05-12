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
  , HasNetwork (..)
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (TBQueue, TVar)
import Control.Concurrent.STM.TBQueue (readTBQueue)
import qualified Data.Strict.Maybe as Strict
import Data.IORef (IORef)

import Cardano.Ledger.BaseTypes (Network)
import Cardano.Slotting.Slot (SlotNo)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)

import DbSync.Block.Types (CardanoPoint)
import DbSync.Checkpoint.SyncState (ControlConnection)
import DbSync.Config.Types (NodeConfig, SyncConfig)
import DbSync.Copy.Writer (CopyWriter)
import qualified DbSync.Era.Shelley.Generic.ProtoParams as Generic
import DbSync.Extractor
  ( BlockLedgerData (..)
  , ExtractState
  , ExtractorDef
  , HasExtractors (..)
  , HasLedgerData (..)
  , HasSyncPhase (..)
  , emptyBlockLedgerData
  )
import DbSync.Id.DedupMap (DedupMaps)
import DbSync.Ingest.ReceiverStats (ReceiverStats)
import DbSync.Ledger.Types
  ( ApplyResult (..)
  , HasLedgerEnv (..)
  , LedgerEnv (..)
  )
import DbSync.Metrics (HasMetrics (..), Metrics)
import DbSync.Phase (SyncPhase (..))
import DbSync.Resolver (HasResolver (..), IdResolver)
import DbSync.Resolver.AddressBuffer (AddressBufferRef)
import DbSync.Resolver.AddressWorker (AddressResolver)
import DbSync.StateQuery.Types (StateQueryVar)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer)
import DbSync.Watchdog (Watchdog)
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

-- | Access the chain's 'Network' (mainnet vs testnet) from any
-- environment. Read once at startup from the Shelley genesis and
-- never changes for the lifetime of a sync.
class HasNetwork env where
  getNetwork :: env -> Network

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
  , ceNetwork    :: !Network
    -- ^ Chain network ID, sourced from the Shelley genesis.
    --   Drives the HRP for stake / reward Bech32 encodings.
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
  , ieAddressBuffer :: !AddressBufferRef
    -- ^ Per-epoch buffer of address-resolution work for the
    -- 'ieAddressResolver' worker. The consumer hands the contents
    -- to the worker at each epoch boundary and resets the ref to
    -- empty.
  , ieAddressResolver :: !AddressResolver
    -- ^ Background worker that drains 'ieAddressBuffer' and writes
    -- @address@ rows / fills @tx_out.address_id@ FKs an epoch behind
    -- the main pipeline.
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
  , ieControlConnection :: !ControlConnection
    -- ^ PG connection used by the consumer to advance
    -- @dbsync_sync_state@ at each epoch boundary via 'commitEpoch'.
  , ieLastCommittedSlotAtBoot :: !(Maybe SlotNo)
    -- ^ Upper edge of the replay window for a ledger-enabled
    -- resume: the consumer skips 'processBlock' for any block at
    -- or before this slot (already in PG). 'Nothing' otherwise.
  , ieReplayStartSlot         :: !(Maybe SlotNo)
    -- ^ Lower edge of the replay window (the chosen snapshot's
    -- slot). Drives the percentage in the consumer\'s replay
    -- progress log. 'Nothing' otherwise.
  , ieWatchdog                :: !Watchdog
    -- ^ Per-thread liveness counters sampled by a background
    -- watchdog. Consumer + receiver bump via this handle; the
    -- ledger worker bumps via the same handle, passed explicitly
    -- to 'DbSync.Ledger.Worker.runLedgerWorker' because the worker
    -- runs under 'LedgerEnv' (no 'IngestEnv' in scope).
  , ieLatestReceivedPoint     :: !(IORef (Maybe CardanoPoint))
    -- ^ The latest chain point the receiver has accepted (forward
    -- or rollback). Read on every (re)connection so the chainsync
    -- client resumes at our current position rather than the
    -- boot-time intersect. Without this, a mid-run @cardano-node@
    -- restart causes the node to intersect at the boot-time point
    -- (Origin on a fresh sync) and roll our chain pointer back to
    -- genesis; the LedgerWorker then crashes with a hash mismatch
    -- when the genesis block arrives over our advanced state.
    -- 'Nothing' on first connection (before any block is received).
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

instance HasNetwork CoreEnv where
  getNetwork = ceNetwork

instance HasNetwork IngestEnv where
  getNetwork = getNetwork . ieCore

instance HasNetwork FollowEnv where
  getNetwork = getNetwork . feCore

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

-- ---------------------------------------------------------------------------
-- * HasLedgerData / HasSyncPhase instances
-- ---------------------------------------------------------------------------

-- | Pop the per-block apply result from the worker's
-- 'leAppliedQueue' and project the deposit data into a
-- 'BlockLedgerData'. Returns 'emptyBlockLedgerData' when the
-- ledger feature is disabled.
--
-- The block argument is unused: the queue's order matches the
-- consumer's order by construction.
instance HasLedgerData IngestEnv where
  getLedgerData env _block = case ieHasLedgerEnv env of
    LedgerDisabled _ -> pure emptyBlockLedgerData
    LedgerEnabled lenv -> do
      ar <- atomically $ readTBQueue (leAppliedQueue lenv)
      let mDeposits = case apDeposits ar of
            Strict.Just d  -> Just d
            Strict.Nothing -> Nothing
      pure BlockLedgerData
        { bldLedgerEnabled   = True
        , bldDepositsMap     = apDepositsMap ar
        , bldStakeKeyDeposit = Generic.stakeKeyDeposit <$> mDeposits
        , bldPoolDeposit     = Generic.poolDeposit <$> mDeposits
        }

instance HasSyncPhase IngestEnv where
  getSyncPhase _ = IngestChainHistory

-- | 'FollowEnv' currently has no ledger env; returns empty until
-- the Follow-side worker plumbing lands.
instance HasLedgerData FollowEnv where
  getLedgerData _ _ = pure emptyBlockLedgerData

instance HasSyncPhase FollowEnv where
  getSyncPhase _ = FollowingChainTip
