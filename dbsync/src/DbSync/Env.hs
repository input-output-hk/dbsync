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

    -- * Follow construction
  , mkFollowEnvFromIngest

    -- * Accessor classes
  , HasConfig (..)
  , HasNetwork (..)  -- re-export from Extractor
  , HasReceiverChannels (..)
  , HasSecurityParam (..)

    -- * Small env adapters
    --
    -- Used by boot code and tests where no full phase env is in
    -- scope. Production phase envs satisfy the same classes directly
    -- via the records above.
  , TracerWithControl (..)
  , TracerWithConn (..)
  , LoaderWithControl (..)
  , CoreWithConn (..)
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (TBQueue, TVar)
import Data.IORef (IORef)
import qualified Hasql.Connection as Conn

import Cardano.Ledger.BaseTypes (Network)
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (SlotNo)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)

import DbSync.Block.Types (CardanoPoint)
import DbSync.Node.ChainSyncMsg (ChainSyncMsg)
import DbSync.Checkpoint.SyncState (ControlConnection, HasControlConnection (..))
import DbSync.Config.Types (NodeConfig, SyncConfig)
import DbSync.Db.Loader (LoaderStream, HasLoaderStream (..))
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Extractor
  ( ExtractState
  , ExtractorDef
  , HasExtractors (..)
  , HasLedgerData (..)
  , HasNetwork (..)
  , emptyBlockLedgerData
  )
import DbSync.Phase.Ingest.DedupMap (DedupMaps)
import DbSync.Phase.Ingest.PipelineStats (PipelineStats)
import DbSync.Phase.Ingest.ReceiverStats (ReceiverStats)
import DbSync.Phase.Ingest.UtxoCache (UtxoCache)
import DbSync.Ledger.Types (HasLedgerEnv (..), LedgerEnv (..))
import DbSync.Metrics (HasMetrics (..), Metrics)
import DbSync.Phase.Current (HasCurrentPhase (..), CurrentPhase, readCurrentPhase)
import DbSync.Resolver (HasResolver (..), IdResolver)
import DbSync.Worker.TxOut.AddressBuffer (AddressBufferRef)
import DbSync.Worker.TxOut.ConsumedByBuffer (ConsumedByBufferRef)
import DbSync.Worker.TxOut (TxOutWorker)
import DbSync.StateQuery.Types
  ( HasStateQueryVar (..)
  , HasSystemStart (..)
  , StateQueryVar
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer, Severity)
import DbSync.Trace.Watchdog (HasWatchdog (..), Watchdog)
import DbSync.Writer (HasWriter (..), Writer)

-- NOTE: DedupMaps is internally mutable (BasicHashTable + IORef counters).
-- No IORef wrapper needed — the hash tables are mutated in-place.

-- ---------------------------------------------------------------------------
-- * Accessor classes
-- ---------------------------------------------------------------------------

-- | Access the sync configuration from any environment.
class HasConfig env where
  getConfig :: env -> SyncConfig

-- | The protocol security parameter @k@ — the maximum rollback depth.
-- A run-time constant sourced from the topology config at boot.
-- Mainnet is 2160; testnets vary.
class HasSecurityParam env where
  getSecurityParam :: env -> Word64

-- | The state the chainsync receiver needs from whichever phase
-- owns it. Both 'IngestEnv' and 'FollowEnv' provide it, so
-- 'DbSync.Node.Connection.connectToNode' can run against either.
--
-- These fields are always used together — the receiver writes into
-- the queues, bumps the watchdog, and publishes the rollback
-- boundary on every observed tip. Bundling rather than 7 single-field
-- classes keeps call-site noise down without hiding any field.
class HasReceiverChannels env where
  getBlockQueue       :: env -> TBQueue ChainSyncMsg
  getLedgerQueue      :: env -> Maybe (TBQueue ChainSyncMsg)
  getStateQueryVar    :: env -> StateQueryVar
  getReceiverStats    :: env -> ReceiverStats
  getWatchdog         :: env -> Watchdog
  getLatestPoint      :: env -> IORef (Maybe CardanoPoint)
  getRollbackBoundary :: env -> TVar (Maybe BlockNo)

-- ---------------------------------------------------------------------------
-- * Environment types
-- ---------------------------------------------------------------------------

-- | Shared core environment available in every phase.
--
-- Constructed once at startup; the phase ref is mutated by the
-- orchestrator and the Follow loop as the lifecycle progresses.
data CoreEnv = CoreEnv
  { ceTracer      :: !AppTracer
    -- ^ Structured logging tracer (contra-tracer)
  , ceMinSeverity :: !Severity
    -- ^ Same value the tracer was built with. Subsystems that gate
    -- allocation on log level (watchdog, per-epoch diagnostic) read
    -- it rather than re-parsing the profile.
  , ceMetrics     :: !Metrics
    -- ^ Prometheus metrics handles
  , ceConfig      :: !SyncConfig
    -- ^ Parsed db-sync profile
  , ceNodeConfig  :: !NodeConfig
    -- ^ Extracted cardano-node configuration
  , ceExtractors  :: ![ExtractorDef]
    -- ^ Active extractor definitions
  , ceNetwork     :: !Network
    -- ^ Chain network ID from the Shelley genesis. Drives the HRP
    -- on stake / reward Bech32 encodings.
  , ceCurrentPhase :: !CurrentPhase
    -- ^ Live lifecycle phase. Written by the orchestrator and the
    -- Follow loop; read by extractors, logs, and the watchdog.
  , ceSecurityParam :: !Word64
    -- ^ Protocol @k@ (max rollback depth). Read by the rollback path
    -- to gate deletes past the k-safety horizon.
  }

-- | Environment for the 'IngestChainHistory' phase.
--
-- Extends 'CoreEnv' with the runtime state needed for loader-stream
-- ingestion: the chainsync message queue, loader-stream connections,
-- ID counters, mutable dedup hash tables, the resolver and writer
-- adapters, the state-query interpreter handle, the system start,
-- and the ledger subsystem 'HasLedgerEnv' which is either
-- @LedgerEnabled !LedgerEnv@ (carrying its own block queue +
-- epoch-coordination 'TMVar's + snapshot queue) or
-- @LedgerDisabled !NoLedgerEnv@ (allocating nothing ledger-stateful).
--
-- The block queue carries raw 'CardanoBlock' values inside
-- 'MsgForward'; parsing into 'DbSync.Block.Types.GenericBlock'
-- happens inside the consumer so the receiver thread doesn't pay
-- parsing cost on the hot path.
data IngestEnv = IngestEnv
  { ieCore          :: !CoreEnv
    -- ^ Shared core environment
  , ieBlockQueue    :: !(TBQueue ChainSyncMsg)
    -- ^ Forward blocks and rollback markers received from the node,
    -- awaiting parse + extraction. Rollback markers are unreachable
    -- on this queue during 'IngestChainHistory' (the consumer exits
    -- at the rollback boundary, so no volatile block ever arrives);
    -- the consumer panics if one slips through.
  , ieLoaderStream    :: !LoaderStream
    -- ^ Multi-threaded loader-stream writer (per-table TBQueues + worker threads)
  , ieDedupMaps     :: !DedupMaps
    -- ^ Mutable deduplication maps (internally mutable hash tables)
  , ieAddressBuffer :: !AddressBufferRef
    -- ^ Per-epoch buffer of address-resolution work for the
    -- 'ieTxOutWorker'. The consumer hands the contents to the worker
    -- at each epoch boundary and resets the ref to empty.
  , ieTxOutWorker :: !TxOutWorker
    -- ^ Background worker that drains 'ieAddressBuffer' and
    -- 'ieConsumedByBuffer' on a single PG connection. Writes
    -- @address@ rows, @tx_out.address_id@, @collateral_tx_out.address_id@,
    -- and (when the flag is on) @tx_out.consumed_by_tx_id@ for the
    -- epoch one boundary behind the main pipeline.
  , ieUtxoCache :: !UtxoCache
    -- ^ Bounded FIFO map from tx hash to @(tx_id, [(tx_out_id, value)])@.
    -- Consulted by the UTxO extractor to resolve inputs at COPY time;
    -- misses fall through to the post-load resolve.
  , ieConsumedByBuffer :: !(Maybe ConsumedByBufferRef)
    -- ^ Per-epoch buffer of @(producer_tx_out_id, consumer_tx_id)@
    -- pairs destined for @tx_out.consumed_by_tx_id@. 'Nothing' when
    -- @utxo.consumed_by_tx_id@ is off; the 'ieTxOutWorker' then
    -- skips that sub-task.
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
    -- ^ Ingest-phase writer (the loader-stream adapter). Built once
    -- from 'ieLoaderStream' at startup.
  , ieExtractState  :: !(IORef ExtractState)
    -- ^ Per-block extraction state — carries the 'IdCounters' through
    -- 'atomicModifyIORef'' so the resolver can hand out fresh IDs.
  , ieReceiverStats :: !ReceiverStats
    -- ^ Cumulative receiver-thread counters (blocks received, writes
    -- blocked). Mutated by the chainsync receiver; sampled by the
    -- watchdog at each interval for Debug diagnostics.
  , iePipelineStats :: !(IORef PipelineStats)
    -- ^ Per-epoch drain-size counters. Consumer increments on every
    -- queue drain and resets at each epoch boundary; the watchdog
    -- samples for interval deltas.
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
  , ieRollbackBoundary        :: !(TVar (Maybe BlockNo))
    -- ^ Latest @nodeTip − k@ observed by the receiver, where @k@ is
    -- the protocol security parameter. Below this block number the
    -- chain is finalised and immune to rollback. 'Nothing' until the
    -- first 'MsgRollForward' arrives, and while the chain is still
    -- shorter than @k@ blocks (everything is volatile in that case).
    -- The consumer compares each processed block against this and
    -- exits 'IngestChainHistory' cleanly once it crosses, so the
    -- caller can run 'PreparingForVolatileTail' before handing off to
    -- 'FollowingChainTip'.
  }

-- | Environment for the Follow loop ('FollowingVolatileTail' and
-- 'FollowingChainTip').
--
-- Lighter than 'IngestEnv' — no COPY connections, no dedup maps, no
-- background address resolver. Reads from the same chainsync message
-- queue as the Ingest consumer did and runs per-block INSERTs with
-- rollback support against a single hasql connection.
data FollowEnv = FollowEnv
  { feCore                :: !CoreEnv
    -- ^ Shared core environment
  , feBlockQueue          :: !(TBQueue ChainSyncMsg)
    -- ^ Forward blocks and rollback markers from the chainsync
    -- receiver. The Follow loop processes one message per
    -- per-block PG transaction.
  , feHasLedgerEnv        :: !HasLedgerEnv
    -- ^ Carried over so the LSM-backed worker keeps producing
    -- 'ApplyResult's while Follow runs.
  , feStateQueryVar       :: !StateQueryVar
    -- ^ Slot-to-time interpreter, used by 'parseBlock'.
  , feSystemStart         :: !SystemStart
  , feReceiverStats       :: !ReceiverStats
  , feWatchdog            :: !Watchdog
  , feLatestReceivedPoint :: !(IORef (Maybe CardanoPoint))
  , feHasqlConnection     :: !Conn.Connection
    -- ^ Dedicated Follow connection — drives the resolver and writer
    -- (INSERTs) and the per-block @BEGIN@/@COMMIT@ envelope. Distinct
    -- from 'feControlConnection' so the rollback cascade and the
    -- 'sync_state' advance don't fight for the same handle.
  , feResolver            :: !(IdResolver IO)
    -- ^ Sequence-driven resolver: @nextval@ for non-dedup, SELECT
    -- then @nextval@ for dedup tables.
  , feWriter              :: !(Writer IO)
    -- ^ INSERT writer over 'feHasqlConnection'.
  , feControlConnection   :: !ControlConnection
    -- ^ PG connection used by 'sync_state' advances at each
    -- per-block commit.
  , feRollbackBoundary    :: !(TVar (Maybe BlockNo))
    -- ^ Tip-derived rollback boundary kept up to date by the
    -- receiver. Unused by the Follow consumer (every block in
    -- Follow is volatile by definition); held only so the receiver
    -- has somewhere to publish it.
  }

-- ---------------------------------------------------------------------------
-- * Follow construction
-- ---------------------------------------------------------------------------

-- | Build a 'FollowEnv' by reusing receiver-side state from the
-- 'IngestEnv'. The block queue, watchdog, state-query interpreter,
-- and latest-point ref are shared so the receiver keeps producing
-- into the same FIFO across the phase boundary.
--
-- The caller supplies the new Follow-only resources: a fresh hasql
-- connection (drives resolver + writer + per-block transactions) and
-- the resolver/writer pair built over it.
mkFollowEnvFromIngest
  :: IngestEnv
  -> Conn.Connection
  -> IdResolver IO
  -> Writer IO
  -> FollowEnv
mkFollowEnvFromIngest ie conn resolver writer =
  FollowEnv
    { feCore                = ieCore ie
    , feBlockQueue          = ieBlockQueue ie
    , feHasLedgerEnv        = ieHasLedgerEnv ie
    , feStateQueryVar       = ieStateQueryVar ie
    , feSystemStart         = ieSystemStart ie
    , feReceiverStats       = ieReceiverStats ie
    , feWatchdog            = ieWatchdog ie
    , feLatestReceivedPoint = ieLatestReceivedPoint ie
    , feHasqlConnection     = conn
    , feResolver            = resolver
    , feWriter              = writer
    , feControlConnection   = ieControlConnection ie
    , feRollbackBoundary    = ieRollbackBoundary ie
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

instance HasSecurityParam CoreEnv where
  getSecurityParam = ceSecurityParam

instance HasSecurityParam IngestEnv where
  getSecurityParam = getSecurityParam . ieCore

instance HasSecurityParam FollowEnv where
  getSecurityParam = getSecurityParam . feCore

-- ---------------------------------------------------------------------------
-- * HasReceiverChannels instances
-- ---------------------------------------------------------------------------

instance HasReceiverChannels IngestEnv where
  getBlockQueue       = ieBlockQueue
  getLedgerQueue ie   = case ieHasLedgerEnv ie of
    LedgerEnabled lenv -> Just (leLedgerQueue lenv)
    LedgerDisabled _   -> Nothing
  getStateQueryVar    = ieStateQueryVar
  getReceiverStats    = ieReceiverStats
  getWatchdog         = ieWatchdog
  getLatestPoint      = ieLatestReceivedPoint
  getRollbackBoundary = ieRollbackBoundary

instance HasReceiverChannels FollowEnv where
  getBlockQueue       = feBlockQueue
  getLedgerQueue fe   = case feHasLedgerEnv fe of
    LedgerEnabled lenv -> Just (leLedgerQueue lenv)
    LedgerDisabled _   -> Nothing
  getStateQueryVar    = feStateQueryVar
  getReceiverStats    = feReceiverStats
  getWatchdog         = feWatchdog
  getLatestPoint      = feLatestReceivedPoint
  getRollbackBoundary = feRollbackBoundary

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

instance HasResolver FollowEnv where
  getResolver = feResolver

-- ---------------------------------------------------------------------------
-- * HasWriter instances (orphan — class defined in DbSync.Writer)
-- ---------------------------------------------------------------------------

instance HasWriter IngestEnv where
  getWriter = ieWriter

instance HasWriter FollowEnv where
  getWriter = feWriter

-- ---------------------------------------------------------------------------
-- * HasLedgerData instances
-- ---------------------------------------------------------------------------

-- | During 'IngestChainHistory' extractors never see ledger output
-- per block: the worker accumulates protocol-param deposits into
-- @epoch_param_pending@ at epoch boundaries and the post-load pass
-- backfills the affected columns once ingest exits. Keeping this
-- 'emptyBlockLedgerData' decouples the worker from the consumer's
-- hot path.
instance HasLedgerData IngestEnv where
  getLedgerData _env _block = pure emptyBlockLedgerData

-- | 'FollowEnv' currently has no ledger env; returns empty until
-- the Follow-side worker plumbing lands.
instance HasLedgerData FollowEnv where
  getLedgerData _ _ = pure emptyBlockLedgerData

-- ---------------------------------------------------------------------------
-- * HasCurrentPhase instances
-- ---------------------------------------------------------------------------

instance HasCurrentPhase CoreEnv where
  getCurrentPhase = readCurrentPhase . ceCurrentPhase

instance HasCurrentPhase IngestEnv where
  getCurrentPhase = getCurrentPhase . ieCore

instance HasCurrentPhase FollowEnv where
  getCurrentPhase = getCurrentPhase . feCore

-- ---------------------------------------------------------------------------
-- * HasControlConnection instances
-- ---------------------------------------------------------------------------

instance HasControlConnection IngestEnv where
  getControlConnection = ieControlConnection

instance HasControlConnection FollowEnv where
  getControlConnection = feControlConnection

instance HasControlConnection LedgerEnv where
  getControlConnection = leControlConnection

-- ---------------------------------------------------------------------------
-- * HasHasqlConnection instances
-- ---------------------------------------------------------------------------

instance HasHasqlConnection FollowEnv where
  getHasqlConnection = feHasqlConnection

-- ---------------------------------------------------------------------------
-- * HasLoaderStream instances
-- ---------------------------------------------------------------------------

instance HasLoaderStream IngestEnv where
  getLoaderStream = ieLoaderStream

-- ---------------------------------------------------------------------------
-- * HasWatchdog instances
-- ---------------------------------------------------------------------------

instance HasWatchdog IngestEnv where
  getWatchdog = ieWatchdog

instance HasWatchdog FollowEnv where
  getWatchdog = feWatchdog

-- ---------------------------------------------------------------------------
-- * HasStateQueryVar / HasSystemStart instances
-- ---------------------------------------------------------------------------

instance HasStateQueryVar IngestEnv where
  getStateQueryVar = ieStateQueryVar

instance HasStateQueryVar FollowEnv where
  getStateQueryVar = feStateQueryVar

instance HasSystemStart IngestEnv where
  getSystemStart = ieSystemStart

instance HasSystemStart FollowEnv where
  getSystemStart = feSystemStart

instance HasSystemStart LedgerEnv where
  getSystemStart = leSystemStart

-- ---------------------------------------------------------------------------
-- * Small env adapters
--
-- Used by boot code (no 'IngestEnv' / 'FollowEnv' built yet) and by
-- tests (no real phase env in scope). Production phase envs satisfy
-- these classes directly via the records above.
-- ---------------------------------------------------------------------------

-- | Tracer + control connection. Drives 'rebuildDedupMaps' and any
-- other 'CheckpointM env m' action that just needs a logger and the
-- @sync_state@ connection.
data TracerWithControl = TracerWithControl !AppTracer !ControlConnection

instance HasTracer TracerWithControl where
  getTracer (TracerWithControl t _) = t

instance HasControlConnection TracerWithControl where
  getControlConnection (TracerWithControl _ c) = c

-- | Tracer + raw hasql connection + 'SyncConfig'. Drives Prep
-- helpers from test code (production boots them via 'CoreWithConn',
-- which projects the same instances out of 'CoreEnv').
data TracerWithConn = TracerWithConn !AppTracer !Conn.Connection !SyncConfig

instance HasTracer TracerWithConn where
  getTracer (TracerWithConn t _ _) = t

instance HasHasqlConnection TracerWithConn where
  getHasqlConnection (TracerWithConn _ c _) = c

instance HasConfig TracerWithConn where
  getConfig (TracerWithConn _ _ cfg) = cfg

-- | Loader stream + control connection. Drives 'commitEpoch' from
-- any caller that has both handles but isn't running inside 'IngestEnv'.
data LoaderWithControl = LoaderWithControl !LoaderStream !ControlConnection

instance HasLoaderStream LoaderWithControl where
  getLoaderStream (LoaderWithControl ls _) = ls

instance HasControlConnection LoaderWithControl where
  getControlConnection (LoaderWithControl _ c) = c

-- | Full 'CoreEnv' plus a hasql connection. Used by boot-time and
-- CLI flows that need both the run-wide config (security param,
-- network, tracer) and a fresh PG connection.
data CoreWithConn = CoreWithConn !CoreEnv !Conn.Connection

instance HasTracer CoreWithConn where
  getTracer (CoreWithConn c _) = getTracer c

instance HasHasqlConnection CoreWithConn where
  getHasqlConnection (CoreWithConn _ conn) = conn

instance HasSecurityParam CoreWithConn where
  getSecurityParam (CoreWithConn c _) = getSecurityParam c

instance HasNetwork CoreWithConn where
  getNetwork (CoreWithConn c _) = getNetwork c

instance HasConfig CoreWithConn where
  getConfig (CoreWithConn c _) = getConfig c
