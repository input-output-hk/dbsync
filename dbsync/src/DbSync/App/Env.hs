{-# OPTIONS_GHC -Wno-orphans #-}

-- | Phase environments: 'CoreEnv' (shared), 'IngestEnv' (bulk COPY
-- phase), 'FollowEnv' (live chain-follow phase).
--
-- The accessor-class instances are orphans here to break the import
-- cycle between the class modules and these records.
module DbSync.App.Env
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

import DbSync.Parser.Types (CardanoPoint)
import DbSync.ChainSync.Msg (ChainSyncMsg)
import DbSync.SyncState.Row (ControlConnection, HasControlConnection (..))
import DbSync.App.Config.Types (NodeConfig, SyncConfig)
import DbSync.Db.Loader (LoaderStream, HasLoaderStream (..))
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Extractor
  ( ExtractState
  , ExtractorDef
  , HasExtractors (..)
  , HasLedgerData (..)
  , HasNetwork (..)
  , takeBlockLedgerData
  )
import DbSync.Phase.Ingest.DedupStore (DedupStores)
import DbSync.Phase.Ingest.LsmSession (LsmSession)
import DbSync.Phase.Ingest.UtxoStore (UtxoStore)
import DbSync.Worker.Ledger.Types (HasLedgerEnv (..), LedgerEnv (..))
import DbSync.Metrics (HasMetrics (..), Metrics)
import DbSync.Phase.Current (HasCurrentPhase (..), CurrentPhase, readCurrentPhase)
import DbSync.Resolver (HasResolver (..), IdResolver)
import DbSync.Worker.OffChain.Pool (OffChainPoolWorker)
import DbSync.Worker.OffChain.Vote (OffChainVoteWorker)
import DbSync.Worker.TxOut.AddressBuffer (AddressBufferRef)
import DbSync.Worker.TxOut.ConsumedByBuffer (ConsumedByBufferRef)
import DbSync.Worker.TxOut.Worker (TxOutWorker)
import DbSync.StateQuery.Types
  ( HasStateQueryVar (..)
  , HasSystemStart (..)
  , StateQueryVar
  )
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Types (AppTracer, Severity)
import DbSync.Writer (HasWriter (..), Writer)

-- ---------------------------------------------------------------------------
-- * Accessor classes
-- ---------------------------------------------------------------------------

class HasConfig env where
  getConfig :: env -> SyncConfig

-- | The protocol security parameter @k@ — the maximum rollback depth.
class HasSecurityParam env where
  getSecurityParam :: env -> Word64

-- | The state the chainsync receiver needs. Both 'IngestEnv' and
-- 'FollowEnv' provide it, so the same receiver runs against either.
class HasReceiverChannels env where
  getBlockQueue       :: env -> TBQueue ChainSyncMsg
  getLedgerQueue      :: env -> Maybe (TBQueue ChainSyncMsg)
  getStateQueryVar    :: env -> StateQueryVar
  getLatestPoint      :: env -> TVar (Maybe CardanoPoint)
  getRollbackBoundary :: env -> TVar (Maybe BlockNo)
  getLatestTipBlock   :: env -> TVar (Maybe BlockNo)

-- ---------------------------------------------------------------------------
-- * Environment types
-- ---------------------------------------------------------------------------

-- | Shared core environment available in every phase. Built once at
-- startup; only 'ceCurrentPhase' changes after that.
data CoreEnv = CoreEnv
  { ceTracer      :: !AppTracer
  , ceMinSeverity :: !Severity
    -- ^ The severity the tracer was built with. Subsystems that gate
    -- allocation on log level read it instead of the config.
  , ceMetrics     :: !Metrics
  , ceConfig      :: !SyncConfig
  , ceNodeConfig  :: !NodeConfig
    -- ^ Unused. No production caller reads this field.
  , ceExtractors  :: ![ExtractorDef]
  , ceNetwork     :: !Network
    -- ^ Network id from the Shelley genesis. Drives the HRP on the
    -- stake and reward Bech32 encodings.
  , ceCurrentPhase :: !CurrentPhase
    -- ^ The orchestrator and the Follow loop write it. Extractors and
    -- logs read it.
  , ceSecurityParam :: !Word64
    -- ^ Protocol @k@. The rollback path uses it to gate deletes past
    -- the k-safety horizon.
  }

-- | Environment for the 'IngestChainHistory' phase. Extends 'CoreEnv'
-- with the state the loader-stream pipeline needs.
data IngestEnv = IngestEnv
  { ieCore          :: !CoreEnv
  , ieBlockQueue    :: !(TBQueue ChainSyncMsg)
    -- ^ Blocks and rollback markers from the receiver. The consumer
    -- exits at the rollback boundary, so it never reaches a rollback
    -- marker; one that arrives anyway panics.
  , ieLoaderStream    :: !LoaderStream
  , ieDedupStores   :: !DedupStores
    -- ^ Ten LSM tables on 'ieLsmSession'. Each maps a natural key to
    -- its assigned database id.
  , ieAddressBuffer :: !AddressBufferRef
    -- ^ Per-epoch buffer of address work for 'ieTxOutWorker'. The
    -- consumer hands the contents to the worker at each epoch
    -- boundary and resets the ref to empty.
  , ieTxOutWorker :: !TxOutWorker
    -- ^ Drains 'ieAddressBuffer' and 'ieConsumedByBuffer' on its own
    -- PG connection, one epoch boundary behind the main pipeline.
  , ieOffChainPoolWorker :: !(Maybe OffChainPoolWorker)
    -- ^ Off-chain pool metadata fetcher. 'Nothing' when
    -- @off_chain_pools@ is disabled.
  , ieOffChainVoteWorker :: !(Maybe OffChainVoteWorker)
    -- ^ Off-chain governance anchor fetcher. 'Nothing' when
    -- @off_chain_votes@ is disabled.
  , ieLsmSession :: !LsmSession
    -- ^ Backs the ingest scratch tables. Closed on a mid-flight
    -- crash; deleted after Prep completes.
  , ieUtxoStore :: !UtxoStore
    -- ^ Tx-hash → @(tx_id, [(tx_out_id, value)])@ store on
    -- 'ieLsmSession'. The UTxO extractor resolves inputs against it
    -- at COPY time; a miss falls through to the post-load resolve.
  , ieConsumedByBuffer :: !(Maybe ConsumedByBufferRef)
    -- ^ Per-epoch @(producer_tx_out_id, consumer_tx_id)@ pairs for
    -- @tx_out.consumed_by_tx_id@. 'Nothing' when
    -- @utxo.consumed_by_tx_id@ is off; 'ieTxOutWorker' then skips
    -- that sub-task.
  , ieHasLedgerEnv  :: !HasLedgerEnv
  , ieStateQueryVar :: !StateQueryVar
    -- ^ LocalStateQuery 'Interpreter' handle. The consumer thread
    -- uses it to compute 'SlotDetails'.
  , ieSystemStart   :: !SystemStart
    -- ^ From the Shelley genesis. The state-query interpreter needs
    -- it to compute slot times.
  , ieResolver      :: !(IdResolver IO)
  , ieWriter        :: !(Writer IO)
  , ieExtractState  :: !(IORef ExtractState)
    -- ^ Carries the 'IdCounters' through 'atomicModifyIORef'' so the
    -- resolver can hand out fresh ids.
  , ieControlConnection :: !ControlConnection
    -- ^ The consumer advances @dbsync_sync_state@ over it at each
    -- epoch boundary.
  , ieLastCommittedSlotAtBoot :: !(Maybe SlotNo)
    -- ^ Upper edge of the replay window on a ledger-enabled resume.
    -- The consumer skips 'processBlock' at or below this slot,
    -- because PG already holds those blocks. 'Nothing' otherwise.
  , ieReplayStartSlot         :: !(Maybe SlotNo)
    -- ^ Lower edge of the replay window: the chosen snapshot's slot.
    -- Drives the percentage in the replay progress log. 'Nothing'
    -- otherwise.
  , ieLatestReceivedPoint     :: !(TVar (Maybe CardanoPoint))
    -- ^ Latest chain point the receiver accepted. Every reconnection
    -- reads it so chainsync resumes at the current position, not the
    -- boot-time intersect. Without it, a mid-run @cardano-node@
    -- restart intersects at the boot-time point (Origin on a fresh
    -- sync) and rolls the chain pointer back to genesis; the ledger
    -- worker then crashes with a hash mismatch when the genesis block
    -- arrives over the advanced state. 'Nothing' until the first
    -- block arrives. A 'TVar' so the receiver updates it in the same
    -- STM transaction as the queue write — a point must never be
    -- recorded without its block, or the reverse.
  , ieRollbackBoundary        :: !(TVar (Maybe BlockNo))
    -- ^ Latest @nodeTip − k@ the receiver observed. Below this block
    -- number the chain is final and immune to rollback. 'Nothing'
    -- until the first 'MsgRollForward', and while the chain is
    -- shorter than @k@ blocks. The consumer leaves
    -- 'IngestChainHistory' once a processed block crosses it.
  , ieLatestTipBlock          :: !(TVar (Maybe BlockNo))
    -- ^ Server tip block number from every chainsync roll message.
    -- The Follow flip predicate reads it directly, so the flip does
    -- not depend on @k@ agreeing across two sources.
  }

-- ---------------------------------------------------------------------------
-- * Follow environment
-- ---------------------------------------------------------------------------

-- | Environment for 'FollowingVolatileTail' and 'FollowingChainTip'.
--
-- Lighter than 'IngestEnv': no COPY connections, no dedup stores, no
-- background address resolver. It reads the same chainsync queue the
-- Ingest consumer used and runs per-block INSERTs against a single
-- hasql connection.
data FollowEnv = FollowEnv
  { feCore                :: !CoreEnv
  , feBlockQueue          :: !(TBQueue ChainSyncMsg)
    -- ^ Blocks and rollback markers from the receiver. The Follow
    -- loop processes one message per PG transaction.
  , feHasLedgerEnv        :: !HasLedgerEnv
    -- ^ Carried over so the ledger worker keeps producing
    -- 'ApplyResult's while Follow runs.
  , feStateQueryVar       :: !StateQueryVar
  , feSystemStart         :: !SystemStart
  , feLatestReceivedPoint :: !(TVar (Maybe CardanoPoint))
    -- ^ Latest chainsync point accepted. Survives the phase flip.
  , feHasqlConnection     :: !Conn.Connection
    -- ^ Drives the resolver, the writer, and the per-block
    -- @BEGIN@/@COMMIT@ envelope. Distinct from
    -- 'feControlConnection' so the rollback cascade and the
    -- @sync_state@ advance do not contend for one handle.
  , feResolver            :: !(IdResolver IO)
    -- ^ Sequence-driven: @nextval@ for non-dedup tables, SELECT then
    -- @nextval@ for dedup tables.
  , feWriter              :: !(Writer IO)
  , feControlConnection   :: !ControlConnection
    -- ^ Carries the @sync_state@ advance at each per-block commit.
  , feRollbackBoundary    :: !(TVar (Maybe BlockNo))
    -- ^ The receiver keeps it current. The Follow consumer ignores
    -- it, because every block in Follow is volatile; the field only
    -- gives the receiver somewhere to publish.
  , feLatestTipBlock      :: !(TVar (Maybe BlockNo))
    -- ^ Server tip block number from every chainsync roll message.
    -- Drives the 'FollowingVolatileTail' -> 'FollowingChainTip' flip.
  , feReplayBootSlot      :: !(Maybe SlotNo)
    -- ^ Upper edge of the Follow replay window:
    -- @dbsync_sync_state.last_committed_slot@ when a ledger-enabled
    -- restart finds the on-disk snapshot below it. PG already holds
    -- those blocks, so the consumer skips them and lets the receiver
    -- fan them out to the ledger worker. 'Nothing' on the in-process
    -- Ingest → Prep → Follow handoff, and when the snapshot already
    -- aligns with PG.
  , feReplayStartSlot     :: !(Maybe SlotNo)
    -- ^ Lower edge of the Follow replay window: the chosen snapshot's
    -- slot. 'Just' exactly when 'feReplayBootSlot' is.
  , feOffChainPoolWorker  :: !(Maybe OffChainPoolWorker)
    -- ^ Carried over from 'IngestEnv' so the worker keeps polling
    -- across the phase boundary. 'Nothing' when disabled.
  , feOffChainVoteWorker  :: !(Maybe OffChainVoteWorker)
    -- ^ Carried over from 'IngestEnv' so the worker keeps polling
    -- across the phase boundary. 'Nothing' when disabled.
  }

-- ---------------------------------------------------------------------------
-- * Follow construction
-- ---------------------------------------------------------------------------

-- | Build a 'FollowEnv' that reuses the receiver-side state of an
-- 'IngestEnv'. The block queue, state-query interpreter, and
-- latest-point ref stay shared, so the receiver keeps producing into
-- the same FIFO across the phase boundary. The caller supplies the
-- Follow-only hasql connection and the resolver/writer built over it.
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
    , feLatestReceivedPoint = ieLatestReceivedPoint ie
    , feHasqlConnection     = conn
    , feResolver            = resolver
    , feWriter              = writer
    , feControlConnection   = ieControlConnection ie
    , feRollbackBoundary    = ieRollbackBoundary ie
    , feLatestTipBlock      = ieLatestTipBlock ie
    , feReplayBootSlot      = Nothing
    , feReplayStartSlot     = Nothing
    , feOffChainPoolWorker  = ieOffChainPoolWorker ie
    , feOffChainVoteWorker  = ieOffChainVoteWorker ie
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
  getLatestPoint      = ieLatestReceivedPoint
  getRollbackBoundary = ieRollbackBoundary
  getLatestTipBlock   = ieLatestTipBlock

instance HasReceiverChannels FollowEnv where
  getBlockQueue       = feBlockQueue
  getLedgerQueue fe   = case feHasLedgerEnv fe of
    LedgerEnabled lenv -> Just (leLedgerQueue lenv)
    LedgerDisabled _   -> Nothing
  getStateQueryVar    = feStateQueryVar
  getLatestPoint      = feLatestReceivedPoint
  getRollbackBoundary = feRollbackBoundary
  getLatestTipBlock   = feLatestTipBlock

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

instance HasLedgerData IngestEnv where
  getLedgerData ie _block = takeBlockLedgerData (ieHasLedgerEnv ie)

instance HasLedgerData FollowEnv where
  getLedgerData fe _block = takeBlockLedgerData (feHasLedgerEnv fe)

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
-- ---------------------------------------------------------------------------

-- | Drives 'rebuildDedupMaps' and any other action that needs only a
-- logger and the @sync_state@ connection.
data TracerWithControl = TracerWithControl !AppTracer !ControlConnection

instance HasTracer TracerWithControl where
  getTracer (TracerWithControl t _) = t

instance HasControlConnection TracerWithControl where
  getControlConnection (TracerWithControl _ c) = c

-- | Drives Prep helpers from test code. Production boots them via
-- 'CoreWithConn', which projects the same instances out of 'CoreEnv'.
data TracerWithConn = TracerWithConn !AppTracer !Conn.Connection !SyncConfig

instance HasTracer TracerWithConn where
  getTracer (TracerWithConn t _ _) = t

instance HasHasqlConnection TracerWithConn where
  getHasqlConnection (TracerWithConn _ c _) = c

instance HasConfig TracerWithConn where
  getConfig (TracerWithConn _ _ cfg) = cfg

-- | Drives 'commitEpoch' from a caller that holds both handles but
-- does not run inside 'IngestEnv'.
data LoaderWithControl = LoaderWithControl !LoaderStream !ControlConnection

instance HasLoaderStream LoaderWithControl where
  getLoaderStream (LoaderWithControl ls _) = ls

instance HasControlConnection LoaderWithControl where
  getControlConnection (LoaderWithControl _ c) = c

-- | For boot-time and CLI flows that need both the run-wide config
-- and a fresh PG connection.
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
