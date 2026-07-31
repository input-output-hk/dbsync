{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Core types for the ledger subsystem.
--
-- 'HasLedgerEnv' splits ledger-enabled from ledger-disabled. The
-- disabled arm carries only tracer, protocol info, system start and
-- network — no LSM session, snapshot queue, or 'LedgerDB' buffer — so
-- the snapshot and worker entry points don't exist on it.
module DbSync.Worker.Ledger.Types
  ( -- * Top-level sum
    HasLedgerEnv (..)
  , NoLedgerEnv (..)
  , LedgerEnv (..)
  , mkNoLedgerEnv

    -- * Configuration switches
  , PanicPolicy (..)

    -- * LedgerDB and its elements
  , LedgerDB (..)
  , DbSyncStateRef (..)
  , CardanoLedgerState (..)
  , EpochBlockNo (..)
  , ConsensusStateRef
  , toConsensusStateRef
  , fromConsensusStateRef
  , initCardanoLedgerState
  , deriveEpochBlockNo

    -- * Snapshot bookkeeping
  , SnapshotPoint (..)

    -- * Block application plumbing
  , ApplyResult (..)
  , defaultApplyResult
  , BlockApplyData (..)
  , BoundaryApplyData (..)
  , ProposedCommitteeMember (..)
  , RegisteredPoolsCache (..)
  , DepositsMap (..)
  , lookupDepositsMap
  , emptyDepositsMap
  , getGovExpiresAt

    -- * Committee helper
  , updatedCommittee

    -- * Per-era NewEpochState access
  , HasNewEpochState (..)
  , newEpochStateT
  ) where

import Cardano.Prelude hiding (atomically)

import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Alonzo.Scripts (Prices)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (StrictTMVar, StrictTVar, newTVarIO)
import Control.Concurrent.STM.TBQueue (TBQueue)
import Data.IORef (IORef)
import qualified Data.Map.Strict as Map

import DbSync.Worker.Ledger.DepositAccumulator (EpochParamsRef)
import DbSync.ChainSync.Msg (ChainSyncMsg)
import Data.SOP.Functors (Flip (..))
import Data.SOP.Strict (NP (..), fn, hap, type (-.->))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Lens.Micro (Traversal', (^.))
import System.Mem.StableName (StableName)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import Ouroboros.Consensus.Cardano.Block
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , CardanoShelleyEras
  , ConwayEra
  , LedgerState (..)
  , MaryEra
  , ShelleyEra
  , StandardCrypto
  )
import Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Ledger.Basics (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Shelley.Ledger (LedgerState (..), ShelleyBlock)
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot, SnapshotManager)
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq (LedgerTablesHandle)
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq as Consensus (StateRef (..))

import Prelude (id)

import DbSync.App.Config.Types (LedgerBackend)
import qualified DbSync.Worker.Ledger.EpochUpdate as Generic
import qualified DbSync.Worker.Ledger.ProtoParams as Generic
import qualified DbSync.Worker.Ledger.StakeDist as Generic
import DbSync.Worker.Ledger.Event (LedgerEvent, RewardsCapture)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Shelley.HFEras ()                -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()  -- 'LedgerSupportsProtocol' orphans

import DbSync.Parser.Types (CardanoPoint)
import DbSync.SyncState.Row (ControlConnection)
import DbSync.Phase.Current (CurrentPhase)
import DbSync.StateQuery.Types (CardanoInterpreter, SlotDetails)
import DbSync.Trace.Types (AppTracer)

-- ---------------------------------------------------------------------------
-- * Top-level sum
-- ---------------------------------------------------------------------------

-- | Is the ledger feature enabled, or is it disabled?
--
-- Pattern-match at every site where behaviour differs (starting the
-- 'LedgerWorker', taking a snapshot, reading a 'LedgerDB'
-- checkpoint, …). The 'LedgerDisabled' arm carries enough to keep
-- the rest of the system running (we still need a 'ProtocolInfo' to
-- deserialise blocks off the wire) but nothing ledger-stateful.
data HasLedgerEnv
  = LedgerEnabled  !LedgerEnv
  | LedgerDisabled !NoLedgerEnv

-- | Environment when the ledger feature is /disabled/.
--
-- Deliberately lean: no LSM session, no snapshot queue, no
-- 'LedgerDB'. This is what 'mkNoLedgerEnv' returns, and it's what
-- lives in 'DbSync.App.Env.IngestEnv' when the user has set
-- @ledger.enabled = false@ in the config.
data NoLedgerEnv = NoLedgerEnv
  { nleTracer       :: !AppTracer
  , nleProtocolInfo :: !(Consensus.ProtocolInfo (CardanoBlock StandardCrypto))
  , nleSystemStart  :: !SystemStart
  , nleNetwork      :: !Ledger.Network
  }

-- | Environment when the ledger feature is /enabled/.
--
-- Contains everything the ledger subsystem needs:
--
-- * Protocol info \/ system start \/ network — shared with
--   'NoLedgerEnv'.
-- * The 'LedgerDB' checkpoint buffer (through 'leStateVar') and the
--   cached 'CardanoInterpreter' (@leInterpreter@).
-- * Three coordination primitives for inter-thread communication:
--   'leLedgerQueue' (receiver → worker),
--   'leEpochReady' (worker → main),
--   'leEpochWait' (main → worker).
-- * The async snapshot pipeline: 'leSnapshotQueue' (worker →
--   snapshot-writer) and 'leSnapshotManager' (consensus-side
--   save \/ load \/ list \/ cleanup).
-- * Two factory callbacks — 'leInitGenesis' \/ 'leLoadSnapshot' —
--   used at boot to produce the first 'DbSyncStateRef', either from
--   genesis or from a disk snapshot.
data LedgerEnv = LedgerEnv
  { leTracer               :: !AppTracer
  , leRewardsCapture       :: !RewardsCapture
    -- ^ When 'DropRewards', reward-related 'LedgerEvent' values are
    -- dropped at the consensus-event conversion boundary.
  , leProtocolInfo         :: !(Consensus.ProtocolInfo (CardanoBlock StandardCrypto))
  , leDir                  :: !FilePath
    -- ^ Root state directory (LSM session + snapshot headers
    -- both live under this path).
  , leNetwork              :: !Ledger.Network
  , leMaxSupply            :: !Word64
  , leSystemStart          :: !SystemStart
  , lePanicPolicy          :: !PanicPolicy
    -- ^ What the worker does when it observes an invalid ledger
    -- state. 'AbortOnPanic' tears the process down; 'LogAndContinue'
    -- records the error and keeps applying.
  , leSnapshotNearTipEpoch :: !Word64
    -- ^ Epoch threshold past which we always snapshot every epoch,
    -- regardless of sync-state cadence. Default 580.
  , leLedgerBackend        :: !LedgerBackend
  , leInterpreter          :: !(StrictTVar IO (Strict.Maybe CardanoInterpreter))
  , leStateVar             :: !(StrictTVar IO (Strict.Maybe LedgerDB))
    -- * Inter-thread coordination queues and TMVars
  , leLedgerQueue          :: !(TBQueue ChainSyncMsg)
    -- ^ @BlockReceiver → LedgerWorker@ — forward blocks to apply
    -- against the LSM-backed ledger and rollback markers to apply
    -- against the in-RAM 'LedgerDB' checkpoint buffer. 'MsgForward'
    -- carries raw 'CardanoBlock' rather than parsed 'GenericBlock'
    -- because 'applyBlock' calls 'tickThenReapplyLedgerResult'
    -- which needs the consensus block shape.
  , leEpochReady           :: !(StrictTMVar IO EpochNo)
    -- ^ @LedgerWorker → Main@ — \"epoch N's ledger data is ready\".
  , leEpochWait            :: !(StrictTMVar IO EpochNo)
    -- ^ @Main → LedgerWorker@ — only used at the Ingest→Follow
    -- transition: \"please reach epoch N so we can swap modes\".
    -- * Consensus snapshot machinery (async writer + manager).
  , leSnapshotQueue        :: !(TBQueue DbSyncStateRef)
    -- ^ @LedgerWorker → LedgerSnapshotWriter@ — deferred snapshot
    -- writes.
  , leSnapshotManager      :: !(SnapshotManager IO IO (CardanoBlock StandardCrypto) ConsensusStateRef)
  , leInitGenesis          :: !(IO ConsensusStateRef)
    -- ^ Build the initial consensus 'StateRef' from genesis (used on
    -- a cold start with no on-disk snapshots).
  , leLoadSnapshot         :: !(DiskSnapshot -> IO (Either Text ConsensusStateRef))
    -- ^ Load a snapshot from disk via the configured backend (used
    -- when resuming from an existing snapshot).
  , leClose                :: !(IO ())
    -- ^ Release the LSM session\/file lock at shutdown. The
    -- consensus 'mkResources' helper allocates the session via
    -- 'allocateTemp' (impossible-to-not-transfer), so the temp
    -- registry does NOT close it on scope exit — we have to call
    -- this explicitly.
  , leLatestApplyResult    :: !(StrictTVar IO (Strict.Maybe ApplyResult))
    -- ^ Latest 'ApplyResult' produced by the worker. Overwritten on
    -- every applied block; used as a slot-reached barrier by
    -- 'waitForApplyResultAt' and by Follow's per-block reads.
  , leBoundaryApplyResults :: !(TBQueue BoundaryApplyData)
    -- ^ FIFO of boundary projections, one per epoch boundary. Each
    -- entry is forced to normal form before enqueue, so a queued
    -- boundary never pins the 'NewEpochState' generation it was
    -- derived from. Drained one per boundary by the consumer; the
    -- worker enqueues independently of 'apply' rate.
  , leBlockApplyResults    :: !(TBQueue BlockApplyData)
    -- ^ FIFO of per-block consumer projections. The worker enqueues
    -- one fully-forced entry per applied block; the consumer drains
    -- one per processed block (replay-window blocks drain and
    -- discard). Bounded so the worker creates back-pressure when the
    -- consumer falls behind.
  , leRegisteredPoolsCache :: !(IORef (Maybe RegisteredPoolsCache))
    -- ^ Worker-private pointer-identity cache backing
    -- 'DbSync.Worker.Ledger.State.getRegisteredPools'; see
    -- 'RegisteredPoolsCache'. Only the worker thread touches it.
  , leDepositAccumulator   :: !EpochParamsRef
    -- ^ Per-epoch protocol-param deposit values (stake_key /
    -- pool). The worker writes to this on every applied non-replay
    -- block via
    -- 'DbSync.Worker.Ledger.DepositAccumulator.recordEpochParams'; the
    -- consumer drains completed epochs at each epoch boundary and
    -- flushes them to @epoch_param_pending@ before advancing
    -- @dbsync_sync_state.last_committed_slot@.
  , leControlConnection    :: !ControlConnection
    -- ^ PG connection used by the snapshot-writer thread to record
    -- successful snapshot completions in
    -- @dbsync_sync_state.last_snapshot_slot@.
  , leCurrentPhase         :: !CurrentPhase
    -- ^ Live lifecycle phase, shared with 'CoreEnv'. The worker
    -- reads 'isFollowPath' on every apply to choose snapshot
    -- cadence: lagging (every 10 epochs) during Ingest, near-tip
    -- (every epoch) once Follow has started.
  }

-- | Constructor for 'NoLedgerEnv'. In 'IO' purely to keep the shape
-- symmetric with @mkHasLedgerEnv@, which genuinely does need 'IO'
-- for @StrictTVar@ allocation and LSM session setup.
mkNoLedgerEnv
  :: AppTracer
  -> Consensus.ProtocolInfo (CardanoBlock StandardCrypto)
  -> SystemStart
  -> Ledger.Network
  -> IO NoLedgerEnv
mkNoLedgerEnv tracer pinfo start network =
  pure
    NoLedgerEnv
      { nleTracer       = tracer
      , nleProtocolInfo = pinfo
      , nleSystemStart  = start
      , nleNetwork      = network
      }

-- ---------------------------------------------------------------------------
-- * Configuration switches
-- ---------------------------------------------------------------------------

-- | What the ledger worker does when it observes an invalid ledger
-- state.
data PanicPolicy
  = AbortOnPanic
  | LogAndContinue
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * LedgerDB and its elements
-- ---------------------------------------------------------------------------

-- | In-memory LedgerDB: at most 100 recent 'DbSyncStateRef' values,
-- newest first. Shallow rollbacks are served entirely from this
-- buffer; deeper rollbacks fall back to disk snapshots.
newtype LedgerDB = LedgerDB
  { ledgerDbCheckpoints :: StrictSeq DbSyncStateRef
  }

-- | A 'CardanoLedgerState' paired with its LSM tables handle and a
-- guard flag that a snapshot write toggles while it's using the
-- handle.
--
-- The 'srCanClose' 'StrictTVar' is the explicit synchronisation point
-- between the snapshot writer and the checkpoint-buffer pruner: we
-- never 'close' a handle while a snapshot is mid-write.
data DbSyncStateRef = DbSyncStateRef
  { srState    :: !CardanoLedgerState
  , srTables   :: !(LedgerTablesHandle IO (ExtLedgerState (CardanoBlock StandardCrypto)))
  , srCanClose :: !(StrictTVar IO Bool)
  }

-- | The pure parts of the ledger state — no tables, no handles. This
-- is cheap to copy and lives inside the 'LedgerDB' checkpoint
-- sequence.
data CardanoLedgerState = CardanoLedgerState
  { clsState        :: !(ExtLedgerState (CardanoBlock StandardCrypto) EmptyMK)
  , clsEpochBlockNo :: !EpochBlockNo
  }

-- | Block number within the current epoch.
--
-- 'EpochBlockNo' is a counter; 'ByronEpochBlockNo' is the
-- \"we don't track this in Byron\" tag (pre-Shelley stake slicing
-- isn't meaningful).
--
-- The derived 'Ord' orders 'ByronEpochBlockNo' last (constructor
-- order) — we never actually compare across the two constructors, so
-- any ordering is fine as long as 'EpochBlockNo' is monotone in its
-- payload.
data EpochBlockNo
  = EpochBlockNo !Word64
  | ByronEpochBlockNo
  deriving stock (Eq, Ord, Show)

-- | The consensus-layer 'StateRef' shape — what 'SnapshotManager'
-- APIs consume and produce. 'toConsensusStateRef' \/
-- 'fromConsensusStateRef' bridge between this and our
-- 'DbSyncStateRef'.
type ConsensusStateRef = Consensus.StateRef IO (ExtLedgerState (CardanoBlock StandardCrypto))

-- | Project a 'DbSyncStateRef' into the consensus-layer shape.
toConsensusStateRef :: DbSyncStateRef -> ConsensusStateRef
toConsensusStateRef sr =
  Consensus.StateRef (clsState $ srState sr) (srTables sr)

-- | Inject a consensus-layer 'StateRef' into our 'DbSyncStateRef',
-- allocating a fresh 'srCanClose' flag that starts @True@.
fromConsensusStateRef :: EpochBlockNo -> ConsensusStateRef -> IO DbSyncStateRef
fromConsensusStateRef ebn (Consensus.StateRef st tbl) = do
  canClose <- newTVarIO True
  pure
    DbSyncStateRef
      { srState =
          CardanoLedgerState
            { clsState        = st
            , clsEpochBlockNo = ebn
            }
      , srTables   = tbl
      , srCanClose = canClose
      }

-- | Build the initial 'DbSyncStateRef' from genesis using
-- 'leInitGenesis'. Only callable in the 'LedgerEnabled' arm.
initCardanoLedgerState :: LedgerEnv -> IO DbSyncStateRef
initCardanoLedgerState env = do
  consensusRef <- leInitGenesis env
  fromConsensusStateRef ByronEpochBlockNo consensusRef

-- | Derive 'EpochBlockNo' from a ledger state.
--
-- For Shelley+ eras sums 'nesBcur' (blocks made this epoch). For
-- Byron returns 'ByronEpochBlockNo' — pre-Shelley we don't need
-- stake-slicing indices.
deriveEpochBlockNo :: ExtLedgerState (CardanoBlock StandardCrypto) mk -> EpochBlockNo
deriveEpochBlockNo st =
  case ledgerState st of
    LedgerStateByron _     -> ByronEpochBlockNo
    LedgerStateShelley sls -> countBlocks sls
    LedgerStateAllegra als -> countBlocks als
    LedgerStateMary mls    -> countBlocks mls
    LedgerStateAlonzo als  -> countBlocks als
    LedgerStateBabbage bls -> countBlocks bls
    LedgerStateConway cls  -> countBlocks cls
    LedgerStateDijkstra dls -> countBlocks dls
  where
    countBlocks :: LedgerState (ShelleyBlock p era) mk -> EpochBlockNo
    countBlocks lstate =
      let nes = shelleyLedgerState lstate
          bm  = nes ^. Shelley.nesBcurL
       in EpochBlockNo $ fromIntegral $ sum bm

-- ---------------------------------------------------------------------------
-- * Snapshot bookkeeping
-- ---------------------------------------------------------------------------

-- | Snapshot origin — on-disk (consensus 'DiskSnapshot') or
-- in-memory at a 'CardanoPoint' in the 'LedgerDB' buffer.
data SnapshotPoint
  = OnDisk !DiskSnapshot
  | InMemory !CardanoPoint

-- ---------------------------------------------------------------------------
-- * Block application plumbing
-- ---------------------------------------------------------------------------

-- | Map from tx-body hash to the deposit value charged for that tx
-- (reward / proposal / stake deposits). Populated incrementally from
-- deposit events; consumed by the tx-insertion path.
newtype DepositsMap = DepositsMap
  { unDepositsMap :: Map ByteString Coin
  }
  deriving stock (Eq, Show)

instance NFData DepositsMap where
  rnf (DepositsMap m) = rnf m

-- | 'Just' the deposit for this tx-body hash, or 'Nothing' if no
-- deposit event was observed (plain transfer).
lookupDepositsMap :: ByteString -> DepositsMap -> Maybe Coin
lookupDepositsMap bs = Map.lookup bs . unDepositsMap

-- | An empty deposits map.
emptyDepositsMap :: DepositsMap
emptyDepositsMap = DepositsMap Map.empty

-- | Result of applying a single block.
--
-- Accumulates everything the downstream insert \/ epoch-boundary
-- paths need: the protocol params at this block, the rewards
-- ledger-event stream, the NewEpoch summary on epoch boundaries, and
-- the deposits map.
data ApplyResult = ApplyResult
  { apPrices          :: !(Strict.Maybe Prices)
  , apGovExpiresAfter :: !(Strict.Maybe Ledger.EpochInterval)
  , apNewEpoch        :: !(Strict.Maybe Generic.NewEpoch)
    -- ^ Only 'Just' for the first block of a new epoch.
  , apDeposits        :: !(Strict.Maybe Generic.Deposits)
  , apSlotDetails     :: !SlotDetails
  , apStakeSlice      :: !Generic.StakeSliceRes
  , apEvents          :: ![LedgerEvent]
  , apGovActionState  :: !(Maybe (ConwayGovState ConwayEra))
  , apDepositsMap     :: !DepositsMap
  , apPoolsRegistered :: !(Set.Set ByteString)
    -- ^ Raw pool-hash bytes registered in the ledger before this
    -- block was applied. Drives the pool_update.active_epoch_no
    -- reactivation offset.
  }

-- | A no-op 'ApplyResult' that only carries 'SlotDetails'. Useful
-- seed value when no block events fired.
defaultApplyResult :: SlotDetails -> ApplyResult
defaultApplyResult slotDetails =
  ApplyResult
    { apPrices          = Strict.Nothing
    , apGovExpiresAfter = Strict.Nothing
    , apNewEpoch        = Strict.Nothing
    , apDeposits        = Strict.Nothing
    , apSlotDetails     = slotDetails
    , apStakeSlice      = Generic.NoSlices
    , apEvents          = []
    , apGovActionState  = Nothing
    , apDepositsMap     = emptyDepositsMap
    , apPoolsRegistered = Set.empty
    }

-- | Pointer-identity cache for the registered-pools projection
-- ('DbSync.Worker.Ledger.State.getRegisteredPools'). The ledger's
-- registered-pool 'Map' is structure-shared across every block that
-- carries no pool certificate, so the projected hash-bytes set only
-- needs rebuilding when the underlying 'Map' object actually
-- changes. Keyed on the 'StableName' of that 'Map'; a false negative
-- (fresh object, same contents — e.g. across an era transition)
-- merely rebuilds.
data RegisteredPoolsCache =
  forall pools. RegisteredPoolsCache !(StableName pools) !(Set.Set ByteString)

-- | A committee member resolved from the ledger for a committee-updating
-- proposal, projected to the fields @committee_member@ needs so the
-- queued value holds no reference to the ledger state it came from.
data ProposedCommitteeMember = ProposedCommitteeMember
  { pcmColdKeyHash :: !ByteString
  , pcmIsScript    :: !Bool
  , pcmExpiryEpoch :: !Word64
  }

instance NFData ProposedCommitteeMember where
  rnf (ProposedCommitteeMember coldKeyHash isScript expiryEpoch) =
    rnf (coldKeyHash, isScript, expiryEpoch)

-- | Per-block projection the consumer needs, carved out of the full
-- 'ApplyResult'. Built and forced to normal form before being
-- enqueued on 'leBlockApplyResults' so a buffered entry never pins
-- the ledger state it was derived from.
data BlockApplyData = BlockApplyData
  { badDepositsMap      :: !DepositsMap
  , badStakeSlice       :: !Generic.StakeSliceRes
  , badPoolsRegistered  :: !(Set.Set ByteString)
  , badGovExpiresAfter  :: !(Strict.Maybe Ledger.EpochInterval)
  , badStakeKeyDeposit  :: !(Strict.Maybe Coin)
  , badPoolDeposit      :: !(Strict.Maybe Coin)
  , badPrices           :: !(Strict.Maybe Prices)
      -- ^ Plutus execution prices from this block's protocol params;
      -- 'Strict.Nothing' pre-Alonzo. Drives @redeemer.fee@.
  , badCommitteeMembers :: !(Map.Map (ByteString, Word64) [ProposedCommitteeMember])
      -- ^ Full resolved committee per committee-updating proposal in
      -- this block, keyed by @(proposal tx hash, proposal index)@.
  }

instance NFData BlockApplyData where
  rnf (BlockApplyData depositsMap stakeSlice poolsRegistered govExpiresAfter stakeKeyDeposit poolDeposit prices committeeMembers) =
    rnf ( (depositsMap, stakeSlice, poolsRegistered)
        , (govExpiresAfter, stakeKeyDeposit, poolDeposit, prices, committeeMembers)
        )

-- | Per-boundary projection the epoch-boundary consumer needs, carved
-- out of the full 'ApplyResult'. Built from the finalised ledger
-- state (so any DRep pulser is already complete) and forced to normal
-- form before being enqueued on 'leBoundaryApplyResults', so a queued
-- entry never pins the 'NewEpochState' generation it was derived from.
data BoundaryApplyData = BoundaryApplyData
  { bndNewEpoch          :: !(Strict.Maybe Generic.NewEpoch)
  , bndEvents            :: ![LedgerEvent]
  , bndGovActionState    :: !(Maybe (ConwayGovState ConwayEra))
  , bndGovExpiresAfter   :: !(Strict.Maybe Ledger.EpochInterval)
  , bndSlotDetails       :: !SlotDetails
  , bndCatchupStakeSlice :: !Generic.StakeSliceRes
      -- ^ Tail of the ended epoch's stake distribution that per-block
      -- slicing never reached (epochs with fewer than @k@ blocks).
      -- 'Generic.NoSlices' when the per-block path covered everything.
  }

instance NFData BoundaryApplyData where
  -- 'bndSlotDetails' is a strict field of small scalars (already WHNF when
  -- the record is), so only the heavy projections need forcing to normal form.
  rnf (BoundaryApplyData newEpoch events govActionState govExpiresAfter _slotDetails catchupSlice) =
    rnf ((newEpoch, events), (govActionState, govExpiresAfter, catchupSlice))

-- | Target epoch at which a governance-action deposit will expire,
-- given the current epoch and the 'apGovExpiresAfter' delta.
getGovExpiresAt :: ApplyResult -> EpochNo -> Maybe EpochNo
getGovExpiresAt applyResult e = case apGovExpiresAfter applyResult of
  Strict.Just ei -> Just $ Ledger.addEpochInterval e ei
  Strict.Nothing -> Nothing

-- | Build the Conway 'Committee' resulting from a governance update:
-- members to remove are dropped, members to add are merged in, and
-- the quorum is overridden.
--
-- TODO: reuse this function from ledger once it's exported there.
updatedCommittee
  :: Set.Set (Credential ColdCommitteeRole)
  -> Map.Map (Credential ColdCommitteeRole) EpochNo
  -> Ledger.UnitInterval
  -> Ledger.StrictMaybe (Committee ConwayEra)
  -> Committee ConwayEra
updatedCommittee membersToRemove membersToAdd newQuorum committee =
  case committee of
    Ledger.SNothing -> Committee membersToAdd newQuorum
    Ledger.SJust (Committee currentMembers _) ->
      let newCommitteeMembers =
            Map.union
              membersToAdd
              (currentMembers `Map.withoutKeys` membersToRemove)
       in Committee newCommitteeMembers newQuorum

-- ---------------------------------------------------------------------------
-- * Per-era NewEpochState access
-- ---------------------------------------------------------------------------

-- | Per-era 'NewEpochState' getter \/ setter.
--
-- Note: this is a slight abuse of the @cardano-ledger@ \/
-- @ouroboros-consensus@ public APIs — ledger state isn't designed to
-- be mutated wholesale this way. We only do so in the replay loop
-- when patching an intermediate @NewEpochState@ back into the
-- hard-fork @LedgerState@, and it's confined to this class.
class HasNewEpochState era where
  getNewEpochState :: ExtLedgerState (CardanoBlock StandardCrypto) mk -> Maybe (NewEpochState era)
  applyNewEpochState
    :: NewEpochState era
    -> ExtLedgerState (CardanoBlock StandardCrypto) mk
    -> ExtLedgerState (CardanoBlock StandardCrypto) mk

instance HasNewEpochState ShelleyEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateShelley shelley -> Just (shelleyLedgerState shelley)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState AllegraEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateAllegra allegra -> Just (shelleyLedgerState allegra)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState MaryEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateMary mary -> Just (shelleyLedgerState mary)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState AlonzoEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateAlonzo alonzo -> Just (shelleyLedgerState alonzo)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn id
        :* fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState BabbageEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateBabbage babbage -> Just (shelleyLedgerState babbage)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState ConwayEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateConway conway -> Just (shelleyLedgerState conway)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* Nil

-- | Lift a per-era Shelley-block @LedgerState@ updater through the
-- hard-fork combinator. The Byron slot is left alone (@fn id@).
hApplyExtLedgerState
  :: NP (Flip LedgerState mk -.-> Flip LedgerState mk) (CardanoShelleyEras StandardCrypto)
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
hApplyExtLedgerState f ledger =
  case ledgerState ledger of
    HardForkLedgerState hfState ->
      let newHfState = hap (fn id :* f) hfState
       in updateLedgerState $ HardForkLedgerState newHfState
  where
    updateLedgerState st = ledger {ledgerState = st}

-- | Per-era updater: replace the @NewEpochState@ inside a single
-- Shelley-family @LedgerState@.
applyNewEpochState'
  :: NewEpochState era
  -> Flip LedgerState mk (ShelleyBlock proto era)
  -> Flip LedgerState mk (ShelleyBlock proto era)
applyNewEpochState' newEpochState' ledger =
  Flip $ updateNewEpochState (unFlip ledger)
  where
    updateNewEpochState l = l {shelleyLedgerState = newEpochState'}

-- | A 'Traversal\'' into the 'NewEpochState' of the current era.
newEpochStateT
  :: HasNewEpochState era
  => Traversal' (ExtLedgerState (CardanoBlock StandardCrypto) mk) (NewEpochState era)
newEpochStateT f ledger =
  case getNewEpochState ledger of
    Just newEpochState' -> flip applyNewEpochState ledger <$> f newEpochState'
    Nothing -> pure ledger
