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
-- Pattern-match at every site where behaviour differs. The
-- 'LedgerDisabled' arm still carries a 'ProtocolInfo', because block
-- deserialisation needs one, but nothing ledger-stateful.
data HasLedgerEnv
  = LedgerEnabled  !LedgerEnv
  | LedgerDisabled !NoLedgerEnv

-- | Environment for @ledger.enabled = false@: no LSM session, no
-- snapshot queue, no 'LedgerDB'.
data NoLedgerEnv = NoLedgerEnv
  { nleTracer       :: !AppTracer
  , nleProtocolInfo :: !(Consensus.ProtocolInfo (CardanoBlock StandardCrypto))
  , nleSystemStart  :: !SystemStart
  , nleNetwork      :: !Ledger.Network
  }

-- | Environment when the ledger feature is enabled: the checkpoint
-- buffer, the inter-thread queues, and the snapshot pipeline.
data LedgerEnv = LedgerEnv
  { leTracer               :: !AppTracer
  , leRewardsCapture       :: !RewardsCapture
    -- ^ When 'DropRewards', reward-related 'LedgerEvent' values are
    -- dropped at the consensus-event conversion boundary.
  , leProtocolInfo         :: !(Consensus.ProtocolInfo (CardanoBlock StandardCrypto))
  , leDir                  :: !FilePath
    -- ^ Root state directory. The LSM session and the snapshot headers
    -- both live under this path.
  , leNetwork              :: !Ledger.Network
  , leMaxSupply            :: !Word64
  , leSystemStart          :: !SystemStart
  , lePanicPolicy          :: !PanicPolicy
    -- ^ What the worker does when it observes an invalid ledger
    -- state. 'AbortOnPanic' tears the process down; 'LogAndContinue'
    -- records the error and keeps applying.
  , leSnapshotNearTipEpoch :: !Word64
    -- ^ Epoch threshold past which every epoch snapshots, whatever the
    -- sync-state cadence says. Default 580.
  , leLedgerBackend        :: !LedgerBackend
  , leInterpreter          :: !(StrictTVar IO (Strict.Maybe CardanoInterpreter))
  , leStateVar             :: !(StrictTVar IO (Strict.Maybe LedgerDB))

    -- Inter-thread coordination queues and TMVars
  , leStopVar              :: !(StrictTVar IO Bool)
    -- ^ Shutdown flag for the ledger worker and the snapshot writer.
    -- Both park in @atomically readTBQueue@, and an async exception
    -- thrown at a thread in that state is not delivered while the
    -- process is otherwise idle. Flipping this TVar wakes them through
    -- STM instead, so they finish the in-flight item and return.
  , leLedgerQueue          :: !(TBQueue ChainSyncMsg)
    -- ^ @BlockReceiver → LedgerWorker@. Forward blocks apply against
    -- the LSM-backed ledger; rollback markers apply against the in-RAM
    -- checkpoint buffer. 'MsgForward' carries a raw 'CardanoBlock', not
    -- a parsed 'GenericBlock', because 'applyBlock' calls
    -- 'tickThenReapplyLedgerResult' which needs the consensus shape.
  , leEpochReady           :: !(StrictTMVar IO EpochNo)
    -- ^ Unused. The worker writes the epoch number here, but nothing
    -- reads it — boundary coordination goes through
    -- 'leBoundaryApplyResults'.
  , leEpochWait            :: !(StrictTMVar IO EpochNo)
    -- ^ Unused. The worker polls this, but nothing writes it.

    -- Consensus snapshot machinery: async writer plus manager
  , leSnapshotQueue        :: !(TBQueue DbSyncStateRef)
    -- ^ @LedgerWorker → LedgerSnapshotWriter@. Deferred snapshot writes.
  , leSnapshotManager      :: !(SnapshotManager IO IO (CardanoBlock StandardCrypto) ConsensusStateRef)
  , leInitGenesis          :: !(IO ConsensusStateRef)
    -- ^ Build the first consensus 'StateRef' from genesis, on a cold
    -- start with no on-disk snapshots.
  , leLoadSnapshot         :: !(DiskSnapshot -> IO (Either Text ConsensusStateRef))
    -- ^ Load a snapshot from disk through the configured backend.
  , leClose                :: !(IO ())
    -- ^ Release the LSM session and file lock at shutdown. Consensus
    -- 'mkResources' allocates the session with 'allocateTemp', so the
    -- temp registry does not close it on scope exit. This call must.
  , leLatestApplyResult    :: !(StrictTVar IO (Strict.Maybe ApplyResult))
    -- ^ Latest 'ApplyResult' from the worker, overwritten on every
    -- applied block. 'waitForApplyResultAt' uses it as a slot-reached
    -- barrier, and Follow reads it per block.
  , leBoundaryApplyResults :: !(TBQueue BoundaryApplyData)
    -- ^ FIFO of one entry per epoch boundary. Each entry is forced to
    -- normal form before enqueue, so a queued boundary never pins the
    -- 'NewEpochState' generation it came from.
  , leBlockApplyResults    :: !(TBQueue BlockApplyData)
    -- ^ FIFO of one fully-forced entry per applied block. The consumer
    -- drains one per processed block and discards replay-window
    -- entries. Bounded, so a lagging consumer back-pressures the worker.
  , leRegisteredPoolsCache :: !(IORef (Maybe RegisteredPoolsCache))
    -- ^ Worker-private cache behind
    -- 'DbSync.Worker.Ledger.State.getRegisteredPools'. Only the worker
    -- thread touches it.
  , leDepositAccumulator   :: !EpochParamsRef
    -- ^ Per-epoch stake-key and pool deposit values. The worker records
    -- each applied non-replay block. The consumer drains completed
    -- epochs at each boundary and flushes them to
    -- @epoch_param_pending@ before it advances
    -- @dbsync_sync_state.last_committed_slot@.
  , leControlConnection    :: !ControlConnection
    -- ^ PG connection the snapshot-writer thread uses to record
    -- completions in @dbsync_sync_state.last_snapshot_slot@.
  , leCurrentPhase         :: !CurrentPhase
    -- ^ Live lifecycle phase, shared with 'CoreEnv'. The worker reads
    -- 'isFollowPath' on every apply to pick the snapshot cadence:
    -- every 10 epochs during Ingest, every epoch once Follow starts.
  }

-- | In 'IO' only to match @mkHasLedgerEnv@, which needs 'IO' for
-- @StrictTVar@ allocation and LSM session setup.
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

-- | What the ledger worker does when it sees an invalid ledger state.
data PanicPolicy
  = AbortOnPanic
  | LogAndContinue
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * LedgerDB and its elements
-- ---------------------------------------------------------------------------

-- | In-RAM checkpoint buffer over the LSM-backed ledger: the most
-- recent 'DbSyncStateRef' values, newest first. It serves shallow
-- rollbacks; deeper ones fall back to disk snapshots.
newtype LedgerDB = LedgerDB
  { ledgerDbCheckpoints :: StrictSeq DbSyncStateRef
  }

-- | A 'CardanoLedgerState' with its LSM tables handle and a guard flag.
--
-- 'srCanClose' is the synchronisation point between the snapshot writer
-- and the checkpoint-buffer pruner. A handle never closes while a
-- snapshot write holds it.
data DbSyncStateRef = DbSyncStateRef
  { srState    :: !CardanoLedgerState
  , srTables   :: !(LedgerTablesHandle IO (ExtLedgerState (CardanoBlock StandardCrypto)))
  , srCanClose :: !(StrictTVar IO Bool)
  }

-- | The pure parts of the ledger state: no tables, no handles. Cheap to
-- copy, and it lives inside the checkpoint sequence.
data CardanoLedgerState = CardanoLedgerState
  { clsState        :: !(ExtLedgerState (CardanoBlock StandardCrypto) EmptyMK)
  , clsEpochBlockNo :: !EpochBlockNo
  }

-- | Block number within the current epoch. 'ByronEpochBlockNo' is the
-- \"not tracked\" tag, because pre-Shelley stake slicing has no
-- meaning.
--
-- The derived 'Ord' puts 'ByronEpochBlockNo' last. Nothing compares
-- across the two constructors, so only monotonicity in the
-- 'EpochBlockNo' payload matters.
data EpochBlockNo
  = EpochBlockNo !Word64
  | ByronEpochBlockNo
  deriving stock (Eq, Ord, Show)

-- | The 'StateRef' shape the 'SnapshotManager' API speaks.
-- 'toConsensusStateRef' and 'fromConsensusStateRef' bridge it to
-- 'DbSyncStateRef'.
type ConsensusStateRef = Consensus.StateRef IO (ExtLedgerState (CardanoBlock StandardCrypto))

toConsensusStateRef :: DbSyncStateRef -> ConsensusStateRef
toConsensusStateRef sr =
  Consensus.StateRef (clsState $ srState sr) (srTables sr)

-- | Wrap a consensus 'StateRef' as a 'DbSyncStateRef' with a fresh
-- 'srCanClose' flag that starts @True@.
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

-- | Build the first 'DbSyncStateRef' from genesis with 'leInitGenesis'.
initCardanoLedgerState :: LedgerEnv -> IO DbSyncStateRef
initCardanoLedgerState env = do
  consensusRef <- leInitGenesis env
  fromConsensusStateRef ByronEpochBlockNo consensusRef

-- | Sums 'nesBcur', the blocks made this epoch, for Shelley and later.
-- Byron gives 'ByronEpochBlockNo': pre-Shelley needs no stake-slicing
-- index.
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

-- | Snapshot origin: an on-disk 'DiskSnapshot', or a 'CardanoPoint' in
-- the checkpoint buffer.
data SnapshotPoint
  = OnDisk !DiskSnapshot
  | InMemory !CardanoPoint

-- ---------------------------------------------------------------------------
-- * Block application plumbing
-- ---------------------------------------------------------------------------

-- | Tx-body hash to the deposit charged for that tx: reward, proposal
-- or stake deposits. Filled from deposit events, read by the
-- tx-insertion path.
newtype DepositsMap = DepositsMap
  { unDepositsMap :: Map ByteString Coin
  }
  deriving stock (Eq, Show)

instance NFData DepositsMap where
  rnf (DepositsMap m) = rnf m

-- | 'Nothing' when no deposit event fired for this tx: a plain transfer.
lookupDepositsMap :: ByteString -> DepositsMap -> Maybe Coin
lookupDepositsMap bs = Map.lookup bs . unDepositsMap

emptyDepositsMap :: DepositsMap
emptyDepositsMap = DepositsMap Map.empty

-- | Result of applying a single block, published on
-- 'leLatestApplyResult'.
--
-- The bulk per-block and per-boundary payloads travel on
-- 'BlockApplyData' and 'BoundaryApplyData' instead, so the fields that
-- would carry them here stay empty.
data ApplyResult = ApplyResult
  { apPrices          :: !(Strict.Maybe Prices)
    -- ^ Plutus execution prices; 'Strict.Nothing' pre-Alonzo.
  , apGovExpiresAfter :: !(Strict.Maybe Ledger.EpochInterval)
    -- ^ Gov-action deposit lifetime; 'Strict.Nothing' pre-Conway.
  , apNewEpoch        :: !(Strict.Maybe Generic.NewEpoch)
    -- ^ Only 'Just' for the first block of a new epoch.
  , apDeposits        :: !(Strict.Maybe Generic.Deposits)
    -- ^ Protocol-param deposits; 'Strict.Nothing' pre-Shelley.
  , apSlotDetails     :: !SlotDetails
    -- ^ Slot, epoch and time of the applied block.
  , apStakeSlice      :: !Generic.StakeSliceRes
    -- ^ Unused. Always 'Generic.NoSlices'; the live slice travels on
    -- 'badStakeSlice'.
  , apEvents          :: ![LedgerEvent]
    -- ^ Unused. Always empty; the live events travel on 'bndEvents'.
  , apGovActionState  :: !(Maybe (ConwayGovState ConwayEra))
    -- ^ Unused. Always 'Nothing'; the live state travels on
    -- 'bndGovActionState'.
  , apDepositsMap     :: !DepositsMap
    -- ^ Unused. Always empty; the live map travels on 'badDepositsMap'.
  , apPoolsRegistered :: !(Set.Set ByteString)
    -- ^ Unused. Always empty; the live set travels on
    -- 'badPoolsRegistered'.
  }

-- | An 'ApplyResult' that carries only 'SlotDetails'. Seed value for
-- when no block events fired.
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

-- | Pointer-identity cache behind
-- 'DbSync.Worker.Ledger.State.getRegisteredPools'.
--
-- The ledger's registered-pool 'Map' is structure-shared across every
-- block with no pool certificate, so the hash-bytes set rebuilds only
-- when the 'Map' object itself changes. The key is the 'StableName' of
-- that 'Map'. A false negative, such as a fresh object with the same
-- contents across an era transition, only causes a rebuild.
data RegisteredPoolsCache =
  forall pools. RegisteredPoolsCache !(StableName pools) !(Set.Set ByteString)

-- | A committee member resolved from the ledger for a
-- committee-updating proposal, cut down to the fields
-- @committee_member@ needs. The queued value therefore holds no
-- reference to the ledger state it came from.
data ProposedCommitteeMember = ProposedCommitteeMember
  { pcmColdKeyHash :: !ByteString
  , pcmIsScript    :: !Bool
  , pcmExpiryEpoch :: !Word64
  }

instance NFData ProposedCommitteeMember where
  rnf (ProposedCommitteeMember coldKeyHash isScript expiryEpoch) =
    rnf (coldKeyHash, isScript, expiryEpoch)

-- | The per-block data the consumer needs. Forced to normal form before
-- it goes on 'leBlockApplyResults', so a buffered entry never pins the
-- ledger state it came from.
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

-- | The per-boundary data the epoch-boundary consumer needs. Built from
-- the finalised ledger state, so any DRep pulser is already complete,
-- and forced to normal form before it goes on
-- 'leBoundaryApplyResults'. A queued entry therefore never pins the
-- 'NewEpochState' generation it came from.
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
  -- 'bndSlotDetails' is a strict field of small scalars, already WHNF
  -- when the record is, so only the heavy fields need forcing.
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

-- | Per-era 'NewEpochState' getter and setter.
--
-- This abuses the @cardano-ledger@ and @ouroboros-consensus@ public
-- APIs: ledger state is not designed for wholesale mutation. The replay
-- loop needs it to patch an intermediate @NewEpochState@ back into the
-- hard-fork @LedgerState@, and this class confines the abuse.
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
