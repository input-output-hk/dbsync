{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : DbSync.Ledger.Types
Description : Core types for the ledger subsystem.

The top-level shape is a sum over \"ledger enabled\" vs \"ledger
disabled\":

@
data HasLedgerEnv
  = LedgerEnabled  !LedgerEnv
  | LedgerDisabled !NoLedgerEnv
@

The @LedgerDisabled@ arm carries only what we genuinely need when the
ledger feature is off (tracer, protocol info, system start, network);
it allocates no LSM session, no snapshot queue, no @LedgerDB@
checkpoint buffer. Code paths that want to start a 'LedgerWorker' or
take snapshots simply don't exist on the disabled arm.
-}
module DbSync.Ledger.Types
  ( -- * Top-level sum
    HasLedgerEnv (..)
  , NoLedgerEnv (..)
  , LedgerEnv (..)
  , mkNoLedgerEnv

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
import qualified Data.Map.Strict as Map
import Data.SOP.Functors (Flip (..))
import Data.SOP.Strict (NP (..), fn, hap, type (-.->))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Lens.Micro (Traversal', (^.))
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

import DbSync.Block.Types (GenericBlock)
import DbSync.Config.Types (LedgerBackend)
import qualified DbSync.Era.Shelley.Generic.EpochUpdate as Generic
import qualified DbSync.Era.Shelley.Generic.ProtoParams as Generic
import qualified DbSync.Era.Shelley.Generic.StakeDist as Generic
import DbSync.Ledger.Event (LedgerEvent)
import DbSync.Ledger.Keys (PoolKeyHash)
import DbSync.Node.Connection (CardanoBlock, CardanoPoint)
import DbSync.StateQuery (CardanoInterpreter, SlotDetails)
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
-- lives in 'DbSync.Env.IngestEnv' when the user has set
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
  , leHasRewards           :: !Bool
    -- ^ If 'False', reward-related 'LedgerEvent' values are dropped
    -- at the consensus-event conversion boundary.
  , leProtocolInfo         :: !(Consensus.ProtocolInfo (CardanoBlock StandardCrypto))
  , leDir                  :: !FilePath
    -- ^ Root state directory (LSM session + consensus snapshots
    -- both live under this path).
  , leNetwork              :: !Ledger.Network
  , leMaxSupply            :: !Word64
  , leSystemStart          :: !SystemStart
  , leAbortOnPanic         :: !Bool
  , leSnapshotNearTipEpoch :: !Word64
    -- ^ Epoch threshold past which we always snapshot every epoch,
    -- regardless of sync-state cadence. Default 580.
  , leLedgerBackend        :: !LedgerBackend
  , leInterpreter          :: !(StrictTVar IO (Strict.Maybe CardanoInterpreter))
  , leStateVar             :: !(StrictTVar IO (Strict.Maybe LedgerDB))
    -- * Inter-thread coordination queues and TMVars
  , leLedgerQueue          :: !(TBQueue GenericBlock)
    -- ^ @BlockReceiver → LedgerWorker@ — blocks to apply.
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
  , apPoolsRegistered :: !(Set.Set PoolKeyHash)
    -- ^ Pool registrations observed __before__ the block was applied.
  , apNewEpoch        :: !(Strict.Maybe Generic.NewEpoch)
    -- ^ Only 'Just' for the first block of a new epoch.
  , apOldLedger       :: !(Strict.Maybe CardanoLedgerState)
  , apDeposits        :: !(Strict.Maybe Generic.Deposits)
  , apSlotDetails     :: !SlotDetails
  , apStakeSlice      :: !Generic.StakeSliceRes
  , apEvents          :: ![LedgerEvent]
  , apGovActionState  :: !(Maybe (ConwayGovState ConwayEra))
  , apDepositsMap     :: !DepositsMap
  }

-- | A no-op 'ApplyResult' that only carries 'SlotDetails'. Useful
-- seed value when no block events fired.
defaultApplyResult :: SlotDetails -> ApplyResult
defaultApplyResult slotDetails =
  ApplyResult
    { apPrices          = Strict.Nothing
    , apGovExpiresAfter = Strict.Nothing
    , apPoolsRegistered = Set.empty
    , apNewEpoch        = Strict.Nothing
    , apOldLedger       = Strict.Nothing
    , apDeposits        = Strict.Nothing
    , apSlotDetails     = slotDetails
    , apStakeSlice      = Generic.NoSlices
    , apEvents          = []
    , apGovActionState  = Nothing
    , apDepositsMap     = emptyDepositsMap
    }

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
