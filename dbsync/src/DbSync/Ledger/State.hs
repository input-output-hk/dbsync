{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : DbSync.Ledger.State
Description : Core ledger-state operations — LedgerDB buffer, block application, rollback.

This module sits between the consensus-provided LedgerDB V2 machinery
and the rest of the sync engine. It owns:

  * The in-memory 100-entry 'LedgerDB' checkpoint buffer (push/prune,
    current-tip lookup, atomic read/write against the
    'LedgerEnv.leStateVar' 'StrictTVar').
  * 'applyBlock' — the single-block entry point that reads the current
    checkpoint, runs @tickThenReapply@ against the block, updates the
    LSM tables handle, and returns an 'ApplyResult' summarising
    everything the downstream extractors need.
  * 'loadLedgerAtPoint' — rollback entry point. Walks the in-memory
    buffer first; falls back to a disk-snapshot load when the target
    is older than our buffer can reach.

Small projections (@getPrices@, @getRegisteredPools@, @findAdaPots@,
@findProposedCommittee@, @getStakeSlice@) are pure helpers exported
for the extractor layer — each reads one thing out of a
'CardanoLedgerState' or an event stream.

The LSM-dependent parts of @applyBlock@ and the disk-fallback leg of
@loadLedgerAtPoint@ are staged: they show up here as
@panic \"TODO: …\"@ placeholders so the module compiles standalone,
and get wired up when the 'DbSync.Ledger.Worker' thread and the
snapshot machinery land.
-}
module DbSync.Ledger.State
  ( -- * LedgerDB management
    pushLedgerDB
  , pruneLedgerDb
  , ledgerDbCheckpointBufferSize
  , ledgerDbCurrent
  , writeLedgerState
  , readCurrentStateUnsafe

    -- * Environment construction
  , mkHasLedgerEnv

    -- * Block application
  , applyBlock
  , applyBlockAndSnapshot
  , tickThenReapplyCheckHash

    -- * Rollback
  , loadLedgerAtPoint

    -- * Stake slice shim
  , getStakeSlice

    -- * Governance / ledger projections
  , findProposedCommittee
  , getGovExpiration
  , getGovState
  , getPrices
  , getRegisteredPools

    -- * Miscellaneous helpers
  , getHeaderHash
  , findAdaPots
  ) where

import Cardano.Prelude hiding (atomically)

import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Cardano.Ledger.Alonzo.Scripts (Prices)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core as Shelley
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Governance as Shelley
import Cardano.Ledger.Shelley.AdaPots (AdaPots)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Class.MonadSTM.Strict
  ( atomically
  , newEmptyTMVarIO
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.Concurrent.STM.TBQueue (newTBQueueIO)
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import GHC.IO.Exception (userError)
import Lens.Micro ((^.))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import Ouroboros.Consensus.Cardano.Block (ConwayEra, LedgerState (..), StandardCrypto)
import Ouroboros.Consensus.Config (TopLevelConfig)
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.Ledger.Abstract as Consensus
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.Basics (EmptyMK)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot, SnapshotManager)

import qualified Ouroboros.Network.Block as Network

import DbSync.Config.Types (LedgerBackend)
import qualified DbSync.Era.Shelley.Generic.StakeDist as Generic
import DbSync.Ledger.Event (LedgerEvent (..))
import DbSync.Ledger.Keys (PoolKeyHash)
import DbSync.Ledger.Types
  ( ApplyResult
  , CardanoLedgerState (..)
  , ConsensusStateRef
  , DbSyncStateRef (..)
  , EpochBlockNo (..)
  , HasLedgerEnv (..)
  , LedgerDB (..)
  , LedgerEnv (..)
  , updatedCommittee
  )
import DbSync.Node.Connection (CardanoBlock, CardanoPoint)
import DbSync.Trace.Types (AppTracer)

-- ---------------------------------------------------------------------------
-- * LedgerDB management
-- ---------------------------------------------------------------------------

-- | Hard cap on how many recent 'DbSyncStateRef' values the in-memory
-- buffer retains. Matching this against @k=2160@ would be ideal, but
-- keeping 2160 full state references in RAM is not cheap; 100 gives
-- fast rollback within a tenth of a security-parameter window and
-- forces deeper rollbacks through the disk-snapshot path.
ledgerDbCheckpointBufferSize :: Int
ledgerDbCheckpointBufferSize = 100

-- | Push a new 'DbSyncStateRef' onto the newest end of the
-- 'LedgerDB', then prune the oldest entries back down to
-- 'ledgerDbCheckpointBufferSize'.
--
-- Callers relying on the pruned refs being @close@d on their
-- 'LedgerTablesHandle' should do so separately — this function is
-- purely the sequence-level push\/prune logic.
pushLedgerDB :: LedgerDB -> DbSyncStateRef -> LedgerDB
pushLedgerDB db sref =
  pruneLedgerDb ledgerDbCheckpointBufferSize $
    LedgerDB (sref StrictSeq.<| ledgerDbCheckpoints db)

-- | Keep at most @k@ newest entries; drop any older ones off the end.
pruneLedgerDb :: Int -> LedgerDB -> LedgerDB
pruneLedgerDb k (LedgerDB s) = LedgerDB (StrictSeq.take k s)
{-# INLINE pruneLedgerDb #-}

-- | Newest 'DbSyncStateRef' in the buffer.
--
-- Partial on an empty buffer, which the system maintains as an
-- invariant: the 'LedgerDB' is initialised with the genesis ref at
-- boot and the buffer is only ever re-populated (never emptied) by
-- the rollback path. An empty buffer at this call site is therefore
-- a programmer error and results in a panic.
ledgerDbCurrent :: LedgerDB -> DbSyncStateRef
ledgerDbCurrent (LedgerDB s) =
  case s of
    StrictSeq.Empty        -> panic "ledgerDbCurrent: empty LedgerDB"
    x StrictSeq.:<| _rest  -> x

-- | Replace the shared 'LedgerDB' state in the 'leStateVar' TVar.
-- @'Strict.Nothing'@ clears the buffer (used at rollback to free old
-- references before loading a disk snapshot).
writeLedgerState :: LedgerEnv -> Strict.Maybe LedgerDB -> IO ()
writeLedgerState env mDb = atomically $ writeTVar (leStateVar env) mDb

-- | Read the newest 'ExtLedgerState' out of the buffer. Throws via
-- STM if the buffer hasn't been initialised yet (pre-boot); callers
-- downstream of 'mkHasLedgerEnv' + genesis init should never see
-- that, so we treat it as a programmer error.
readCurrentStateUnsafe
  :: LedgerEnv
  -> IO (ExtLedgerState (CardanoBlock StandardCrypto) EmptyMK)
readCurrentStateUnsafe env =
  atomically (clsState . srState . ledgerDbCurrent <$> readStateUnsafe env)

-- | STM inner helper for 'readCurrentStateUnsafe'. Kept private — the
-- 'Strict.Nothing' case throws a descriptive STM error rather than
-- panicking so callers can at least ROLLBACK their transactions.
readStateUnsafe :: LedgerEnv -> STM LedgerDB
readStateUnsafe env = do
  mState <- readTVar $ leStateVar env
  case mState of
    Strict.Nothing -> throwSTM $ userError "DbSync.Ledger.State.readStateUnsafe: LedgerDB not initialised"
    Strict.Just db -> pure db

-- ---------------------------------------------------------------------------
-- * Environment construction
-- ---------------------------------------------------------------------------

-- | Construct a 'LedgerEnv' (the \"enabled\" arm of 'HasLedgerEnv'),
-- allocating its 'StrictTVar's and bounded queues up front.
--
-- The 'SnapshotManager' and the genesis \/ snapshot-loading callbacks
-- are taken as parameters because their construction depends on the
-- 'LedgerBackend' (LSM) and the configured state directory — both
-- concerns that live closer to the boot flow. Callers wire them in
-- once the on-disk directories are initialised.
mkHasLedgerEnv
  :: AppTracer
  -> Consensus.ProtocolInfo (CardanoBlock StandardCrypto)
  -> FilePath                                       -- ^ State directory root
  -> Ledger.Network
  -> Word64                                         -- ^ Max Lovelace supply
  -> SystemStart
  -> Word64                                         -- ^ \"near tip\" epoch threshold (default 580)
  -> Bool                                           -- ^ Capture rewards events in 'ApplyResult'
  -> Bool                                           -- ^ Abort on invalid ledger state
  -> LedgerBackend
  -> SnapshotManager IO IO (CardanoBlock StandardCrypto) ConsensusStateRef
  -> IO ConsensusStateRef                           -- ^ Build genesis state ref
  -> (DiskSnapshot -> IO (Either Text ConsensusStateRef))
                                                    -- ^ Load a disk snapshot
  -> IO HasLedgerEnv
mkHasLedgerEnv
  tracer pinfo dir network maxSupply start snapEpoch
  hasRewards abortOnPanic backend snapManager initGenesis loadSnap = do
    interpreterVar <- newTVarIO Strict.Nothing
    stateVar       <- newTVarIO Strict.Nothing
    ledgerQueue    <- newTBQueueIO ledgerQueueBound
    epochReady     <- newEmptyTMVarIO
    epochWait      <- newEmptyTMVarIO
    snapshotQueue  <- newTBQueueIO snapshotQueueBound
    pure $
      LedgerEnabled
        LedgerEnv
          { leTracer               = tracer
          , leHasRewards           = hasRewards
          , leProtocolInfo         = pinfo
          , leDir                  = dir
          , leNetwork              = network
          , leMaxSupply            = maxSupply
          , leSystemStart          = start
          , leAbortOnPanic         = abortOnPanic
          , leSnapshotNearTipEpoch = snapEpoch
          , leLedgerBackend        = backend
          , leInterpreter          = interpreterVar
          , leStateVar             = stateVar
          , leLedgerQueue          = ledgerQueue
          , leEpochReady           = epochReady
          , leEpochWait            = epochWait
          , leSnapshotQueue        = snapshotQueue
          , leSnapshotManager      = snapManager
          , leInitGenesis          = initGenesis
          , leLoadSnapshot         = loadSnap
          }
  where
    -- Shallow — the worker is a single consumer and we want strong
    -- back-pressure into the receiver as soon as it falls behind.
    ledgerQueueBound :: Natural
    ledgerQueueBound = 100

    -- One slot per retained snapshot (the manager keeps three) plus
    -- a little slack so a mid-write snapshot doesn't block the worker.
    snapshotQueueBound :: Natural
    snapshotQueueBound = 4

-- ---------------------------------------------------------------------------
-- * Block application
-- ---------------------------------------------------------------------------

-- | Apply a single block to the current ledger state, returning the
-- __old__ state ref (for 'apOldLedger' bookkeeping) and the
-- 'ApplyResult' carrying every derived value the extractors need.
--
-- Per the project's ledger-state plan, this runs on the dedicated
-- 'DbSync.Ledger.Worker' thread during 'IngestChainHistory' and
-- inline during 'FollowingChainTip'. It is not yet wired up: the
-- read\/pushDiffs operations against the LSM
-- 'Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq.LedgerTablesHandle'
-- are implemented alongside the worker thread that drives this
-- function — doing so here standalone would mean writing dead code.
applyBlock
  :: LedgerEnv
  -> CardanoBlock StandardCrypto
  -> IO (DbSyncStateRef, ApplyResult)
applyBlock _env _blk =
  panic "DbSync.Ledger.State.applyBlock: LSM block-application path wired when the LedgerWorker lands"

-- | 'applyBlock' plus an optional snapshot request on epoch
-- boundaries. Matches the consensus snapshot cadence described in the
-- ledger-state plan: every epoch near tip, every 10 epochs when
-- lagging, and always past @sicNearTipEpoch@ (default 580).
--
-- Returns a 'Bool' indicating whether a snapshot request was enqueued
-- (the snapshot-writer drains the queue asynchronously; this call
-- returns as soon as the request is in the queue).
applyBlockAndSnapshot
  :: LedgerEnv
  -> CardanoBlock StandardCrypto
  -> Bool                                           -- ^ @True@ if we're near tip
  -> IO (ApplyResult, Bool)
applyBlockAndSnapshot _env _blk _isNearTip =
  panic "DbSync.Ledger.State.applyBlockAndSnapshot: snapshot cadence wired when the SnapshotWriter lands"

{- |
Like consensus's @tickThenReapply@ but also verifies that the block's
@prevHash@ matches the current ledger tip hash. A mismatch is caught
here and reported with both hashes in the error text, which is the
single most common root-cause signal when rollback bookkeeping has
gone wrong.

The signature intentionally runs in 'IO' because the LSM backend's
@read@ \/ @pushDiffs@ operations are 'IO' — unlike the in-memory
backend where the same logic is pure 'Either'. Wiring up the
operations proper is deferred to the worker-thread commit where
block-ingest actually calls this function.
-}
tickThenReapplyCheckHash
  :: TopLevelConfig (CardanoBlock StandardCrypto)
  -> CardanoBlock StandardCrypto
  -> DbSyncStateRef
  -> IO (Either Text DbSyncStateRef)
tickThenReapplyCheckHash _cfg _blk _sref =
  panic "DbSync.Ledger.State.tickThenReapplyCheckHash: LSM read/pushDiffs wiring lands with the LedgerWorker"

-- ---------------------------------------------------------------------------
-- * Rollback
-- ---------------------------------------------------------------------------

{- |
Load the ledger state at a given 'CardanoPoint'. The memory-first
walk is implemented here; the disk-snapshot fallback is delivered by
the snapshot-manager integration.

Returns:

  * @'Right' sref@ — the point was found in the in-memory buffer, and
    the buffer has been trimmed to end at that ref.
  * @'Left' []@ — not in memory; caller should try the on-disk
    snapshot manager. The caller is also responsible for deleting any
    newer snapshots that fail the \"resume constraint\" check
    described in the ledger-state plan.

When the target point lives in the in-memory buffer we write the
trimmed 'LedgerDB' back into 'leStateVar' before returning; the ref
we return is the new tip. Callers don't need to push or prune.
-}
loadLedgerAtPoint
  :: LedgerEnv
  -> CardanoPoint
  -> IO (Either [DiskSnapshot] DbSyncStateRef)
loadLedgerAtPoint env point = do
  mLedger <- atomically $ readTVar (leStateVar env)
  case mLedger of
    Strict.Nothing ->
      -- No buffer yet (pre-boot or post-rollback). Caller falls back
      -- to the disk snapshot list.
      pure (Left [])
    Strict.Just ledger ->
      case rollbackBuffer point ledger of
        Just ledger' -> do
          writeLedgerState env (Strict.Just ledger')
          pure (Right (ledgerDbCurrent ledger'))
        Nothing ->
          pure (Left [])

-- | Walk the 'LedgerDB' newest-first, dropping refs whose tip is
-- newer than the rollback target. If the resulting buffer is
-- non-empty and its head tip is at or before the target slot, return
-- it; otherwise the point is too far back for the in-memory buffer.
rollbackBuffer :: CardanoPoint -> LedgerDB -> Maybe LedgerDB
rollbackBuffer point (LedgerDB s) =
  let trimmed = StrictSeq.dropWhileL isNewerThanTarget s
   in case trimmed of
        StrictSeq.Empty -> Nothing
        _               -> Just (LedgerDB trimmed)
  where
    targetSlot = Network.pointSlot point

    isNewerThanTarget :: DbSyncStateRef -> Bool
    isNewerThanTarget sref =
      Consensus.ledgerTipSlot (ledgerState (clsState (srState sref))) > targetSlot

-- ---------------------------------------------------------------------------
-- * Stake slice shim
-- ---------------------------------------------------------------------------

-- | Produce the per-block stake slice for the EpochBoundary path.
-- Byron / pre-Shelley states carry 'ByronEpochBlockNo' and yield
-- 'Generic.NoSlices'; everywhere else we hand the counter to
-- 'Generic.getStakeSlice' to read the \"mark\" snapshot.
getStakeSlice :: LedgerEnv -> CardanoLedgerState -> Bool -> Generic.StakeSliceRes
getStakeSlice env cls isMigration =
  case clsEpochBlockNo cls of
    ByronEpochBlockNo ->
      Generic.NoSlices
    EpochBlockNo n ->
      Generic.getStakeSlice
        (leProtocolInfo env)
        n
        (clsState cls)
        isMigration

-- ---------------------------------------------------------------------------
-- * Governance / ledger projections
-- ---------------------------------------------------------------------------

-- | Given a governance-action id and the current 'ConwayGovState',
-- compute the @Committee@ it would install — or 'Nothing' if the
-- action isn't a committee update (or isn't in the proposals map).
--
-- Walks the proposal tree up to the root action so that
-- chains of committee updates are applied in the correct order.
findProposedCommittee
  :: GovActionId
  -> ConwayGovState ConwayEra
  -> Either Text (Maybe (Committee ConwayEra))
findProposedCommittee gaId cgs = do
  (rootCommittee, updateList) <- findRoot gaId
  computeCommittee rootCommittee updateList
  where
    ps = cgsProposals cgs
    findRoot = findRootRecursively []

    findRootRecursively
      :: [GovAction ConwayEra]
      -> GovActionId
      -> Either Text (Ledger.StrictMaybe (Committee ConwayEra), [GovAction ConwayEra])
    findRootRecursively acc gid = do
      gas <- fromNothing ("findProposedCommittee: proposal " <> show gid <> " not found") $
              proposalsLookupId gid ps
      let ga = pProcGovAction (gasProposalProcedure gas)
      case ga of
        NoConfidence _ -> Right (Ledger.SNothing, acc)
        UpdateCommittee Ledger.SNothing _ _ _ ->
          Right (cgsCommittee cgs, ga : acc)
        UpdateCommittee gpid _ _ _
          | gpid == ps ^. pRootsL . grCommitteeL . prRootL ->
              Right (cgsCommittee cgs, ga : acc)
        UpdateCommittee (Ledger.SJust gpid) _ _ _ ->
          findRootRecursively (ga : acc) (unGovPurposeId gpid)
        _ ->
          Left "findProposedCommittee: non-committee gov action referenced by a committee action"

    computeCommittee
      :: Ledger.StrictMaybe (Committee ConwayEra)
      -> [GovAction ConwayEra]
      -> Either Text (Maybe (Committee ConwayEra))
    computeCommittee sCommittee actions =
      Ledger.strictMaybeToMaybe <$> foldM applyCommitteeUpdate sCommittee actions

    applyCommitteeUpdate
      :: Ledger.StrictMaybe (Committee ConwayEra)
      -> GovAction ConwayEra
      -> Either Text (Ledger.StrictMaybe (Committee ConwayEra))
    applyCommitteeUpdate scommittee = \case
      UpdateCommittee _ toRemove toAdd q ->
        Right . Ledger.SJust $
          updatedCommittee toRemove toAdd q scommittee
      _ ->
        Left "findProposedCommittee: unexpected gov action in committee update chain"

    fromNothing err = maybe (Left err) Right

-- | Governance-action-deposit lifetime, in epochs, as of this
-- ledger state. 'Strict.Nothing' for pre-Conway eras.
getGovExpiration :: CardanoLedgerState -> Strict.Maybe Ledger.EpochInterval
getGovExpiration st =
  case ledgerState $ clsState st of
    LedgerStateConway bls ->
      Strict.Just $
        Shelley.nesEs (Consensus.shelleyLedgerState bls)
          ^. (Shelley.curPParamsEpochStateL . Shelley.ppGovActionLifetimeL)
    _ -> Strict.Nothing

-- | Current Conway 'ConwayGovState'; 'Nothing' for pre-Conway eras.
getGovState :: ExtLedgerState (CardanoBlock StandardCrypto) mk -> Maybe (ConwayGovState ConwayEra)
getGovState ls = case ledgerState ls of
  LedgerStateConway cls ->
    Just $ Consensus.shelleyLedgerState cls ^. Shelley.newEpochStateGovStateL
  _ -> Nothing

-- | Current Plutus-execution 'Prices'. 'Strict.Nothing' for
-- pre-Alonzo eras that don't have script execution.
--
-- Dispatched per-era rather than through a polymorphic helper because
-- 'Shelley.curPParamsEpochStateL' insists on the full 'Shelley.EraGov'
-- constraint, which Alonzo\/Babbage\/Conway all satisfy for different
-- reasons and a single constraint wouldn't line up across all three.
getPrices :: CardanoLedgerState -> Strict.Maybe Prices
getPrices st = case ledgerState $ clsState st of
  LedgerStateAlonzo als ->
    Strict.Just
      ( Shelley.nesEs (Consensus.shelleyLedgerState als)
          ^. Shelley.curPParamsEpochStateL
           . Alonzo.ppPricesL
      )
  LedgerStateBabbage bls ->
    Strict.Just
      ( Shelley.nesEs (Consensus.shelleyLedgerState bls)
          ^. Shelley.curPParamsEpochStateL
           . Alonzo.ppPricesL
      )
  LedgerStateConway cls ->
    Strict.Just
      ( Shelley.nesEs (Consensus.shelleyLedgerState cls)
          ^. Shelley.curPParamsEpochStateL
           . Alonzo.ppPricesL
      )
  _ -> Strict.Nothing

-- | Every currently-registered pool, as a set of pool key hashes.
-- Byron returns the empty set (no pools pre-Shelley).
getRegisteredPools :: CardanoLedgerState -> Set PoolKeyHash
getRegisteredPools st =
  case ledgerState (clsState st) of
    LedgerStateByron _      -> Set.empty
    LedgerStateShelley sls  -> getRegisteredPoolShelley sls
    LedgerStateAllegra als  -> getRegisteredPoolShelley als
    LedgerStateMary mls     -> getRegisteredPoolShelley mls
    LedgerStateAlonzo als   -> getRegisteredPoolShelley als
    LedgerStateBabbage bls  -> getRegisteredPoolShelley bls
    LedgerStateConway cls   -> getRegisteredPoolShelley cls
    LedgerStateDijkstra dls -> getRegisteredPoolShelley dls

getRegisteredPoolShelley
  :: Shelley.EraCertState era
  => Consensus.LedgerState (ShelleyBlock p era) mk
  -> Set PoolKeyHash
getRegisteredPoolShelley lState =
  Map.keysSet $
    let certState =
          Shelley.lsCertState $
            Shelley.esLState $
              Shelley.nesEs $
                Consensus.shelleyLedgerState lState
     in certState ^. Shelley.certPStateL . Shelley.psStakePoolsL

-- ---------------------------------------------------------------------------
-- * Miscellaneous helpers
-- ---------------------------------------------------------------------------

-- | Serialise a Cardano header hash to its 32-byte raw form.
-- Delegates to the consensus 'OneEraHash' encoding used everywhere
-- else in the pipeline.
getHeaderHash :: Network.HeaderHash (CardanoBlock StandardCrypto) -> ByteString
getHeaderHash = SBS.fromShort . Consensus.getOneEraHash

-- | Pull out the first 'LedgerAdaPots' event seen in a stream.
-- Returns 'Nothing' when the stream is pots-free (any non-epoch
-- boundary, or pre-Shelley).
findAdaPots :: [LedgerEvent] -> Maybe AdaPots
findAdaPots = go
  where
    go []                       = Nothing
    go (LedgerAdaPots p : _)    = Just p
    go (_               : rest) = go rest
