{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
  , pruneStrictSeq
  , ledgerDbCheckpointBufferSize
  , ledgerDbCurrent
  , writeLedgerState
  , readCurrentStateUnsafe

    -- * Environment construction
  , mkHasLedgerEnv
  , initLedgerDbFromGenesis
  , initLedgerDbFromSnapshot
  , dropLedgerStateDir

    -- * Block application
  , applyBlock
  , applyBlockAndSnapshot
  , tickThenReapplyCheckHash
  , applyToEpochBlockNo
  , ledgerEpochNo
  , shouldSnapshotAtEpoch

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
  , getTopLevelConfig
  ) where

import Cardano.Prelude hiding (atomically)

import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Cardano.Ledger.Alonzo.Scripts (Prices)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core as Shelley
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Shelley.AdaPots (AdaPots (..), sumAdaPots)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Slotting.EpochInfo (EpochInfo, epochInfoEpoch)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict
  ( atomically
  , newEmptyTMVarIO
  , newTVarIO
  , readTVar
  , writeTVar
  )
import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
import Control.Concurrent.STM.TBQueue (newTBQueueIO)
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Time.Clock as Time
import GHC.IO.Exception (userError)
import Lens.Micro ((%~), (^.), (^?))
import Ouroboros.Consensus.Block (blockHash, blockIsEBB, blockPrevHash, blockSlot)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import Ouroboros.Consensus.Cardano.Block (ConwayEra, LedgerState (..), StandardCrypto)
import Ouroboros.Consensus.Config (TopLevelConfig, configCodec, configLedger)
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import Ouroboros.Consensus.HardFork.Combinator.State (epochInfoLedger)
import Ouroboros.Consensus.Ledger.Abstract (LedgerResult)
import qualified Ouroboros.Consensus.Ledger.Abstract as Consensus
import Ouroboros.Consensus.Ledger.Basics (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot)
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend hiding (Trace)
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq as Consensus
  ( LedgerTablesHandle (..)
  )

import qualified Ouroboros.Network.Block as Network

import Control.ResourceRegistry (runWithTempRegistry)
import qualified Control.Tracer as Tracer
import System.FS.API (SomeHasFS (..), mkFsPath)
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removePathForcibly)
import System.FilePath ((</>))
import System.Random (genWord64, newStdGen)

import DbSync.AppM (LedgerM, runAppM)
import DbSync.Checkpoint.SyncState (ControlConnection)
import DbSync.Config.Types (LedgerBackend (..))
import DbSync.Db.Types (DbLovelace (..))
import qualified DbSync.Era.Shelley.Generic.EpochUpdate as Generic
import qualified DbSync.Era.Shelley.Generic.ProtoParams as Generic
import qualified DbSync.Era.Shelley.Generic.StakeDist as Generic
import DbSync.Ledger.DepositAccumulator
  ( EpochParams (..)
  , newEpochParamsRef
  , recordEpochParams
  )
import DbSync.Ledger.Event
  ( LedgerEvent (..)
  , convertAuxLedgerEvent
  , splitDeposits
  )
import DbSync.Ledger.Keys (PoolKeyHash)
import DbSync.Ledger.Types
  ( ApplyResult (..)
  , CardanoLedgerState (..)
  , ConsensusStateRef
  , DbSyncStateRef (..)
  , DepositsMap (..)
  , EpochBlockNo (..)
  , HasLedgerEnv (..)
  , LedgerDB (..)
  , LedgerEnv (..)
  , initCardanoLedgerState
  , newEpochStateT
  , updatedCommittee
  )
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Shelley.HFEras ()                -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()  -- 'LedgerSupportsProtocol' orphans

import qualified DbSync.Ledger.Snapshot
import DbSync.Ledger.Snapshot (loadSnapshotFromDisk)
import DbSync.Block.Types (CardanoPoint)
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Trace.Types (AppTracer)
import DbSync.Util (maybeToStrictMaybe)

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
-- 'LedgerDB', then prune any entries that fall outside the
-- 'ledgerDbCheckpointBufferSize' window.
--
-- Returns the new 'LedgerDB' along with any pruned refs whose
-- 'LedgerTablesHandle' the caller is responsible for closing
-- (subject to invariant I3 — the snapshot writer must release
-- 'srCanClose' before the close is permitted).
pushLedgerDB :: LedgerDB -> DbSyncStateRef -> (LedgerDB, [DbSyncStateRef])
pushLedgerDB db sref =
  pruneLedgerDb ledgerDbCheckpointBufferSize $
    LedgerDB (sref StrictSeq.<| ledgerDbCheckpoints db)

-- | Split the buffer at @k@ entries, keeping the @k@ newest and
-- returning the older ones for the caller to close.
pruneLedgerDb :: Int -> LedgerDB -> (LedgerDB, [DbSyncStateRef])
pruneLedgerDb k (LedgerDB s) =
  let (kept, dropped) = pruneStrictSeq k s
   in (LedgerDB kept, dropped)
{-# INLINE pruneLedgerDb #-}

-- | Polymorphic spine-only logic underlying 'pruneLedgerDb'. Split
-- a 'StrictSeq' at index @k@; return the @k@ newest along with the
-- older ones as a plain list.
--
-- Exported (above 'pruneLedgerDb') so tests can exercise the
-- shape-only behaviour against simple element types — constructing
-- a 'DbSyncStateRef' just to test sequence slicing would require an
-- LSM session (Phase 6 fixture territory).
pruneStrictSeq :: Int -> StrictSeq.StrictSeq a -> (StrictSeq.StrictSeq a, [a])
pruneStrictSeq k s =
  let (kept, dropped) = StrictSeq.splitAt k s
   in (kept, toList dropped)
{-# INLINE pruneStrictSeq #-}

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
writeLedgerState :: Strict.Maybe LedgerDB -> LedgerM ()
writeLedgerState mDb = do
  env <- ask
  liftIO $ atomically $ writeTVar (leStateVar env) mDb

-- | Read the newest 'ExtLedgerState' out of the buffer. Throws via
-- STM if the buffer hasn't been initialised yet (pre-boot); callers
-- downstream of 'mkHasLedgerEnv' + genesis init should never see
-- that, so we treat it as a programmer error.
readCurrentStateUnsafe
  :: LedgerM (ExtLedgerState (CardanoBlock StandardCrypto) EmptyMK)
readCurrentStateUnsafe = do
  env <- ask
  liftIO $ atomically (clsState . srState . ledgerDbCurrent <$> readStateUnsafe env)

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

-- | Construct a 'HasLedgerEnv' in the 'LedgerEnabled' arm: opens an
-- LSM session under the configured state directory, builds the
-- consensus 'SnapshotManager', wires up the genesis-init and
-- snapshot-load callbacks, and allocates all the in-process
-- coordination primitives.
--
-- Per decision D1 (LSM only) the 'LedgerBackend' is always
-- 'LedgerBackendLSM'; the in-memory branch was rejected at config
-- parse time. We still take the backend value as input so a future
-- knob (\"use a different LSM directory\") can flow through.
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
  -> ControlConnection                              -- ^ For 'markSnapshotComplete' from the writer thread
  -> IO HasLedgerEnv
mkHasLedgerEnv
  tracer pinfo dir network maxSupply start snapEpoch
  hasRewards abortOnPanic backend ctrlConn = do
    interpreterVar  <- newTVarIO Strict.Nothing
    stateVar        <- newTVarIO Strict.Nothing
    latestApplyVar  <- newTVarIO Strict.Nothing
    ledgerQueue     <- newTBQueueIO ledgerQueueBound
    depositAccumRef <- newEpochParamsRef
    epochReady      <- newEmptyTMVarIO
    epochWait       <- newEmptyTMVarIO
    snapshotQueue   <- newTBQueueIO snapshotQueueBound
    consistentVar   <- newTVarIO False

    -- One snapshot, two directories — both halves required, neither a duplicate:
    --   <dir>/snapshot-headers/<slot>/  small (KB–MB): ExtLedgerState
    --     (era, governance, stake dist, params, tip) + utxoSize + checksum.
    --     The entry door on resume; without it we'd replay from genesis.
    --   <dir>/lsm/snapshots/<slot>/     bulk (multi-GB): UTxO tables only.
    -- Can't merge: 'LSM.saveSnapshot' rejects pre-existing dirs and the
    -- matching load path is upstream's V2 LSM 'implTakeSnapshot'.
    let snapshotsDir = dir </> "snapshot-headers"
    createDirectoryIfMissing True snapshotsDir

    let codecConfig = configCodec (Consensus.pInfoConfig pinfo)
        someHasFS   = SomeHasFS (ioHasFS (MountPoint snapshotsDir))
        snapTracer  = Tracer.nullTracer
        lsmPath     = case backend of
                        LedgerBackendLSM (Just p) -> p
                        LedgerBackendLSM Nothing  -> dir </> "lsm"

    salt <- fst . genWord64 <$> newStdGen
    -- The HasBlockIO is rooted at lsmPath, so the session's FsPath
    -- inside it must be the empty path (matches upstream — using
    -- the full lsmPath here puts the session at <lsmPath>/<lsmPath>
    -- and breaks snapshot bundling).
    let lsmArgs = LSM.LSMArgs (mkFsPath []) salt (LSM.stdMkBlockIOFS lsmPath)

    res <-
      runWithTempRegistry $
        (,())
          <$> mkResources
                (Proxy @(CardanoBlock StandardCrypto))
                Tracer.nullTracer
                lsmArgs
                someHasFS

    let snapMgr =
          snapshotManager
            (Proxy @(CardanoBlock StandardCrypto))
            res
            codecConfig
            snapTracer
            someHasFS

        initGenesis :: IO ConsensusStateRef
        initGenesis =
          createAndPopulateStateRefFromGenesis
            Tracer.nullTracer
            res
            (Consensus.pInfoInitLedger pinfo)

        loadSnap :: DiskSnapshot -> IO (Either Text ConsensusStateRef)
        loadSnap ds = do
          eResult <-
            runExceptT $
              openStateRefFromSnapshot
                Tracer.nullTracer
                codecConfig
                someHasFS
                res
                ds
          case eResult of
            Left err          -> pure (Left (show err))
            Right (cRef, _pt) -> pure (Right cRef)

        closeBackend :: IO ()
        closeBackend =
          releaseResources (Proxy @(CardanoBlock StandardCrypto)) res

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
          , leSnapshotManager      = snapMgr
          , leInitGenesis          = initGenesis
          , leLoadSnapshot         = loadSnap
          , leClose                = closeBackend
          , leLatestApplyResult    = latestApplyVar
          , leDepositAccumulator   = depositAccumRef
          , leControlConnection    = ctrlConn
          , leConsistentWithTip    = consistentVar
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

-- | Seed the in-memory 'LedgerDB' buffer with the genesis state.
--
-- Call this once at boot, on a fresh database, after 'mkHasLedgerEnv'
-- has constructed the 'LedgerEnv'. Without it the buffer stays empty
-- and the first 'applyBlock' crashes in 'readStateUnsafe' with
-- @\"LedgerDB not initialised\"@.
--
-- For a resume from an existing populated database the buffer should
-- be seeded from a matching disk snapshot instead; that path is not
-- implemented yet, so resuming a ledger-enabled database without
-- @--resync-from-genesis@ is currently unsupported.
initLedgerDbFromGenesis :: LedgerEnv -> IO ()
initLedgerDbFromGenesis env = do
  sref <- initCardanoLedgerState env
  atomically $ writeTVar (leStateVar env)
    (Strict.Just (LedgerDB (StrictSeq.singleton sref)))

-- | Restore the in-memory 'LedgerDB' from an on-disk snapshot.
-- Returns 'Left' with the backend's error text on failure; the
-- caller decides how to escalate.
initLedgerDbFromSnapshot :: LedgerEnv -> DiskSnapshot -> IO (Either Text ())
initLedgerDbFromSnapshot env snap = do
  eRef <- runAppM env (loadSnapshotFromDisk snap)
  case eRef of
    Left err -> pure (Left err)
    Right sref -> do
      atomically $ writeTVar (leStateVar env)
        (Strict.Just (LedgerDB (StrictSeq.singleton sref)))
      pure (Right ())

-- | Recursively wipe the ledger state directory (LSM session +
-- snapshot headers). Companion to @dropSchema@: invoked when
-- @--resync-from-genesis@ is in effect so the next boot starts from
-- genesis with a clean slate.
--
-- A no-op when the directory doesn't exist.
dropLedgerStateDir :: FilePath -> IO ()
dropLedgerStateDir dir = do
  exists <- doesDirectoryExist dir
  when exists $ removePathForcibly dir

-- ---------------------------------------------------------------------------
-- * Block application
-- ---------------------------------------------------------------------------

-- | Tick the chain to the block's slot, reapply the block against the
-- backing LSM tables, and produce a fresh 'DbSyncStateRef' that takes
-- the place of the previous tip in the 'LedgerDB' buffer.
--
-- Verifies that the block's 'blockPrevHash' matches the current
-- ledger tip hash before doing any work — a hash mismatch is the
-- canonical signal that rollback bookkeeping has gone wrong, and the
-- error text includes both hashes for diagnosis.
--
-- Runs in 'LedgerM' (atop 'IO') because the LSM handle's @read@ /
-- @duplicateWithDiffs@ operations require 'IO'.
tickThenReapplyCheckHash
  :: ExtLedgerCfg (CardanoBlock StandardCrypto)
  -> CardanoBlock StandardCrypto
  -> LedgerM
       (Either Text
          ( DbSyncStateRef
          , LedgerResult (ExtLedgerState (CardanoBlock StandardCrypto)) CardanoLedgerState
          , [DbSyncStateRef]
          ))
tickThenReapplyCheckHash cfg block = do
  env <- ask
  liftIO $ do
    -- Snapshot the current LedgerDB + tip atomically.
    (ledgerDB, oldRef) <- atomically $ do
      !db <- readStateUnsafe env
      pure (db, ledgerDbCurrent db)
    let !oldCls = srState oldRef
        oldExt  = clsState oldCls
    if blockPrevHash block == Consensus.ledgerTipHash (ledgerState oldExt)
      then do
        -- Read the keys this block touches from the backing LSM tables.
        let keys = Consensus.getBlockKeySets block
        restrictedTables <- Consensus.read (srTables oldRef) oldExt keys
        let -- Attach the just-read values to the in-memory state, then tick + reapply.
            ledgerStateWithTables = Consensus.withLedgerTables oldExt restrictedTables
            newLedgerResult =
              Consensus.tickThenReapplyLedgerResult
                Consensus.ComputeLedgerEvents
                cfg
                block
                ledgerStateWithTables
            newLedgerStateEmpty = forgetLedgerTables (Consensus.lrResult newLedgerResult)
            isNewEpoch =
              case ( ledgerEpochNo env oldExt
                   , ledgerEpochNo env newLedgerStateEmpty
                   ) of
                (Right oldE, Right newE) -> oldE /= newE
                _                        -> False
            isByron = case ledgerState newLedgerStateEmpty of
                        LedgerStateByron _ -> True
                        _                  -> False
            !newEpochBlockNo =
              applyToEpochBlockNo isByron isNewEpoch (clsEpochBlockNo oldCls)
            newCls =
              fmap
                (\stt ->
                   CardanoLedgerState
                     { clsState        = forgetLedgerTables stt
                     , clsEpochBlockNo = newEpochBlockNo
                     })
                newLedgerResult
        -- Clone the LSM handle and apply the block-level diffs onto the clone.
        newHandle <-
          Consensus.duplicateWithDiffs
            (srTables oldRef)
            oldExt
            (Consensus.lrResult newLedgerResult)
        canClose <- newTVarIO True
        let !newRef =
              DbSyncStateRef
                { srState    = Consensus.lrResult newCls
                , srTables   = newHandle
                , srCanClose = canClose
                }
            (!ledgerDB', !pruned) = pushLedgerDB ledgerDB newRef
        atomically $ writeTVar (leStateVar env) (Strict.Just ledgerDB')
        pure $ Right (oldRef, newCls, pruned)
      else
        pure $ Left $
          mconcat
            [ "Ledger state hash mismatch. Ledger head is slot "
            , show (Consensus.ledgerTipSlot (ledgerState oldExt))
            , "; block previous hash is "
            , show (blockPrevHash block)
            , "; block hash is "
            , show (blockHash block)
            , "."
            ]

-- | Apply a single block to the current ledger state, returning the
-- /old/ state ref (for snapshot bookkeeping), the 'ApplyResult'
-- carrying every derived value the downstream extractors need, and
-- the list of pruned refs whose LSM handles must subsequently be
-- closed.
--
-- 'SlotDetails' is supplied by the caller (the worker computes it
-- via 'DbSync.StateQuery.getSlotDetails' before invoking this
-- function) — block application itself does not query the slot
-- machinery.
applyBlock
  :: CardanoBlock StandardCrypto
  -> SlotDetails
  -> LedgerM (DbSyncStateRef, ApplyResult, [DbSyncStateRef])
applyBlock blk slotDetails = do
  env <- ask
  result <- tickThenReapplyCheckHash (ExtLedgerCfg (getTopLevelConfig env)) blk
  case result of
    Left err -> panic err
    Right (oldRef, newResult, pruned) -> do
      let !oldCls = srState oldRef
          eventsFull =
            mapMaybe
              (convertAuxLedgerEvent (leHasRewards env))
              (Consensus.lrEvents newResult)
          (!events, !deposits) = splitDeposits eventsFull
          !rawNewState         = clsState (Consensus.lrResult newResult)
      newEpoch <-
        case mkOnNewEpoch env blk (clsState oldCls) rawNewState (findAdaPots events) of
          Left e   -> panic e
          Right ne -> pure ne
      let !finalState =
            case newEpoch of
              Just _  -> finaliseDrepDistr rawNewState
              Nothing -> rawNewState
          !newCls' =
            (Consensus.lrResult newResult)
              { clsState = finalState }
          appResult =
            ApplyResult
              { apPrices          = getPrices newCls'
              , apGovExpiresAfter = getGovExpiration newCls'
              , apPoolsRegistered = getRegisteredPools oldCls
              , apNewEpoch        = maybeToStrictMaybe newEpoch
              , apDeposits        = maybeToStrictMaybe (Generic.getDeposits finalState)
              , apSlotDetails     = slotDetails
              , apStakeSlice      = getStakeSlice env newCls' False
              , apEvents          = events
              , apGovActionState  = getGovState finalState
              , apDepositsMap     = DepositsMap deposits
              }
      liftIO $ atomically $
        writeTVar (leLatestApplyResult env) (Strict.Just appResult)
      pure (oldRef, appResult, pruned)

-- | 'applyBlock' plus the snapshot-cadence decision and pruning of
-- old-ref handles. Returns the 'ApplyResult' and whether a snapshot
-- write was enqueued (drained asynchronously by the snapshot writer).
--
-- Pruned refs are closed only after their 'srCanClose' flag clears,
-- so an in-flight snapshot write can't lose its handle (I3 in the
-- ledger-state plan).
--
-- The optional @replayBoundary@ suppresses snapshot writes inside
-- the @[snapshotSlot+1, last_committed_slot]@ resume catch-up
-- window; the consensus V2 backend would reject those attempts as
-- redundant anyway (its tip overlaps the just-loaded snapshot),
-- producing a confusing @takeSnapshot returned Nothing@ trace.
applyBlockAndSnapshot
  :: CardanoBlock StandardCrypto
  -> SlotDetails
  -> Bool                                           -- ^ \"consistent with chain tip\"
  -> Maybe SlotNo                                   -- ^ replay boundary
  -> LedgerM (ApplyResult, Bool)
applyBlockAndSnapshot blk slotDetails consistent mReplayBoundary = do
  env <- ask
  (oldRef, appResult, pruned) <- applyBlock blk slotDetails
  let nearTip        = isSyncedNearTip slotDetails
      inReplayWindow = maybe False (blockSlot blk <=) mReplayBoundary
  -- Record this block's epoch params in the in-memory accumulator
  -- so the consumer can flush them at the next epoch boundary.
  -- Skipped inside the replay window: those epochs are already in
  -- @epoch_param_pending@ from the previous run.
  unless inReplayWindow $
    liftIO $ accumulateEpochParams env appResult
  tookSnapshot <-
    if not inReplayWindow
       && shouldSnapshotAtEpoch appResult consistent nearTip (leSnapshotNearTipEpoch env)
      then do
        DbSync.Ledger.Snapshot.saveCleanupState oldRef
        pure True
      else pure False
  liftIO $ forM_ pruned $ \sr -> do
    atomically $ readTVar (srCanClose sr) >>= STM.check
    Consensus.close (srTables sr)
  pure (appResult, tookSnapshot)

-- | Project the 'ApplyResult'\'s deposit data into the per-epoch
-- accumulator. Byron blocks (no @apDeposits@) and pre-Shelley
-- blocks are skipped — there are no protocol-param deposits to
-- record. Multiple writes for the same epoch are idempotent
-- because protocol params are constant within an epoch.
accumulateEpochParams :: LedgerEnv -> ApplyResult -> IO ()
accumulateEpochParams env result =
  case apDeposits result of
    Strict.Nothing -> pure ()
    Strict.Just d  -> do
      let !ep = EpochParams
            { epStakeKeyDeposit = DbLovelace (fromIntegral (unCoin (Generic.stakeKeyDeposit d)))
            , epPoolDeposit     = DbLovelace (fromIntegral (unCoin (Generic.poolDeposit d)))
            }
      recordEpochParams
        (leDepositAccumulator env)
        (sdEpochNo (apSlotDetails result))
        ep

-- ---------------------------------------------------------------------------
-- * Helpers used by block application
-- ---------------------------------------------------------------------------

-- | Bump the per-epoch block counter following a block application.
--
-- @applyToEpochBlockNo isByron isNewEpoch oldCounter@:
--
-- * Byron eras always read 'ByronEpochBlockNo' (we don't track
--   stake-slice indices pre-Shelley).
-- * A new-epoch boundary resets the counter to @0@.
-- * Non-boundary blocks advance the counter by one (or seed it at
--   @0@ if we just transitioned out of Byron).
applyToEpochBlockNo :: Bool -> Bool -> EpochBlockNo -> EpochBlockNo
applyToEpochBlockNo True  _    _              = ByronEpochBlockNo
applyToEpochBlockNo _     True _              = EpochBlockNo 0
applyToEpochBlockNo _     _    (EpochBlockNo n) = EpochBlockNo (n + 1)
applyToEpochBlockNo _     _    ByronEpochBlockNo = EpochBlockNo 0

-- | Project the current 'EpochNo' from a ledger state via the HFC
-- interpreter built from the ledger's hard-fork summary. Returns
-- @'Right' 'Nothing'@ at the genesis tip and @'Left' err@ if the
-- requested slot falls outside the summary's horizon.
ledgerEpochNo
  :: LedgerEnv
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> Either Text (Maybe EpochNo)
ledgerEpochNo env st =
  case Consensus.ledgerTipSlot (ledgerState st) of
    Origin -> Right Nothing
    At sl  ->
      case runExcept (epochInfoEpoch epochInfo sl) of
        Left err -> Left $ "ledgerEpochNo: " <> show err
        Right en -> Right (Just en)
  where
    epochInfo :: EpochInfo (Except Consensus.PastHorizonException)
    epochInfo =
      epochInfoLedger
        (configLedger (getTopLevelConfig env))
        (Consensus.hardForkLedgerStatePerEra (ledgerState st))

-- | Pure decision: should we save a snapshot at this epoch boundary?
--
-- Mirrors upstream's cadence:
--
--   * Only fires on epoch boundaries (when @apNewEpoch@ is 'Just').
--   * Never fires at epoch @0@ (the boot epoch — there's nothing to
--     snapshot yet).
--   * Otherwise: every epoch when consistent + near tip; every 10
--     epochs when lagging; every epoch unconditionally past the
--     near-tip-epoch threshold.
shouldSnapshotAtEpoch
  :: ApplyResult
  -> Bool         -- ^ consistent with chain tip
  -> Bool         -- ^ near tip (e.g. within ~10 days of head)
  -> Word64       -- ^ near-tip-epoch threshold (e.g. 580)
  -> Bool
shouldSnapshotAtEpoch result consistent nearTip thresholdEpoch =
  case apNewEpoch result of
    Strict.Nothing -> False
    Strict.Just ne ->
      let n = unEpochNo (Generic.neEpoch ne)
       in n > 0
            && ( (consistent && nearTip)
                 || n `mod` 10 == 0
                 || n >= thresholdEpoch
               )

-- | Approximate "is the chain tip near the current wall-clock time?"
-- — used as the @near tip@ flag for the snapshot cadence decision.
-- 60-second window; matches upstream's heuristic and is generous
-- enough to absorb consumer-side latency.
isSyncedNearTip :: SlotDetails -> Bool
isSyncedNearTip sd =
  let secsBehind =
        ceiling
          (realToFrac
             (diffUTCTime' (sdCurrentTime sd) (sdSlotTime sd))
             :: Double) :: Int
   in abs secsBehind <= 60
  where
    diffUTCTime' a b = a `Time.diffUTCTime` b

-- | Detect epoch boundary and build a 'Generic.NewEpoch' summary.
mkOnNewEpoch
  :: LedgerEnv
  -> CardanoBlock StandardCrypto
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk1
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk2
  -> Maybe AdaPots
  -> Either Text (Maybe Generic.NewEpoch)
mkOnNewEpoch env blk oldState newState mPots =
  case (ledgerEpochNo env oldState, ledgerEpochNo env newState) of
    (Left e, _)      -> Left e
    (_, Left e)      -> Left e
    (Right Nothing, Right (Just (EpochNo 0))) ->
      Right (Just (mkNewEpoch (EpochNo 0)))
    (Right (Just prev), Right (Just curr))
      | unEpochNo curr == 1 + unEpochNo prev ->
          Right (Just (mkNewEpoch curr))
    _ -> Right Nothing
  where
    mkNewEpoch :: EpochNo -> Generic.NewEpoch
    mkNewEpoch curr =
      Generic.NewEpoch
        { Generic.neEpoch       = curr
        , Generic.neIsEBB       = isJust (blockIsEBB blk)
        , Generic.neAdaPots     = fixUTxOPots <$> maybeToStrictMaybe mPots
        , Generic.neEpochUpdate = Generic.epochUpdate newState
        , Generic.neDRepState   = maybeToStrictMaybe (getDrepState newState)
        , Generic.neEnacted     = maybeToStrictMaybe (getGovState newState)
        , Generic.nePoolDistr   = maybeToStrictMaybe (Generic.getPoolDistr newState)
        }

    fixUTxOPots :: AdaPots -> AdaPots
    fixUTxOPots adaPots =
      adaPots
        { utxoAdaPot =
            Coin $
              fromIntegral (leMaxSupply env) - unCoin (sumAdaPots adaPots)
        }

-- | Pull the Conway-era DRep pulsing state out of a ledger state, if
-- any. 'Nothing' for pre-Conway eras.
getDrepState
  :: ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> Maybe (DRepPulsingState ConwayEra)
getDrepState ls =
  ls ^? newEpochStateT . newEpochStateDRepPulsingStateL

-- | Force the Conway DRep pulsing state to its non-pulsing
-- representative. Called only at the epoch boundary, where the
-- pulser is supposed to have completed.
finaliseDrepDistr
  :: ExtLedgerState (CardanoBlock StandardCrypto) mk
  -> ExtLedgerState (CardanoBlock StandardCrypto) mk
finaliseDrepDistr ledger =
  ledger & newEpochStateT %~ forceDRepPulsingState @ConwayEra

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
  :: CardanoPoint
  -> LedgerM (Either [DiskSnapshot] DbSyncStateRef)
loadLedgerAtPoint point = do
  env <- ask
  mLedger <- liftIO $ atomically $ readTVar (leStateVar env)
  case mLedger of
    Strict.Nothing ->
      -- No buffer yet (pre-boot or post-rollback). Caller falls back
      -- to the disk snapshot list.
      pure (Left [])
    Strict.Just ledger ->
      case rollbackBuffer point ledger of
        Just ledger' -> do
          writeLedgerState (Strict.Just ledger')
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

-- | The 'TopLevelConfig' embedded in the 'LedgerEnv'\'s
-- 'ProtocolInfo'. Exposed so the worker can build an 'ExtLedgerCfg'
-- to pass to 'tickThenReapplyCheckHash'.
getTopLevelConfig :: LedgerEnv -> TopLevelConfig (CardanoBlock StandardCrypto)
getTopLevelConfig = Consensus.pInfoConfig . leProtocolInfo

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
