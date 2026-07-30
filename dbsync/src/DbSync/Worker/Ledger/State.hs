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

-- | Core ledger-state operations: the 'LedgerDB' buffer, block
-- application, and rollback. Sits between the consensus LedgerDB V2
-- machinery and the sync engine. Owns:
--
--   * The in-memory 'LedgerDB' checkpoint buffer (push, prune, current
--     tip, atomic read\/write against 'leStateVar').
--   * 'applyBlock' \/ 'applyBlockAndSnapshot' — tick the chain to the
--     block's slot, reapply against the LSM tables, and return an
--     'ApplyResult' for the extractors.
--   * 'loadLedgerAtPoint' — rollback: walk the in-memory buffer first,
--     then fall back to a disk-snapshot load when the target predates
--     the buffer.
--
-- Small ledger projections (@getPrices@, @findAdaPots@,
-- @findProposedCommittee@, @getStakeSlice@) live here, each pulling one
-- value out of a 'CardanoLedgerState' or event stream.
module DbSync.Worker.Ledger.State
  ( -- * LedgerDB management
    pushLedgerDB
  , pruneLedgerDb
  , pruneStrictSeq
  , ledgerDbCheckpointBufferSize
  , ingestLedgerDbCheckpointBufferSize
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

    -- * Block-application switches
  , BoundaryStatus (..)
  , TipProximity (..)
  , ledgerStateEra

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

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Cardano.Ledger.Alonzo.Scripts (Prices)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core as Shelley
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.AdaPots (AdaPots (..), sumAdaPots)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.TxIn (TxId (..))
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
import Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import System.Mem.StableName (eqStableName, makeStableName)
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
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
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

import DbSync.AppM (LedgerM)
import DbSync.SyncState.Row (ControlConnection)
import DbSync.App.Config.Types (LedgerBackend (..))
import DbSync.Db.Types (DbLovelace (..))
import qualified DbSync.Worker.Ledger.EpochUpdate as Generic
import qualified DbSync.Worker.Ledger.ProtoParams as Generic
import qualified DbSync.Worker.Ledger.StakeDist as Generic
import DbSync.Worker.Ledger.DepositAccumulator
  ( EpochParams (..)
  , newEpochParamsRef
  , recordEpochParams
  )
import DbSync.Worker.Ledger.Event
  ( LedgerEvent (..)
  , RewardsCapture
  , convertAuxLedgerEvent
  , splitDeposits
  )
import DbSync.Worker.Ledger.Types
  ( ApplyResult (..)
  , BlockApplyData (..)
  , BoundaryApplyData (..)
  , ProposedCommitteeMember (..)
  , CardanoLedgerState (..)
  , ConsensusStateRef
  , DbSyncStateRef (..)
  , DepositsMap (..)
  , EpochBlockNo (..)
  , HasLedgerEnv (..)
  , LedgerDB (..)
  , LedgerEnv (..)
  , PanicPolicy
  , RegisteredPoolsCache (..)
  , initCardanoLedgerState
  , newEpochStateT
  , updatedCommittee
  )
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Shelley.HFEras ()                -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()  -- 'LedgerSupportsProtocol' orphans

import qualified DbSync.Worker.Ledger.Snapshot
import DbSync.Worker.Ledger.Snapshot (loadSnapshotFromDisk)
import DbSync.Parser.Types
  ( BlockEra (..)
  , CardanoPoint
  , EraStakeModel (..)
  , classifyEra
  )
import DbSync.StateQuery (SlotDetails (..))
import DbSync.Phase.Current (CurrentPhase, readCurrentPhase)
import DbSync.Phase.Type (SyncPhase, isFollowPath)
import DbSync.Trace.Types (AppTracer)
import DbSync.Util (maybeToStrictMaybe)

-- ---------------------------------------------------------------------------
-- * LedgerDB management
-- ---------------------------------------------------------------------------

-- | Hard cap on how many recent 'DbSyncStateRef' values the in-memory
-- buffer retains once a Follow path is live. Matching this against
-- @k=2160@ would be ideal, but keeping 2160 full state references in
-- RAM is not cheap; 100 gives fast rollback within a tenth of a
-- security-parameter window and forces deeper rollbacks through the
-- disk-snapshot path.
ledgerDbCheckpointBufferSize :: Int
ledgerDbCheckpointBufferSize = 100

-- | Buffer cap during 'IngestChainHistory'. Rollbacks are impossible
-- below @nodeTip − k@ (the consumer panics on one), so the buffer is
-- pure overhead during ingest — and each retained ref pins an open
-- LSM tables handle. Two slots rather than one: while a snapshot
-- write holds the oldest generation open (via @srCanClose@), the
-- worker can still advance into the spare slot instead of blocking
-- for the whole write; the extra generation is only held while a
-- snapshot writes. The buffer grows to
-- 'ledgerDbCheckpointBufferSize' naturally within ~100 blocks of
-- the Follow handoff.
ingestLedgerDbCheckpointBufferSize :: Int
ingestLedgerDbCheckpointBufferSize = 2

-- | Push a new 'DbSyncStateRef' onto the newest end of the
-- 'LedgerDB', then prune any entries that fall outside the supplied
-- cap.
--
-- Returns the new 'LedgerDB' along with any pruned refs whose
-- 'LedgerTablesHandle' the caller is responsible for closing
-- (subject to invariant I3 — the snapshot writer must release
-- 'srCanClose' before the close is permitted).
pushLedgerDB :: Int -> LedgerDB -> DbSyncStateRef -> (LedgerDB, [DbSyncStateRef])
pushLedgerDB cap db sref =
  pruneLedgerDb cap $
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
    Strict.Nothing -> throwSTM $ userError "DbSync.Worker.Ledger.State.readStateUnsafe: LedgerDB not initialised"
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
  -> RewardsCapture
  -> PanicPolicy
  -> LedgerBackend
  -> ControlConnection                              -- ^ For 'markSnapshotComplete' from the writer thread
  -> CurrentPhase                                   -- ^ Shared lifecycle phase, read by the worker for snapshot cadence
  -> IO HasLedgerEnv
mkHasLedgerEnv
  tracer pinfo dir network maxSupply start snapEpoch
  rewardsCapture panicPolicy backend ctrlConn phaseRef = do
    interpreterVar  <- newTVarIO Strict.Nothing
    stateVar        <- newTVarIO Strict.Nothing
    latestApplyVar  <- newTVarIO Strict.Nothing
    boundaryQueue   <- newTBQueueIO boundaryQueueBound
    blockApplyQueue <- newTBQueueIO blockApplyQueueBound
    ledgerQueue     <- newTBQueueIO ledgerQueueBound
    depositAccumRef <- newEpochParamsRef
    epochReady      <- newEmptyTMVarIO
    epochWait       <- newEmptyTMVarIO
    snapshotQueue   <- newTBQueueIO snapshotQueueBound

    -- One snapshot, two directories — both halves required, neither a duplicate:
    --   <dir>/snapshot-headers/<slot>/  ExtLedgerState minus UTxO, plus
    --     utxoSize + checksum. The entry door on resume; without it we'd
    --     replay from genesis.
    --   <dir>/lsm/snapshots/<slot>/     UTxO tables.
    -- Retention is bounded by 'snapshotRetention' (currently 3).
    -- 'LSM.saveSnapshot' rejects pre-existing dirs and the load path
    -- is upstream's V2 LSM 'implTakeSnapshot', so the two halves can't
    -- be merged.
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

    poolsCacheRef <- newIORef Nothing

    pure $
      LedgerEnabled
        LedgerEnv
          { leTracer               = tracer
          , leRewardsCapture       = rewardsCapture
          , leProtocolInfo         = pinfo
          , leDir                  = dir
          , leNetwork              = network
          , leMaxSupply            = maxSupply
          , leSystemStart          = start
          , lePanicPolicy          = panicPolicy
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
          , leBoundaryApplyResults = boundaryQueue
          , leBlockApplyResults    = blockApplyQueue
          , leRegisteredPoolsCache = poolsCacheRef
          , leDepositAccumulator   = depositAccumRef
          , leControlConnection    = ctrlConn
          , leCurrentPhase         = phaseRef
          }
  where
    -- The receiver enqueues each block to the main block queue and
    -- this queue atomically, so the shallower bound caps its
    -- prefetch window. Matches the block queue's 300; entries alias
    -- the same blocks, so the depth adds no extra retained heap.
    ledgerQueueBound :: Natural
    ledgerQueueBound = 300

    -- One slot per retained snapshot (the manager keeps three) plus
    -- a little slack so a mid-write snapshot doesn't block the worker.
    snapshotQueueBound :: Natural
    snapshotQueueBound = 4

    -- Each entry is a small, NF-forced 'BoundaryApplyData' projection
    -- (never a full 'ApplyResult'), and the replay window suppresses
    -- the enqueue, so this only absorbs the rare case where the
    -- consumer briefly lags a boundary. Look-ahead is capped by the
    -- receiver-to-worker queue, so few boundaries are ever in
    -- flight; 4 is ample slack.
    boundaryQueueBound :: Natural
    boundaryQueueBound = 4

    -- One slot per worker-side look-ahead block; entries are small,
    -- fully-forced projections. Deep enough that the consumer (one pop
    -- per block) keeps draining banked results while the worker pauses
    -- for its epoch-boundary tick; a bound below the consumer's drain
    -- batch (100) turns every worker pause into per-block consumer stalls.
    blockApplyQueueBound :: Natural
    blockApplyQueueBound = 256

-- | Seed the in-memory 'LedgerDB' buffer with the genesis state on
-- a fresh boot. The resume path uses 'initLedgerDbFromSnapshot'
-- instead so an existing populated database does not replay from
-- genesis.
initLedgerDbFromGenesis :: LedgerM ()
initLedgerDbFromGenesis = do
  env <- ask
  liftIO $ do
    sref <- initCardanoLedgerState env
    atomically $ writeTVar (leStateVar env)
      (Strict.Just (LedgerDB (StrictSeq.singleton sref)))

-- | Restore the in-memory 'LedgerDB' from an on-disk snapshot.
-- Returns 'Left' with the backend's error text on failure; the
-- caller decides how to escalate.
initLedgerDbFromSnapshot :: DiskSnapshot -> LedgerM (Either Text ())
initLedgerDbFromSnapshot snap = do
  env <- ask
  eRef <- loadSnapshotFromDisk snap
  case eRef of
    Left err -> pure (Left err)
    Right sref -> do
      liftIO $ atomically $ writeTVar (leStateVar env)
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
            !eraModel = classifyEra (ledgerStateEra (ledgerState newLedgerStateEmpty))
            !boundary =
              case ( ledgerEpochNo env oldExt
                   , ledgerEpochNo env newLedgerStateEmpty
                   ) of
                (Right oldE, Right newE) | oldE /= newE -> EpochStart
                _                                       -> InEpoch
            !newEpochBlockNo =
              applyToEpochBlockNo eraModel boundary (clsEpochBlockNo oldCls)
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
        -- Rollbacks are impossible during ingest, so the checkpoint
        -- buffer (and the open LSM handles it pins) stays small
        -- until a Follow path is live.
        phase <- readCurrentPhase (leCurrentPhase env)
        let bufferCap
              | isFollowPath phase = ledgerDbCheckpointBufferSize
              | otherwise          = ingestLedgerDbCheckpointBufferSize
            !newRef =
              DbSyncStateRef
                { srState    = Consensus.lrResult newCls
                , srTables   = newHandle
                , srCanClose = canClose
                }
            (!ledgerDB', !pruned) = pushLedgerDB bufferCap ledgerDB newRef
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
  -> Bool
  -- ^ Suppress the boundary enqueue (replay window)
  -> LedgerM (DbSyncStateRef, ApplyResult, [DbSyncStateRef])
applyBlock blk slotDetails suppressBoundary = do
  env <- ask
  result <- tickThenReapplyCheckHash (ExtLedgerCfg (getTopLevelConfig env)) blk
  case result of
    Left err -> panic err
    Right (oldRef, newResult, pruned) -> do
      let !oldCls = srState oldRef
          eventsFull =
            mapMaybe
              (convertAuxLedgerEvent (leRewardsCapture env))
              (Consensus.lrEvents newResult)
          (!events, !deposits) = splitDeposits eventsFull
          !rawNewState         = clsState (Consensus.lrResult newResult)
      -- Detect the epoch boundary from the raw post-tick state. The
      -- test is epoch-number based, so the un-finalised pulser in
      -- 'rawNewState' is irrelevant and this probe is discarded
      -- without being forced.
      isBoundary <-
        case mkOnNewEpoch env blk (clsState oldCls) rawNewState (findAdaPots events) of
          Left e   -> panic e
          Right ne -> pure (isJust ne)
      let !finalState
            | isBoundary = finaliseDrepDistr rawNewState
            | otherwise  = rawNewState
          !newCls' =
            (Consensus.lrResult newResult)
              { clsState = finalState }
      -- Rebuild 'NewEpoch' from 'finalState' so its DRep pulsing state
      -- is the completed 'DRComplete' representative rather than a
      -- 'DRPulsing' that pins the epoch stake maps.
      newEpoch <-
        if isBoundary
          then case mkOnNewEpoch env blk (clsState oldCls) finalState (findAdaPots events) of
                 Left e   -> panic e
                 Right ne -> pure ne
          else pure Nothing
      poolsRegistered <-
        liftIO $ getRegisteredPools (leRegisteredPoolsCache env) oldCls
      let mDeposits = Generic.getDeposits finalState
          appResult =
            ApplyResult
              { apPrices          = getPrices newCls'
              , apGovExpiresAfter = getGovExpiration newCls'
              , apNewEpoch        = maybeToStrictMaybe newEpoch
              , apDeposits        = maybeToStrictMaybe mDeposits
              , apSlotDetails     = slotDetails
              -- Never read from 'leLatestApplyResult'; the live copies
              -- travel via 'blockData' / 'boundaryData'.
              , apStakeSlice      = Generic.NoSlices
              , apEvents          = []
              , apGovActionState  = Nothing
              , apDepositsMap     = DepositsMap mempty
              , apPoolsRegistered = Set.empty
              }
          blockData =
            BlockApplyData
              { badDepositsMap      = DepositsMap deposits
              , badStakeSlice       = getStakeSlice env newCls' Generic.SteadyStateSlice
              , badPoolsRegistered  = poolsRegistered
              , badGovExpiresAfter  = getGovExpiration newCls'
              , badStakeKeyDeposit  = maybeToStrictMaybe (Generic.stakeKeyDeposit <$> mDeposits)
              , badPoolDeposit      = maybeToStrictMaybe (Generic.poolDeposit <$> mDeposits)
              , badPrices           = getPrices newCls'
              , badCommitteeMembers = maybe Map.empty resolveBlockCommittees (getGovState finalState)
              }
          boundaryData =
            BoundaryApplyData
              { bndNewEpoch          = maybeToStrictMaybe newEpoch
              , bndEvents            = events
              , bndGovActionState    = getGovState finalState
              , bndGovExpiresAfter   = getGovExpiration newCls'
              , bndSlotDetails       = slotDetails
              -- The pre-block state is the ended epoch's last: its
              -- "mark" snapshot fed that epoch's per-block slices.
              , bndCatchupStakeSlice = getCatchupStakeSlice env oldCls
              }
      -- Force the per-block projection to normal form before queueing
      -- so a buffered entry can't pin the ledger state it came from.
      blockData' <- liftIO (evaluate (force blockData))
      -- At an epoch boundary (outside the replay window) force the
      -- boundary projection too, so the queued entry holds only
      -- compact data and never pins a 'NewEpochState' generation.
      mBoundaryData <-
        if isBoundary && not suppressBoundary
          then Just <$> liftIO (evaluate (force boundaryData))
          else pure Nothing
      liftIO $ atomically $ do
        writeTVar (leLatestApplyResult env) (Strict.Just appResult)
        writeTBQueue (leBlockApplyResults env) blockData'
        case mBoundaryData of
          Just bd -> writeTBQueue (leBoundaryApplyResults env) bd
          Nothing -> pure ()
      pure (oldRef, appResult, pruned)

-- | 'applyBlock' plus the snapshot-cadence decision and pruning of
-- old-ref handles.
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
  -> SyncPhase             -- ^ caller's current lifecycle phase
  -> Maybe SlotNo          -- ^ replay boundary
  -> LedgerM ApplyResult
applyBlockAndSnapshot blk slotDetails phase mReplayBoundary = do
  env <- ask
  let proximity      = isSyncedNearTip slotDetails
      inReplayWindow = maybe False (blockSlot blk <=) mReplayBoundary
  (oldRef, appResult, pruned) <- applyBlock blk slotDetails inReplayWindow
  -- Record this block's epoch params in the in-memory accumulator
  -- so the consumer can flush them at the next epoch boundary.
  -- Skipped inside the replay window: those epochs are already in
  -- @epoch_param_pending@ from the previous run.
  unless inReplayWindow $
    accumulateEpochParams appResult
  -- Snapshot cadence: outside the replay window, at the epoch
  -- boundaries 'shouldSnapshotAtEpoch' permits for this phase /
  -- tip-proximity / threshold, hand the prior ref to the snapshot
  -- writer. The writer drains its queue on its own thread and
  -- releases 'srCanClose' once the on-disk write completes.
  when (not inReplayWindow
        && shouldSnapshotAtEpoch appResult phase proximity (leSnapshotNearTipEpoch env)) $
    DbSync.Worker.Ledger.Snapshot.saveCurrentLedgerState oldRef
  liftIO $ forM_ pruned $ \sr -> do
    atomically $ readTVar (srCanClose sr) >>= STM.check
    Consensus.close (srTables sr)
  pure appResult

-- | Project the 'ApplyResult'\'s deposit data into the per-epoch
-- accumulator. Byron blocks (no @apDeposits@) and pre-Shelley
-- blocks are skipped — there are no protocol-param deposits to
-- record. Multiple writes for the same epoch are idempotent
-- because protocol params are constant within an epoch.
accumulateEpochParams :: ApplyResult -> LedgerM ()
accumulateEpochParams result =
  case apDeposits result of
    Strict.Nothing -> pure ()
    Strict.Just d  -> do
      env <- ask
      let !ep = EpochParams
            { epStakeKeyDeposit = DbLovelace (fromIntegral (unCoin (Generic.stakeKeyDeposit d)))
            , epPoolDeposit     = DbLovelace (fromIntegral (unCoin (Generic.poolDeposit d)))
            }
      liftIO $ recordEpochParams
        (leDepositAccumulator env)
        (sdEpochNo (apSlotDetails result))
        ep

-- ---------------------------------------------------------------------------
-- * Helpers used by block application
-- ---------------------------------------------------------------------------

-- | Whether this block opens a new epoch or sits inside one.
data BoundaryStatus
  = EpochStart
  | InEpoch
  deriving stock (Eq, Show)

-- | Bump the per-epoch block counter following a block application.
applyToEpochBlockNo :: EraStakeModel -> BoundaryStatus -> EpochBlockNo -> EpochBlockNo
applyToEpochBlockNo NoStakeSlices  _          _                 = ByronEpochBlockNo
applyToEpochBlockNo StandardSlices EpochStart _                 = EpochBlockNo 0
applyToEpochBlockNo StandardSlices InEpoch    (EpochBlockNo n)  = EpochBlockNo (n + 1)
applyToEpochBlockNo StandardSlices InEpoch    ByronEpochBlockNo = EpochBlockNo 0

-- | Map a consensus 'LedgerState' onto our era enum.
ledgerStateEra :: LedgerState (CardanoBlock StandardCrypto) mk -> BlockEra
ledgerStateEra = \case
  LedgerStateByron _    -> Byron
  LedgerStateShelley _  -> Shelley
  LedgerStateAllegra _  -> Allegra
  LedgerStateMary _     -> Mary
  LedgerStateAlonzo _   -> Alonzo
  LedgerStateBabbage _  -> Babbage
  LedgerStateConway _   -> Conway
  LedgerStateDijkstra _ -> Dijkstra

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

-- | Whether the chain tip is approximately aligned with wall-clock.
data TipProximity
  = NearTip
  | LaggingTip
  deriving stock (Eq, Show)

-- | Pure decision: should we save a snapshot at this epoch boundary?
--
-- Cadence:
--
--   * Only fires on epoch boundaries (when @apNewEpoch@ is 'Just').
--   * Never fires at epoch @0@ — nothing to snapshot at boot.
--   * Ingest path: every 10 epochs, regardless of @nearTip@ or the
--     epoch threshold. Keeps the same coarse cadence whether the
--     resume point is epoch 5 or 1200.
--   * Follow path: every epoch when 'NearTip', or once we cross
--     @thresholdEpoch@ (catches the "Follow but slightly lagging"
--     case).
shouldSnapshotAtEpoch
  :: ApplyResult
  -> SyncPhase     -- ^ caller's current lifecycle phase
  -> TipProximity  -- ^ relationship between chain tip and wall clock
  -> Word64        -- ^ near-tip-epoch threshold (e.g. 580)
  -> Bool
shouldSnapshotAtEpoch result phase proximity thresholdEpoch =
  case apNewEpoch result of
    Strict.Nothing -> False
    Strict.Just ne ->
      let n          = unEpochNo (Generic.neEpoch ne)
          consistent = isFollowPath phase
          nearTip    = proximity == NearTip
       in n > 0
            && ( (consistent && nearTip)
                 || (consistent && n >= thresholdEpoch)
                 || n `mod` 10 == 0
               )

-- | Approximate "is the chain tip near the current wall-clock time?"
-- Uses a 60-second window, generous enough to absorb consumer-side
-- latency without flapping at the threshold.
isSyncedNearTip :: SlotDetails -> TipProximity
isSyncedNearTip sd =
  let secsBehind =
        ceiling
          (realToFrac
             (diffUTCTime' (sdCurrentTime sd) (sdSlotTime sd))
             :: Double) :: Int
   in if abs secsBehind <= 60 then NearTip else LaggingTip
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
    newer snapshots that fail the \"resume constraint\" check.

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
getStakeSlice
  :: LedgerEnv
  -> CardanoLedgerState
  -> Generic.StakeSliceMode
  -> Generic.StakeSliceRes
getStakeSlice env cls mode =
  case clsEpochBlockNo cls of
    ByronEpochBlockNo ->
      Generic.NoSlices
    EpochBlockNo n ->
      Generic.getStakeSlice
        (leProtocolInfo env)
        n
        (clsState cls)
        mode

-- | Takes the /pre-boundary/ state: its epoch-block counter and
-- \"mark\" snapshot describe the epoch that just ended.
getCatchupStakeSlice
  :: LedgerEnv
  -> CardanoLedgerState
  -> Generic.StakeSliceRes
getCatchupStakeSlice env cls =
  case clsEpochBlockNo cls of
    ByronEpochBlockNo ->
      Generic.NoSlices
    EpochBlockNo n ->
      Generic.catchupStakeSlice
        (leProtocolInfo env)
        n
        (clsState cls)

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

-- | Resolve the full committee membership for every committee-updating
-- proposal pending in this block's gov state, keyed by
-- @(proposal tx hash, proposal index)@ so the proposal pass can write
-- the complete @committee_member@ set rather than only the tx-body
-- delta. A proposal that fails to resolve is dropped, and the extractor
-- falls back to the added-members delta.
resolveBlockCommittees
  :: ConwayGovState ConwayEra
  -> Map.Map (ByteString, Word64) [ProposedCommitteeMember]
resolveBlockCommittees cgs =
  Map.fromList
    [ (govActionIdKey gaId, map projectMember (Map.toList (committeeMembers committee)))
    | gas <- toList (proposalsActions (cgsProposals cgs))
    , let gaId = gasId gas
    , UpdateCommittee {} <- [pProcGovAction (gasProposalProcedure gas)]
    , Right (Just committee) <- [findProposedCommittee gaId cgs]
    ]
  where
    projectMember (cred, expiry) =
      ProposedCommitteeMember (credHashBytes cred) (isScriptCred cred) (unEpochNo expiry)

    credHashBytes :: Credential kr -> ByteString
    credHashBytes = \case
      KeyHashObj    (KeyHash h)    -> Crypto.hashToBytes h
      ScriptHashObj (ScriptHash h) -> Crypto.hashToBytes h

    isScriptCred :: Credential kr -> Bool
    isScriptCred = \case
      KeyHashObj {}    -> False
      ScriptHashObj {} -> True

-- | Project a ledger 'GovActionId' to the @(tx hash, proposal index)@
-- key the proposal pass looks its committee membership up by.
govActionIdKey :: GovActionId -> (ByteString, Word64)
govActionIdKey (GovActionId (TxId h) (GovActionIx ix)) =
  (Crypto.hashToBytes (extractHash h), fromIntegral ix)

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

-- | Raw 28-byte hashes of the pools registered in the ledger as of
-- this state. Empty for Byron (no pools). Returned as 'ByteString' so
-- extractors can match against their raw pool-hash bytes without
-- pulling in ledger key types.
--
-- Cached on the pointer identity of the ledger's registered-pool
-- 'Map' ('RegisteredPoolsCache'): the 'Map' is structure-shared
-- across blocks without pool certificates, so the projection is
-- rebuilt only when a certificate actually changed it.
getRegisteredPools
  :: IORef (Maybe RegisteredPoolsCache)
  -> CardanoLedgerState
  -> IO (Set.Set ByteString)
getRegisteredPools cacheRef st =
  case ledgerState $ clsState st of
    LedgerStateByron _      -> pure Set.empty
    LedgerStateShelley sts  -> registeredPoolBytes cacheRef sts
    LedgerStateAllegra sts  -> registeredPoolBytes cacheRef sts
    LedgerStateMary sts     -> registeredPoolBytes cacheRef sts
    LedgerStateAlonzo ats   -> registeredPoolBytes cacheRef ats
    LedgerStateBabbage bts  -> registeredPoolBytes cacheRef bts
    LedgerStateConway stc   -> registeredPoolBytes cacheRef stc
    LedgerStateDijkstra dls -> registeredPoolBytes cacheRef dls

registeredPoolBytes
  :: Shelley.EraCertState era
  => IORef (Maybe RegisteredPoolsCache)
  -> LedgerState (ShelleyBlock p era) mk
  -> IO (Set.Set ByteString)
registeredPoolBytes cacheRef lState = do
  -- Force the projection to the shared 'Map' object itself before
  -- taking its 'StableName' — a fresh projection thunk would never
  -- compare equal and the cache would always miss.
  pools  <- evaluate (certState ^. Shelley.certPStateL . Shelley.psStakePoolsL)
  key    <- makeStableName pools
  cached <- readIORef cacheRef
  case cached of
    Just (RegisteredPoolsCache cachedKey bytes)
      | eqStableName cachedKey key -> pure bytes
    _ -> do
      let !bytes = Set.map keyHashBytes (Map.keysSet pools)
      writeIORef cacheRef (Just (RegisteredPoolsCache key bytes))
      pure bytes
  where
    certState =
      Shelley.lsCertState . Shelley.esLState . Shelley.nesEs $
        Consensus.shelleyLedgerState lState
    keyHashBytes (KeyHash h) = Crypto.hashToBytes h

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
