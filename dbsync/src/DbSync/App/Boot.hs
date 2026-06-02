{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Boot lifecycle: pure decision + effectful resolution.
--
-- 'decideBoot' classifies the observed state (sync-state row +
-- on-disk snapshots) into a 'BootDecision' (pure). The resolve
-- helpers ('resolveFreshBoot', 'resolveResumeBoot',
-- 'runBootFollowRestart') turn that decision into either an
-- 'IngestBootState' the caller wires into the Ingest pipeline, or —
-- on a 'BootFollowRestart' — drive the Follow loop inline and
-- return.
--
-- 'handlePreBootRollback' applies an outstanding rollback request
-- (CLI flag or on-DB marker) before 'decideBoot' runs.
--
-- Mismatch cases are returned as 'BootError'; 'abortBoot' renders
-- them via 'renderBootError' and exits.
module DbSync.App.Boot
  ( -- * Types
    BootDecision (..)
  , ResumeContext (..)
  , ResumeIntersection (..)
  , FollowRestartContext (..)
  , FollowRestartMode (..)
  , BootError (..)
  , IngestBootState (..)

    -- * Pure decision
  , decideBoot

    -- * Lifecycle
  , abortBoot
  , handlePreBootRollback
  , resolveFreshBoot
  , resolveResumeBoot
  , resolveIntersection
  , runBootFollowRestart
  , runFollowSession

    -- * Operator-facing rendering
  , renderBootError

    -- * Helpers (exported for tests)
  , mkCardanoPoint
  , candidateSnapshotSlots
  , resumeContextFrom
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (TVar, newTBQueueIO, newTVarIO, readTVar)
import qualified Data.Text as T
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Settings as HasqlSettings
import Data.IORef (newIORef)

import Cardano.Network.NodeToClient (IOManager, withIOManager)
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.Block.Abstract (fromRawHash)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Cardano.Node ()                       -- 'CanHardFork' orphan
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.Shelley.HFEras ()                     -- per-era HFC instances
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()    -- 'LedgerSupportsProtocol' orphans
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot (..))
import Ouroboros.Network.Block (pattern BlockPoint, pattern GenesisPoint)
import Ouroboros.Network.Magic (NetworkMagic)

import DbSync.AppM (runAppM)
import DbSync.App.Env
  ( CoreEnv (..)
  , CoreWithConn (..)
  , FollowEnv (..)
  , TracerWithControl (..)
  )
import DbSync.ChainSync.Connection
  ( IntersectionRequirement (..)
  , connectToNode
  )
import DbSync.Db.Schema.SyncState (SyncStateRow (..))
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Extractor (ExtractState (..), ExtractorDef (..), freshExtractState)
import DbSync.Parser.Types (CardanoPoint)
import DbSync.Phase.Current (setCurrentPhase)
import qualified DbSync.Phase.Following.Resolver as FollowResolver
import qualified DbSync.Phase.Following.Run as Follow
import qualified DbSync.Phase.Following.Rollback as Rollback
import DbSync.Phase.Following.Tuning (defaultFollowTuning, setFollowSessionGUCs)
import qualified DbSync.Phase.Following.Writer as FollowingWriter
import DbSync.Phase.Ingest.DedupStore (DedupStores, newStores)
import DbSync.Phase.Ingest.LsmSession (LsmSession, closeLsmSession)
import DbSync.Phase.Ingest.ReceiverStats (newReceiverStats)
import DbSync.Phase.Type (SyncPhase (..))
import DbSync.Resolver (IdResolver)
import DbSync.Writer (Writer)
import DbSync.StateQuery
  ( StateQueryVar
  , seedInterpreterFromLedgerState
  )
import DbSync.SyncState.Manager (mkResumeExtractState)
import DbSync.SyncState.Resume (CleanupMode (..), deleteRowsPastSlot)
import DbSync.SyncState.Row
  ( ControlConnection (..)
  , clearPendingRollbackSlot
  , closeControlConnection
  , fetchBlockHashAtSlot
  , openControlConnection
  , populateCostModelCache
  , readPendingRollbackSlot
  , rebuildDedupMaps
  )
import DbSync.Trace.Timing (withHeartbeatIO)
import DbSync.Trace.Types
  ( AppTracer
  , logErrorIO
  , logInfoIO
  , logWarnIO
  )
import DbSync.Trace.Watchdog (newWatchdog, runWatchdogIO)
import DbSync.Worker.Ledger.Fingerprint (LedgerStateFingerprint, renderFingerprint)
import DbSync.Worker.Ledger.Snapshot (deleteNewerSnapshots)
import DbSync.Worker.Ledger.State
  ( initLedgerDbFromGenesis
  , initLedgerDbFromSnapshot
  , readCurrentStateUnsafe
  )
import DbSync.Worker.Ledger.Types (HasLedgerEnv (..), LedgerEnv (..))
import DbSync.Worker.Ledger.Worker (withLedgerThreads)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | What the boot flow should do.
data BootDecision
  = BootFresh
    -- ^ Start from genesis (fresh DB or a seeded-but-uncommitted row).
  | BootResume !ResumeContext
    -- ^ Resume past a previous run.
  | BootFollowRestart !FollowRestartContext
    -- ^ A prior run completed sync; resume directly in Follow.
  deriving stock (Eq, Show)

-- | Information needed to resume from a 'SyncStateRow'.
--
-- Invariant: when 'rcChosenSnapshot' is 'Just' (ledger-enabled
-- resume), its 'dsNumber' equals the head of 'rcIntersection'\'s
-- @NeedsPgHashes@ list — that snapshot is the one the in-RAM
-- ledger is restored from, and also the preferred intersection
-- point we offer the node.
data ResumeContext = ResumeContext
  { rcSyncState      :: !SyncStateRow
  , rcChosenSnapshot :: !(Maybe DiskSnapshot)
  , rcIntersection   :: !ResumeIntersection
  }
  deriving stock (Eq, Show)

-- | Information needed to enter the Follow phase on a restart after
-- Ingest+Prep have completed (@sync_complete = true@). 'frcMode'
-- carries the ledger-aware data ('FollowRestartMode'); 'frcSyncState'
-- carries the raw row for diagnostics and bookkeeping.
data FollowRestartContext = FollowRestartContext
  { frcSyncState :: !SyncStateRow
  , frcMode      :: !FollowRestartMode
  }
  deriving stock (Eq, Show)

-- | What the Follow-restart path knows about the chainsync resume
-- point before 'prepareFollowRestart' runs.
--
-- Ledger disabled: PG is the single source of truth. The decision
-- step pre-builds the intersection point from
-- @last_committed_slot@/@last_committed_block_hash@; if either is
-- missing the row is malformed and 'decideFollowRestart' returns
-- 'BootSyncStateMissing' instead of constructing this.
--
-- Ledger enabled: the decision step hands the orchestrator a
-- non-empty list of candidate snapshots (newest-first). The
-- orchestrator walks them newest-first, picking the first whose
-- slot has a matching @block.hash@ in PG. That chosen snapshot
-- becomes the restart point: the ledger is loaded from it, the gap
-- to @last_committed_slot@ becomes the replay window, and chainsync
-- intersects at the snapshot\'s point.
data FollowRestartMode
  = FollowRestartLedgerDisabled !CardanoPoint
    -- ^ Pre-validated intersection point.
  | FollowRestartLedgerEnabled ![DiskSnapshot]
    -- ^ Non-empty candidate list, newest-first.
  deriving stock (Eq, Show)

-- | How the chainsync intersection point(s) are produced.
--
-- Ledger-disabled resume can hand the receiver a fully-formed point
-- straight from the @last_committed_*@ columns. Ledger-enabled resume
-- can\'t: a snapshot only knows its slot (consensus's 'DiskSnapshot'
-- is @dsNumber + dsSuffix@), and PG\'s last-committed hash refers to
-- a /different/ block. So we nominate slots here and let the caller
-- ask PG for the canonical hash at each — mirrors upstream
-- cardano-db-sync's @verifySnapshotPoint@.
data ResumeIntersection
  = ReadyPoint !CardanoPoint
    -- ^ Ledger-disabled: @(slot, hash)@ from 'SyncStateRow'.
  | NeedsPgHashes ![Word64]
    -- ^ Ledger-enabled: candidate snapshot slots, /newest-first/.
    -- Head is the chosen snapshot for ledger restoration; the rest
    -- are fallbacks. The caller resolves each via PG and drops
    -- orphans (no matching @block.hash@) silently.
  deriving stock (Eq, Show)

-- | Resolved state handed from the boot-resolution step to the
-- Ingest pipeline setup.
--
-- The boot resolution produces 'Just' this on 'BootFresh' /
-- 'BootResume'; on 'BootFollowRestart' it runs the Follow loop
-- inline and returns 'Nothing'.
data IngestBootState = IngestBootState
  { ibsInitialExtractState :: !ExtractState
    -- ^ Counters and per-table state to seed the consumer with.
  , ibsDedupStores         :: !DedupStores
    -- ^ LSM-backed dedup tables, either freshly opened ('BootFresh')
    -- or rebuilt from PG ('BootResume').
  , ibsIntersection        :: !IntersectionRequirement
    -- ^ Where chainsync should resume from.
  , ibsReplayBoundary      :: !(Maybe SlotNo)
    -- ^ Upper edge of the replay window. The consumer skips
    -- 'processBlock' for blocks at or below this slot. 'Nothing'
    -- when ledger is off or the snapshot is aligned with PG.
  , ibsReplayStart         :: !(Maybe SlotNo)
    -- ^ Lower edge of the replay window: the chosen snapshot's
    -- slot. Drives the percentage in the consumer's replay
    -- progress log. 'Just' iff 'ibsReplayBoundary' is.
  , ibsAddressIdCounter    :: !Int64
    -- ^ Initial @address.id@ for the 'TxOutWorker'. Seeded from the
    -- sync-state row on resume, 1 on fresh.
  }

-- | Boot mismatches that abort the run.
data BootError
  = BootSyncStateMissing
    -- ^ Schema present but no @dbsync_sync_state@ row.
  | BootLedgerEnabledMismatch !Bool !Bool
    -- ^ The row's @ledger_enabled@ disagrees with the current
    -- config's @ledger.enabled@. Fields: @(rowSays, configSays)@.
  | BootResumeStateMissing
    -- ^ Ledger enabled, PG has committed data, on-disk snapshot
    -- directory is empty.
  | BootSnapshotsWithoutPgState
    -- ^ Ledger enabled, snapshot directory has content, PG row
    -- records no committed progress.
  | BootNoUsableSnapshot !Word64
    -- ^ Ledger enabled, the row has @last_committed_slot@, but no
    -- on-disk snapshot exists at or before that slot.
  | BootLedgerStateFingerprintMismatch !LedgerStateFingerprint !LedgerStateFingerprint
    -- ^ The fingerprint file in the ledger state directory describes
    -- a different chain than the current config. Fields:
    -- @(onDisk, expected)@.
  | BootLedgerStateFingerprintMissing !FilePath
    -- ^ The ledger state directory contains data but no readable
    -- fingerprint file. Carries the directory path for the operator
    -- message.
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Decision
-- ---------------------------------------------------------------------------

-- | Classify the boot.
decideBoot
  :: Maybe SyncStateRow
  -> [DiskSnapshot]            -- ^ Disk snapshots, newest-first. Empty when ledger disabled.
  -> Bool                      -- ^ @ledger.enabled@ from current config.
  -> Either BootError BootDecision
decideBoot mRow snapshots ledgerEnabledCfg = case mRow of
  Nothing
    | ledgerEnabledCfg, not (null snapshots) ->
        Left BootSnapshotsWithoutPgState
    | otherwise ->
        Left BootSyncStateMissing

  Just row
    | ssrLedgerEnabled row /= ledgerEnabledCfg ->
        Left $ BootLedgerEnabledMismatch (ssrLedgerEnabled row) ledgerEnabledCfg

    | ssrSyncComplete row ->
        decideFollowRestart row snapshots ledgerEnabledCfg

    | rowHasNoCommittedProgress row ->
        -- Snapshots without committed PG progress would diverge
        -- silently from genesis-seeded ledger state.
        if ledgerEnabledCfg && not (null snapshots)
          then Left BootSnapshotsWithoutPgState
          else Right BootFresh

    | not ledgerEnabledCfg ->
        case (ssrLastCommittedSlot row, ssrLastCommittedBlockHash row) of
          (Just slotNo, Just blockHash) ->
            Right $ BootResume
              ResumeContext
                { rcSyncState     = row
                , rcChosenSnapshot = Nothing
                , rcIntersection  = ReadyPoint (mkCardanoPoint slotNo blockHash)
                }
          _ ->
            Left BootSyncStateMissing

    | otherwise ->
        case (ssrLastCommittedSlot row, ssrLastCommittedBlockHash row) of
          (Just slotNo, Just _)
            | null snapshots -> Left BootResumeStateMissing
            | otherwise ->
                case candidateSnapshotSlots snapshots slotNo of
                  []                 -> Left (BootNoUsableSnapshot slotNo)
                  candidates@(c : _) ->
                    Right $ BootResume
                      ResumeContext
                        { rcSyncState      = row
                        , rcChosenSnapshot = Just c
                        , rcIntersection  = NeedsPgHashes (map dsNumber candidates)
                        }
          _ -> Left BootSyncStateMissing

-- | Build the 'BootFollowRestart' decision for @sync_complete =
-- true@.
--
-- Ledger disabled: PG is the single source of truth. The caller
-- intersects chainsync directly at the row's @last_committed_*@,
-- which the decision pre-builds into 'FollowRestartLedgerDisabled'.
-- A row missing either of @last_committed_slot@ or
-- @last_committed_block_hash@ is malformed and rejected as
-- 'BootSyncStateMissing'.
--
-- Ledger enabled: returns the candidate list (snapshots at or
-- before @last_committed_slot@, newest-first). Same fallback
-- conditions as 'BootResume' — empty snapshot directory is
-- 'BootResumeStateMissing', candidates all beyond the committed
-- slot is 'BootNoUsableSnapshot'.
decideFollowRestart
  :: SyncStateRow
  -> [DiskSnapshot]
  -> Bool
  -> Either BootError BootDecision
decideFollowRestart row snapshots ledgerEnabledCfg
  | not ledgerEnabledCfg =
      case (ssrLastCommittedSlot row, ssrLastCommittedBlockHash row) of
        (Just s, Just h) ->
          Right $ BootFollowRestart
            FollowRestartContext
              { frcSyncState = row
              , frcMode      = FollowRestartLedgerDisabled (mkCardanoPoint s h)
              }
        _ ->
          Left BootSyncStateMissing
  | otherwise =
      case ssrLastCommittedSlot row of
        Nothing ->
          -- 'sync_complete = true' implies Ingest+Prep ran, which
          -- only happens after at least one epoch boundary was
          -- committed. Treat the inconsistent shape as a missing
          -- resume anchor.
          Left BootSyncStateMissing
        Just slotNo
          | null snapshots -> Left BootResumeStateMissing
          | otherwise ->
              case candidateSnapshotSlots snapshots slotNo of
                []         -> Left (BootNoUsableSnapshot slotNo)
                candidates ->
                  Right $ BootFollowRestart
                    FollowRestartContext
                      { frcSyncState = row
                      , frcMode      = FollowRestartLedgerEnabled candidates
                      }

-- | True when the row has no committed chain position.
rowHasNoCommittedProgress :: SyncStateRow -> Bool
rowHasNoCommittedProgress r =
  isNothing (ssrLastCommittedSlot r)
    && isNothing (ssrLastCommittedBlockNo r)
    && isNothing (ssrLastCommittedBlockHash r)

-- | Build a 'ResumeContext' with no chosen snapshot and a default
-- intersection point. Used by the in-process Ingest → Prep → Follow
-- handoff where chainsync intersects at the row's last committed
-- block (the ledger and PG are already aligned by the receiver's
-- 'latestPointRef').
resumeContextFrom :: SyncStateRow -> Maybe DiskSnapshot -> ResumeContext
resumeContextFrom row mSnap =
  ResumeContext
    { rcSyncState     = row
    , rcChosenSnapshot = mSnap
    , rcIntersection  = case (ssrLastCommittedSlot row, ssrLastCommittedBlockHash row) of
        (Just s, Just h) -> ReadyPoint (mkCardanoPoint s h)
        _                -> ReadyPoint GenesisPoint
    }

-- | Snapshots at or before @lastSlot@, newest-first (relies on
-- @listSnapshots@\'s newest-first ordering).
candidateSnapshotSlots :: [DiskSnapshot] -> Word64 -> [DiskSnapshot]
candidateSnapshotSlots snaps lastSlot =
  filter (\ds -> dsNumber ds <= lastSlot) snaps

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Build a 'CardanoPoint' from a raw slot number and 32-byte block
-- header hash.
mkCardanoPoint :: Word64 -> ByteString -> CardanoPoint
mkCardanoPoint slotNo blockHash =
  BlockPoint
    (SlotNo slotNo)
    (fromRawHash (Proxy @(CardanoBlock StandardCrypto)) blockHash)

-- ---------------------------------------------------------------------------
-- * Operator-facing rendering
-- ---------------------------------------------------------------------------

-- | Multi-line message suitable for stderr.
renderBootError :: BootError -> Text
renderBootError = \case
  BootSyncStateMissing ->
    T.unlines
      [ "Cannot resume: PG schema is present but the dbsync_sync_state row is empty."
      , ""
      , "This usually means one of:"
      , "  - The schema was manually created without seeding the sync-state row."
      , "  - A failed earlier run left the DB in a partial state."
      , ""
      , "Recovery: restart with --resync-from-genesis to wipe the database and"
      , "re-sync from genesis."
      ]

  BootLedgerEnabledMismatch rowSays cfgSays ->
    T.unlines
      [ "Cannot resume: ledger.enabled has flipped between runs."
      , ""
      , "  dbsync_sync_state.ledger_enabled = " <> show rowSays
      , "  current config ledger.enabled    = " <> show cfgSays
      , ""
      , "Resuming with a different ledger setting would invalidate the existing"
      , "data. Recovery options:"
      , "  - Restore the previous config so it matches the database."
      , "  - Restart with --resync-from-genesis to wipe both the database and"
      , "    the ledger state directory and re-sync from genesis."
      ]

  BootResumeStateMissing ->
    T.unlines
      [ "Cannot resume: PG database has committed data but the ledger state"
      , "directory is empty."
      , ""
      , "This usually means the state directory was moved, deleted, or never"
      , "copied into place; or that ledger.enabled was previously false and is"
      , "now true."
      , ""
      , "Recovery options:"
      , "  - Restore the state directory from a backup or snapshot bundle."
      , "  - Restart with --resync-from-genesis to wipe and re-sync from genesis."
      ]

  BootSnapshotsWithoutPgState ->
    T.unlines
      [ "Cannot resume: the ledger state directory contains snapshots, but the"
      , "PG database has no committed progress."
      , ""
      , "The two should advance in lock-step. This usually means the PG schema"
      , "was wiped without also wiping the state directory."
      , ""
      , "Recovery: restart with --resync-from-genesis to wipe both and re-sync"
      , "from genesis."
      ]

  BootNoUsableSnapshot lastSlot ->
    T.unlines
      [ "Cannot resume: PG records committed progress through slot "
          <> show lastSlot
      , "but no on-disk snapshot at or before that slot is available."
      , ""
      , "This usually means snapshots were manually deleted past the resume"
      , "point."
      , ""
      , "Recovery: restart with --resync-from-genesis to wipe both the database"
      , "and the ledger state directory and re-sync from genesis."
      ]

  BootLedgerStateFingerprintMismatch onDisk expected ->
    T.unlines
      [ "Cannot resume: the ledger state directory was created for a different chain."
      , ""
      , "  On disk  : " <> renderFingerprint onDisk
      , "  This run : " <> renderFingerprint expected
      , ""
      , "Resuming would corrupt the database. Recovery options:"
      , "  - Point --ledger-state-dir at the directory used for this network, or"
      , "  - Restart with --resync-from-genesis to wipe and re-sync from genesis."
      ]

  BootLedgerStateFingerprintMissing dir ->
    T.unlines
      [ "Cannot resume: the ledger state directory contains data but no"
      , "fingerprint file. Path:"
      , ""
      , "  " <> T.pack dir
      , ""
      , "This means the directory was populated by a build that predates the"
      , "fingerprint check, or the file was deleted. Refusing to proceed because"
      , "we cannot verify the directory belongs to this chain."
      , ""
      , "Recovery: restart with --resync-from-genesis to wipe and re-sync from"
      , "genesis."
      ]

-- ---------------------------------------------------------------------------
-- * Lifecycle
-- ---------------------------------------------------------------------------

-- | Render a 'BootError' and exit. Never returns.
abortBoot :: AppTracer -> BootError -> IO a
abortBoot tracer err = do
  for_ (T.lines (renderBootError err)) (logErrorIO tracer "Boot")
  exitFailure

-- | Apply a rollback request that arrived before normal boot.
--
-- The CLI flag wins over the on-DB marker; if neither is set, this
-- is a no-op. On success the marker is cleared so the next boot
-- doesn't replay the rollback. Ledger snapshots strictly newer than
-- the target are dropped so the next boot's snapshot picker stays
-- aligned with the rolled-back chain.
handlePreBootRollback
  :: AppTracer
  -> CoreEnv
  -> ControlConnection
  -> [TableDef]
  -> HasLedgerEnv
  -> Maybe Word64        -- ^ CLI request
  -> IO ()
handlePreBootRollback tracer coreEnv ctrl tableDefs hasLE mCli = do
  mMarker <- runAppM ctrl readPendingRollbackSlot
  let mTarget = mCli <|> mMarker
  for_ mTarget $ \targetSlot -> do
    case mCli of
      Just _  ->
        logInfoIO tracer "Boot" $
          "--rollback-to-slot " <> show targetSlot <> " requested"
      Nothing ->
        logInfoIO tracer "Boot" $
          "Recovering from previous deep rollback to slot "
            <> show targetSlot
    let rollbackEnv = CoreWithConn coreEnv (unControlConnection ctrl)
    mResolved <- runAppM rollbackEnv
      (Rollback.rollbackToSlot tableDefs targetSlot)
    case mResolved of
      Just blockNo ->
        logInfoIO tracer "Boot" $
          "Rollback complete; database tip is block " <> show blockNo
      Nothing ->
        logInfoIO tracer "Boot" $
          "Rollback no-op: no block at or after slot "
            <> show targetSlot
            <> " (database already below the requested point)"
    case hasLE of
      LedgerEnabled lenv ->
        runAppM lenv (deleteNewerSnapshots (SlotNo targetSlot))
      LedgerDisabled _ -> pure ()
    runAppM ctrl clearPendingRollbackSlot

-- | Resolve a 'BootFresh' decision: seed the ledger DB from genesis
-- (when ledger is enabled), open the dedup stores, and start
-- chainsync at 'IntersectGenesis'.
resolveFreshBoot
  :: AppTracer
  -> HasLedgerEnv
  -> LsmSession
  -> IO IngestBootState
resolveFreshBoot tracer hasLedgerEnv lsmSession = do
  case hasLedgerEnv of
    LedgerEnabled lenv -> do
      logInfoIO tracer "Boot" "Seeding ledger DB from genesis"
      runAppM lenv initLedgerDbFromGenesis
    LedgerDisabled _ -> pure ()
  stores <- newStores lsmSession
  pure IngestBootState
    { ibsInitialExtractState = freshExtractState
    , ibsDedupStores         = stores
    , ibsIntersection        = IntersectGenesis
    , ibsReplayBoundary      = Nothing
    , ibsReplayStart         = Nothing
    , ibsAddressIdCounter    = 1
    }

-- | Resolve a 'BootResume' decision.
--
-- Cleans rows past @last_committed_slot@ left over from a prior
-- crash, rebuilds the dedup stores from PG, populates the
-- cost-model cache, and (when ledger is on) loads the chosen
-- snapshot into the in-memory 'LedgerDB' and seeds the HFC
-- interpreter. When the snapshot lags PG the replay window
-- ('ibsReplayBoundary' / 'ibsReplayStart') is set so the consumer
-- can suppress its PG-write path until the ledger worker has
-- re-applied the gap.
resolveResumeBoot
  :: AppTracer
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> StateQueryVar
  -> HasLedgerEnv
  -> ControlConnection
  -> [TableDef]
  -> LsmSession
  -> ResumeContext
  -> IO IngestBootState
resolveResumeBoot
  tracer topLevelCfg stateQueryVar hasLedgerEnv
  consumerCtrlConn tableDefs lsmSession rc = do
    let row     = rcSyncState rc
        envCtrl = TracerWithControl tracer consumerCtrlConn
    logInfoIO tracer "Boot" $
      "Resuming from slot "
        <> show (ssrLastCommittedSlot row)
        <> ", block "
        <> show (ssrLastCommittedBlockNo row)
    logInfoIO tracer "Boot" "Cleaning rows past last_committed_slot…"
    deleted <- runAppM envCtrl (deleteRowsPastSlot IngestResume tableDefs row)
    when (deleted > 0) $
      logInfoIO tracer "Boot" $
        "Cleaned up " <> show deleted
          <> " rows past last_committed_slot from a prior crash"
    logInfoIO tracer "Boot" "Rebuilding dedup stores from PG…"
    stores  <- runAppM envCtrl (rebuildDedupMaps tableDefs lsmSession)
    cmCache <- runAppM envCtrl (populateCostModelCache tableDefs)

    (replayBoundary, replayStart) <-
      resolveResumeReplay tracer topLevelCfg stateQueryVar hasLedgerEnv row rc

    intersection <- resolveIntersection tracer consumerCtrlConn rc
    let resumeState = (mkResumeExtractState row)
          { esCostModelCache = cmCache }
    pure IngestBootState
      { ibsInitialExtractState = resumeState
      , ibsDedupStores         = stores
      , ibsIntersection        = intersection
      , ibsReplayBoundary      = replayBoundary
      , ibsReplayStart         = replayStart
      , ibsAddressIdCounter    = ssrAddressIdCounter row
      }

-- | Load the resume snapshot (when ledger is on) and compute the
-- replay window. Internal helper of 'resolveResumeBoot'.
resolveResumeReplay
  :: AppTracer
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> StateQueryVar
  -> HasLedgerEnv
  -> SyncStateRow
  -> ResumeContext
  -> IO (Maybe SlotNo, Maybe SlotNo)
resolveResumeReplay tracer topLevelCfg stateQueryVar hasLedgerEnv row rc =
  case (hasLedgerEnv, rcChosenSnapshot rc) of
    (LedgerDisabled _, _)            -> pure (Nothing, Nothing)
    (LedgerEnabled _,  Nothing)      ->
      panic "BootResume (ledger enabled) returned without a chosen snapshot"
    (LedgerEnabled lenv, Just snap)  -> do
      loadAndSeedSnapshot tracer lenv stateQueryVar topLevelCfg snap
      let startSlot = dsNumber snap
      for_ (ssrLastCommittedSlot row) $ \endSlot ->
        when (endSlot > startSlot) $
          logInfoIO tracer "Boot" $
            "Resume replay window: applying ledger from slot "
              <> show startSlot <> " forward to last-committed slot "
              <> show endSlot <> " ("
              <> show (endSlot - startSlot)
              <> " slots). Consumer COPY paused; ledger worker"
              <> " applying. Snapshot writes suppressed inside"
              <> " the window."
      pure
        ( fmap SlotNo (ssrLastCommittedSlot row)
        , Just (SlotNo startSlot)
        )

-- | Turn a 'ResumeContext' into the receiver's intersection
-- requirement. Mirrors upstream cardano-db-sync's
-- @verifySnapshotPoint@: the snapshot supplies /the slot/, PG\'s
-- @block@ table is the oracle for /the hash/. Orphaned candidates
-- are dropped silently; panics when every candidate is orphaned.
resolveIntersection
  :: AppTracer
  -> ControlConnection
  -> ResumeContext
  -> IO IntersectionRequirement
resolveIntersection tracer ctrl rc = case rcIntersection rc of
  ReadyPoint p ->
    pure (IntersectAt [p])
  NeedsPgHashes slots -> do
    candidates <- catMaybes <$> for slots (resolveSlot tracer ctrl)
    case candidates of
      [] -> do
        logErrorIO tracer "Boot" $
          "All " <> show (length slots) <> " snapshot intersection candidates "
            <> "are orphaned in PG (no matching row in the block table). "
            <> "Snapshot slots tried: " <> show slots <> ". "
            <> "Recovery: restore PG from a backup that covers one of these "
            <> "slots, or restart with --resync-from-genesis."
        panic "resolveIntersection: no usable snapshot intersection points"
      _ ->
        pure (IntersectAt candidates)

resolveSlot
  :: AppTracer
  -> ControlConnection
  -> Word64
  -> IO (Maybe CardanoPoint)
resolveSlot tracer ctrl slot = do
  mHash <- runAppM ctrl (fetchBlockHashAtSlot slot)
  case mHash of
    Nothing -> do
      logInfoIO tracer "Boot" $
        "Snapshot at slot " <> show slot
          <> " has no matching row in the block table; "
          <> "skipping as a chainsync intersection candidate."
      pure Nothing
    Just h ->
      pure (Just (mkCardanoPoint slot h))

-- ---------------------------------------------------------------------------
-- * Follow restart
-- ---------------------------------------------------------------------------

-- | Resolved Follow-restart parameters: the chainsync intersection
-- point and, when the on-disk snapshot lags PG, the (lower, upper)
-- edges of the replay window the ledger worker walks while Follow's
-- consumer skips its PG-write path.
--
-- Both replay edges are 'Just' together or 'Nothing' together;
-- 'Nothing' when ledger is off or the snapshot is aligned with PG.
data FollowRestartStart = FollowRestartStart
  { frsIntersectPoint  :: !CardanoPoint
  , frsReplayBootSlot  :: !(Maybe SlotNo)
  , frsReplayStartSlot :: !(Maybe SlotNo)
  }

-- | Block-count threshold above which a Follow-restart resume warns
-- that @--resync-from-genesis@ may be faster than replaying every
-- block one transaction at a time.
--
-- Tuned for mainnet: 500k blocks is roughly four months of chain
-- advance. Below this the per-block Follow loop is the right tool;
-- above it the COPY pipeline that @--resync-from-genesis@ triggers
-- typically catches up faster despite redoing the historical work.
resumeGapWarnBlocks :: Word64
resumeGapWarnBlocks = 500_000

-- | One-shot helper that waits for the receiver to publish its first
-- rollback-boundary observation, computes the gap between the
-- resumed @last_committed_block_no@ and the node tip, and warns the
-- operator when the gap exceeds 'resumeGapWarnBlocks'.
--
-- The receiver publishes @nodeTip - k@ on every 'MsgRollForward'; we
-- recover the tip as @boundary + k@. While the chain is shorter than
-- @k@ the boundary stays 'Nothing' and the helper blocks until a
-- tip is seen, which on mainnet always happens within the first
-- chainsync intersection.
--
-- Exits silently when there is no resumed block number (the row was
-- inconsistent) or when the gap is below the threshold.
checkResumeGap
  :: AppTracer
  -> Word64                          -- ^ Protocol security parameter @k@.
  -> Maybe Word64                    -- ^ Resumed @last_committed_block_no@.
  -> TVar (Maybe BlockNo)            -- ^ Receiver-populated rollback boundary.
  -> IO ()
checkResumeGap _ _ Nothing _ = pure ()
checkResumeGap tracer kBlocks (Just lastBlock) boundaryVar = do
  boundary <- atomically $ do
    mb <- readTVar boundaryVar
    case mb of
      Just (BlockNo b) -> pure b
      Nothing          -> retry
  let nodeTip = boundary + kBlocks
      gap     = if nodeTip > lastBlock then nodeTip - lastBlock else 0
  when (gap > resumeGapWarnBlocks) $
    logWarnIO tracer "Boot" $
      "Resume point is " <> show gap <> " blocks behind the node tip "
        <> "(last_committed_block_no=" <> show lastBlock
        <> ", node tip ~ " <> show nodeTip <> "). "
        <> "Replaying that many blocks one transaction at a time can "
        <> "take significantly longer than starting from genesis with "
        <> "the bulk-load pipeline. Consider --resync-from-genesis if "
        <> "catch-up time matters."

-- | Boot directly into 'FollowingChainTip' on a restart after Prep
-- has already marked sync complete. Releases the Ingest LSM
-- session (the follow loop doesn't use it), flips the phase, and
-- runs the follow loop with the ledger worker bracket parametrised
-- by the replay window computed from the loaded snapshot.
--
-- When ledger is enabled, the on-disk snapshot is the authoritative
-- restart point. The flow:
--
--   1. Walk the candidate snapshots newest-first; pick the first
--      whose slot has a matching @block.hash@ in PG.
--   2. Load that snapshot into the in-memory 'LedgerDB'.
--   3. If the snapshot's slot is below @last_committed_slot@ —
--      the async snapshot writer was behind the consumer at
--      shutdown — configure a replay window. The ledger worker
--      re-applies the gap from the receiver fan-out; Follow's
--      consumer skips its PG-write path for blocks in the window.
--   4. Start the ledger worker + snapshot writer and intersect
--      chainsync at the snapshot's point.
--
-- When ledger is disabled there is no snapshot to load; chainsync
-- intersects directly at the row's @last_committed_*@.
--
-- When the optional shutdown signal fires, the Follow loop is
-- cancelled and this returns normally; otherwise it blocks forever
-- (production behaviour).
runBootFollowRestart
  :: AppTracer
  -> HasqlSettings.Settings
  -> CoreEnv
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath                                         -- ^ socketPath
  -> SystemStart
  -> StateQueryVar
  -> HasLedgerEnv
  -> ControlConnection                                -- ^ consumerCtrlConn
  -> LsmSession
  -> FollowRestartContext
  -> Maybe (IO ())                                    -- ^ mShutdown
  -> IO ()
runBootFollowRestart
  tracer hasqlSettings coreEnv topLevelCfg networkMagic
  socketPath systemStart stateQueryVar hasLedgerEnv consumerCtrlConn
  lsmSession frc mShutdown = do
    logInfoIO tracer "Boot" "Boot: sync_complete=true; entering FollowingVolatileTail"
    -- The follow loop doesn't touch the ingest LSM tables; release
    -- the session so the directory lock is dropped before entering
    -- the long-running follow loop.
    closeLsmSession lsmSession
    runAppM coreEnv (setCurrentPhase (ceCurrentPhase coreEnv) FollowingVolatileTail)
    watchdog <- newWatchdog (ceMinSeverity coreEnv)

    let row = frcSyncState frc
        tableDefs = concatMap pdTables (ceExtractors coreEnv)
    -- 'FollowRestart' mode skips the dedup-counter DELETE. The counter
    -- columns on 'SyncStateRow' are frozen at Ingest's last
    -- pending-boundary snapshot; running them here would wipe every
    -- dedup row Ingest's last two epochs and Follow wrote, silently
    -- orphaning the fact-table FKs that reference them.
    logInfoIO tracer "Boot" "Cleaning rows past last_committed_slot…"
    deleted <- runAppM (TracerWithControl tracer consumerCtrlConn)
                 (deleteRowsPastSlot FollowRestart tableDefs row)
    when (deleted > 0) $
      logInfoIO tracer "Boot" $
        "Cleaned up " <> show deleted
          <> " rows past last_committed_slot from a prior Follow crash"

    -- Pick the chainsync restart point and the replay window.
    restartStart <- prepareFollowRestart
      coreEnv consumerCtrlConn
      hasLedgerEnv stateQueryVar topLevelCfg frc
    let intersectPoint = frsIntersectPoint  restartStart
        mReplayBoot    = frsReplayBootSlot  restartStart
        mReplayStart   = frsReplayStartSlot restartStart

    withIOManager $ \iomgr ->
      -- Start the ledger worker + snapshot writer with the replay
      -- boundary baked in. Inside the window the worker suppresses
      -- snapshot writes and 'accumulateEpochParams' because those
      -- epochs are already represented in PG / on disk.
      withLedgerThreads hasLedgerEnv mReplayBoot stateQueryVar watchdog $ do
        -- A fresh receiver-side state. Ingest has been bypassed on this
        -- restart path, so none of it is inherited from an upstream env.
        blockQueue       <- newTBQueueIO 500
        receiverStats    <- newReceiverStats
        latestPointRef   <- newIORef Nothing
        rollbackBoundary <- newTVarIO Nothing

        let mLedgerQueue = case hasLedgerEnv of
              LedgerEnabled lenv -> Just (leLedgerQueue lenv)
              LedgerDisabled _   -> Nothing
            intersectReq = IntersectAt [intersectPoint]
            mkEnv conn resolver writer =
              FollowEnv
                { feCore                = coreEnv
                , feBlockQueue          = blockQueue
                , feHasLedgerEnv        = hasLedgerEnv
                , feStateQueryVar       = stateQueryVar
                , feSystemStart         = systemStart
                , feReceiverStats       = receiverStats
                , feWatchdog            = watchdog
                , feLatestReceivedPoint = latestPointRef
                , feHasqlConnection     = conn
                , feResolver            = resolver
                , feWriter              = writer
                , feControlConnection   = consumerCtrlConn
                , feRollbackBoundary    = rollbackBoundary
                , feReplayBootSlot      = mReplayBoot
                , feReplayStartSlot     = mReplayStart
                }

        let mLastBlock = ssrLastCommittedBlockNo (frcSyncState frc)
            kBlocks    = ceSecurityParam coreEnv
        withAsync (runWatchdogIO tracer watchdog blockQueue mLedgerQueue Nothing) $ \watchdogThread -> do
          link watchdogThread
          withAsync (checkResumeGap tracer kBlocks mLastBlock rollbackBoundary) $ \gapThread -> do
            link gapThread
            runFollowSession tracer "Boot" iomgr hasqlSettings topLevelCfg
              networkMagic socketPath intersectReq mShutdown mkEnv

-- | Open a dedicated Follow hasql connection, build its resolver and
-- writer, hand them to the caller-supplied 'FollowEnv' builder, and
-- run 'Follow.run' against the resulting env. The chainsync receiver
-- runs as a linked async for the duration; an optional shutdown
-- signal races the loop so tests can stop cleanly.
--
-- Shared by 'runBootFollowRestart' (cold restart into Follow) and
-- 'handoffToFollow' (in-process Ingest \\u2192 Prep \\u2192 Follow handoff).
-- Each caller is responsible for the watchdog and ledger-worker
-- asyncs being alive across this call.
runFollowSession
  :: AppTracer
  -> Text                                              -- ^ Log component
  -> IOManager
  -> HasqlSettings.Settings
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath                                          -- ^ socketPath
  -> IntersectionRequirement
  -> Maybe (IO ())                                     -- ^ mShutdown
  -> (Conn.Connection -> IdResolver IO -> Writer IO -> FollowEnv)
       -- ^ FollowEnv builder. Receives the just-opened Follow
       --   connection along with its resolver + writer.
  -> IO ()
runFollowSession
  tracer component iomgr hasqlSettings topLevelCfg networkMagic
  socketPath intersectReq mShutdown mkFollowEnv = do
    followCtrl <- openControlConnection hasqlSettings
    let followConn = unControlConnection followCtrl
    -- @synchronous_commit = off@: per-block COMMITs no longer wait on
    -- WAL fsync. Crash recovery is covered by chainsync replay from
    -- @last_committed_slot@.
    runAppM followConn (setFollowSessionGUCs defaultFollowTuning)
    resolver <- FollowResolver.mkFollowResolver followConn
    let writer    = FollowingWriter.mkWriter followConn
        followEnv = mkFollowEnv followConn resolver writer

        followAction =
          runAppM followEnv Follow.run
            `finally` do
              logInfoIO tracer component "Closing Follow hasql connection..."
              closeControlConnection followCtrl
                `catch` \(e :: SomeException) ->
                  logErrorIO tracer component $
                    "Error closing Follow connection: " <> show e

        -- When 'mShutdown' is provided, race it against the Follow
        -- loop so a test can stop the app cleanly.
        racedFollow = case mShutdown of
          Nothing      -> followAction
          Just waitSig -> void (race waitSig followAction)

    withAsync (runAppM followEnv $ connectToNode iomgr topLevelCfg networkMagic socketPath intersectReq) $ \nodeThread -> do
      link nodeThread
      racedFollow

-- | Resolve the chainsync intersection point for a Follow restart,
-- loading the ledger snapshot and computing the replay window when
-- the snapshot lags PG. Panics when every candidate snapshot is
-- orphaned in PG.
--
-- 'HasLedgerEnv' and 'frcMode' must agree (both ledger-enabled or
-- both ledger-disabled); this is established by 'decideBoot' running
-- against the same @ledger.enabled@ config that drove
-- 'mkHasLedgerEnv'. A mismatch is a programmer error.
prepareFollowRestart
  :: CoreEnv
  -> ControlConnection
  -> HasLedgerEnv
  -> StateQueryVar
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> FollowRestartContext
  -> IO FollowRestartStart
prepareFollowRestart coreEnv ctrl hasLE sqv topLevelCfg frc =
  case (hasLE, frcMode frc) of
    (LedgerDisabled _,   FollowRestartLedgerDisabled point) ->
      pure FollowRestartStart
        { frsIntersectPoint  = point
        , frsReplayBootSlot  = Nothing
        , frsReplayStartSlot = Nothing
        }
    (LedgerEnabled lenv, FollowRestartLedgerEnabled snaps) ->
      ledgerEnabledStart lenv snaps
    (LedgerEnabled _,    FollowRestartLedgerDisabled _) ->
      panic
        "Follow restart: ledger feature is enabled but the boot\
        \ decision produced a ledger-disabled context."
    (LedgerDisabled _,   FollowRestartLedgerEnabled _) ->
      panic
        "Follow restart: ledger feature is disabled but the boot\
        \ decision produced a ledger-enabled context."
  where
    row    = frcSyncState frc
    tracer = ceTracer coreEnv

    ledgerEnabledStart lenv snaps = do
      (snap, snapHash) <- pickValidatedSnapshot tracer ctrl snaps
      loadAndSeedSnapshot tracer lenv sqv topLevelCfg snap
      let snapSlot  = dsNumber snap
          snapPoint = mkCardanoPoint snapSlot snapHash
          mReplayBoot = case ssrLastCommittedSlot row of
            Just lastSlot | lastSlot > snapSlot -> Just (SlotNo lastSlot)
            _                                   -> Nothing
          mReplayStart = case mReplayBoot of
            Just _  -> Just (SlotNo snapSlot)
            Nothing -> Nothing
      case mReplayBoot of
        Just (SlotNo lastSlot) ->
          logInfoIO tracer "Boot" $
            "Snapshot lags PG by " <> show (lastSlot - snapSlot)
              <> " slots (snapshot=" <> show snapSlot
              <> ", last_committed_slot=" <> show lastSlot
              <> "); ledger will catch up via chainsync replay before"
              <> " Follow resumes PG writes."
        Nothing -> pure ()
      pure FollowRestartStart
        { frsIntersectPoint  = snapPoint
        , frsReplayBootSlot  = mReplayBoot
        , frsReplayStartSlot = mReplayStart
        }

-- | Walk the candidate snapshots newest-first; return the first one
-- whose slot has a matching @block.hash@ in PG, paired with that
-- hash. Logs each skipped orphan.
pickValidatedSnapshot
  :: AppTracer
  -> ControlConnection
  -> [DiskSnapshot]
  -> IO (DiskSnapshot, ByteString)
pickValidatedSnapshot tracer ctrl = go
  where
    go [] =
      panic
        "Follow restart: every candidate snapshot is orphaned in PG\
        \ (no matching row in the block table). The state directory\
        \ and PG database have drifted apart. Restart with\
        \ --resync-from-genesis."
    go (snap : rest) = do
      mHash <- runAppM ctrl (fetchBlockHashAtSlot (dsNumber snap))
      case mHash of
        Just h  -> pure (snap, h)
        Nothing -> do
          logInfoIO tracer "Boot" $
            "Snapshot at slot " <> show (dsNumber snap)
              <> " is orphaned in PG (no matching block row);\
                 \ trying older candidate"
          go rest

-- | Load a 'DiskSnapshot' into the in-memory 'LedgerDB' and seed the
-- HFC interpreter from the resulting ledger state. Emits a heartbeat
-- line every 15 s so a slow load doesn't read as a stalled boot.
-- Panics on load failure.
loadAndSeedSnapshot
  :: AppTracer
  -> LedgerEnv
  -> StateQueryVar
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> DiskSnapshot
  -> IO ()
loadAndSeedSnapshot tracer lenv sqv topLevelCfg snap = do
  logInfoIO tracer "Boot" $ "Loading ledger snapshot at slot " <> show snapSlot
  loadResult <-
    withHeartbeatIO tracer "LedgerSnapshot"
      ("still loading snapshot at slot " <> show snapSlot)
      heartbeatSeconds $
      runAppM lenv (initLedgerDbFromSnapshot snap)
  case loadResult of
    Left err -> panic $ "Failed to load ledger snapshot: " <> err
    Right () -> do
      loadedExt <- runAppM lenv readCurrentStateUnsafe
      seedInterpreterFromLedgerState topLevelCfg loadedExt sqv
  where
    snapSlot = dsNumber snap
    -- Heartbeat cadence tuned so a fast load emits no heartbeats
    -- while a slow one still gives the operator visibility within
    -- the first minute.
    heartbeatSeconds = 15
