{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Boot lifecycle: a pure decision plus its effectful resolution.
--
-- 'decideBoot' classifies the sync-state row and the on-disk
-- snapshots into a 'BootDecision'. The resolve helpers turn that
-- decision into an 'IngestBootState', or run the Follow loop inline
-- on a 'BootFollowRestart'. Mismatches surface as 'BootError'.
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
import Ouroboros.Network.Block (data BlockPoint, data GenesisPoint)
import Ouroboros.Network.Magic (NetworkMagic (..))

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
  , networkNameFromMagic
  )
import DbSync.Db.Schema.SyncState (SyncStateRow (..))
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Extractor (ExtractState (..), ExtractorDef (..), freshExtractState)
import DbSync.Parser.Types (CardanoPoint)
import DbSync.Phase.Current (setCurrentPhase)
import DbSync.Phase.Following.Resolver (ConsumedTracking (..))
import qualified DbSync.Phase.Following.Resolver as FollowResolver
import qualified DbSync.Phase.Following.Run as Follow
import qualified DbSync.Phase.Following.Rollback as Rollback
import DbSync.Phase.Following.Tuning (defaultFollowTuning, setFollowSessionGUCs)
import qualified DbSync.Phase.Following.Writer as FollowingWriter
import DbSync.Phase.Ingest.DedupStore (DedupStores, newStores)
import DbSync.Phase.Ingest.LsmSession (LsmSession, closeLsmSession)
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
  , populateGovActionProposalCache
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
import DbSync.Worker.Ledger.Fingerprint (LedgerStateFingerprint, renderFingerprint)
import DbSync.Worker.Ledger.Snapshot (deleteNewerSnapshots)
import DbSync.Worker.Ledger.State
  ( initLedgerDbFromGenesis
  , initLedgerDbFromSnapshot
  , readCurrentStateUnsafe
  )
import DbSync.Worker.Ledger.Types (HasLedgerEnv (..), LedgerEnv (..))
import DbSync.Worker.Ledger.Worker (withLedgerThreads)
import DbSync.Worker.OffChain.Pool (closeOffChainPoolWorker)
import DbSync.Worker.OffChain.Vote (closeOffChainVoteWorker)
import DbSync.App.Setup (setupOffChainPoolWorker, setupOffChainVoteWorker)
import DbSync.App.Config.Types
  ( Extractors (..)
  , SyncConfig (..)
  , UtxoOption (..)
  )

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

-- | What a resume from a 'SyncStateRow' needs.
--
-- Invariant: when 'rcChosenSnapshot' is 'Just', its 'dsNumber' equals
-- the head of the @NeedsPgHashes@ list in 'rcIntersection'. That
-- snapshot restores the in-RAM ledger and is also the preferred
-- intersection point offered to the node.
data ResumeContext = ResumeContext
  { rcSyncState      :: !SyncStateRow
  , rcChosenSnapshot :: !(Maybe DiskSnapshot)
  , rcIntersection   :: !ResumeIntersection
  }
  deriving stock (Eq, Show)

-- | What entering Follow on a restart needs, after Ingest and Prep
-- completed (@sync_complete = true@). 'frcSyncState' carries the raw
-- row for diagnostics.
data FollowRestartContext = FollowRestartContext
  { frcSyncState :: !SyncStateRow
  , frcMode      :: !FollowRestartMode
  }
  deriving stock (Eq, Show)

-- | What the Follow-restart path knows about the chainsync resume
-- point before 'prepareFollowRestart' runs.
--
-- Ledger disabled: PG is the single source of truth, so the decision
-- step pre-builds the point from the @last_committed_*@ columns.
--
-- Ledger enabled: the decision step hands over candidate snapshots.
-- The orchestrator walks them newest-first and takes the first whose
-- slot has a matching @block.hash@ in PG. That snapshot restores the
-- ledger, the gap to @last_committed_slot@ becomes the replay window,
-- and chainsync intersects at the snapshot's point.
data FollowRestartMode
  = FollowRestartLedgerDisabled !CardanoPoint
    -- ^ Pre-validated intersection point.
  | FollowRestartLedgerEnabled ![DiskSnapshot]
    -- ^ Non-empty candidate list, newest-first.
  deriving stock (Eq, Show)

-- | How the chainsync intersection points are produced.
--
-- A ledger-disabled resume hands the receiver a complete point from
-- the @last_committed_*@ columns. A ledger-enabled resume cannot: a
-- 'DiskSnapshot' knows only its slot, and PG's last-committed hash
-- belongs to a different block. So this nominates slots and the
-- caller asks PG for the canonical hash at each. Mirrors upstream
-- cardano-db-sync's @verifySnapshotPoint@.
data ResumeIntersection
  = ReadyPoint !CardanoPoint
    -- ^ Ledger disabled: @(slot, hash)@ from the 'SyncStateRow'.
  | NeedsPgHashes ![Word64]
    -- ^ Ledger enabled: candidate snapshot slots, newest-first. The
    -- head restores the ledger; the rest are fallbacks. The caller
    -- resolves each against PG and drops orphans silently.
  deriving stock (Eq, Show)

-- | State the boot resolution hands to the Ingest pipeline setup.
data IngestBootState = IngestBootState
  { ibsInitialExtractState :: !ExtractState
  , ibsDedupStores         :: !DedupStores
    -- ^ Freshly opened on 'BootFresh', rebuilt from PG on
    -- 'BootResume'.
  , ibsIntersection        :: !IntersectionRequirement
  , ibsReplayBoundary      :: !(Maybe SlotNo)
    -- ^ Upper edge of the replay window. The consumer skips
    -- 'processBlock' at or below this slot. 'Nothing' when the ledger
    -- is off, or when the snapshot already aligns with PG.
  , ibsReplayStart         :: !(Maybe SlotNo)
    -- ^ Lower edge of the replay window: the chosen snapshot's slot.
    -- 'Just' exactly when 'ibsReplayBoundary' is.
  , ibsAddressIdCounter    :: !Int64
    -- ^ Initial @address.id@ for the 'TxOutWorker'. Read from the
    -- sync-state row on resume, 1 on a fresh boot.
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
  | BootSchemaNewerThanBinary !Int !Int
    -- ^ The database's @schema_version_applied@ exceeds the version this
    -- binary targets. Fields: @(database, binary)@.
  | BootSchemaDriftUncovered !Text !Text
    -- ^ Versions match but the stored @schema_fingerprint@ differs from the
    -- declared one. Fields: @(stored, declared)@.
  | BootNetworkMismatch !NetworkMagic !NetworkMagic
    -- ^ The database's recorded @network_magic@ differs from the magic in
    -- the configured genesis. Fields: @(database, config)@.
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Decision
-- ---------------------------------------------------------------------------

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

-- | Build the 'BootFollowRestart' decision for @sync_complete = true@.
--
-- A row missing @last_committed_slot@ or @last_committed_block_hash@
-- is malformed and becomes 'BootSyncStateMissing'. With the ledger
-- enabled the failure cases match 'BootResume': an empty snapshot
-- directory gives 'BootResumeStateMissing', and candidates that all
-- sit beyond the committed slot give 'BootNoUsableSnapshot'.
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

rowHasNoCommittedProgress :: SyncStateRow -> Bool
rowHasNoCommittedProgress r =
  isNothing (ssrLastCommittedSlot r)
    && isNothing (ssrLastCommittedBlockNo r)
    && isNothing (ssrLastCommittedBlockHash r)

-- | Build a 'ResumeContext' with no chosen snapshot. Serves the
-- in-process Ingest → Prep → Follow handoff, where chainsync
-- intersects at the row's last committed block and the receiver has
-- already aligned the ledger with PG.
resumeContextFrom :: SyncStateRow -> Maybe DiskSnapshot -> ResumeContext
resumeContextFrom row mSnap =
  ResumeContext
    { rcSyncState     = row
    , rcChosenSnapshot = mSnap
    , rcIntersection  = case (ssrLastCommittedSlot row, ssrLastCommittedBlockHash row) of
        (Just s, Just h) -> ReadyPoint (mkCardanoPoint s h)
        _                -> ReadyPoint GenesisPoint
    }

-- | Snapshots at or before @lastSlot@. Relies on the newest-first
-- order that @listSnapshots@ returns.
candidateSnapshotSlots :: [DiskSnapshot] -> Word64 -> [DiskSnapshot]
candidateSnapshotSlots snaps lastSlot =
  filter (\ds -> dsNumber ds <= lastSlot) snaps

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

mkCardanoPoint
  :: Word64
  -> ByteString   -- ^ 32-byte block header hash
  -> CardanoPoint
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

  BootSchemaNewerThanBinary database binary ->
    T.unlines
      [ "Cannot start: the database schema is newer than this binary supports."
      , ""
      , "  Database schema version : " <> show database
      , "  Binary schema version   : " <> show binary
      , ""
      , "Recovery: upgrade dbsync to a build that targets this schema version."
      ]

  BootSchemaDriftUncovered stored declared ->
    T.unlines
      [ "Cannot start: the database schema fingerprint does not match the"
      , "declared schema, and no migration covers the difference."
      , ""
      , "  Stored   : " <> stored
      , "  Declared : " <> declared
      , ""
      , "Recovery: add a migration that raises the schema version to cover the"
      , "change, or restart with --resync-from-genesis to rebuild from genesis."
      ]

  BootNetworkMismatch database config ->
    T.unlines
      [ "Cannot resume: the database was synced against a different network."
      , ""
      , "  Database : " <> renderNetwork database
      , "  This run : " <> renderNetwork config
      , ""
      , "Resuming would interleave two chains. Check --node-config: it must"
      , "point at the config.json of the network this database was synced"
      , "against. To sync the configured network instead, use a fresh database"
      , "or restart with --resync-from-genesis to wipe and re-sync."
      ]
    where
      renderNetwork m =
        networkNameFromMagic m <> " (magic " <> show (unNetworkMagic m) <> ")"

-- ---------------------------------------------------------------------------
-- * Lifecycle
-- ---------------------------------------------------------------------------

-- | Never returns.
abortBoot :: AppTracer -> BootError -> IO a
abortBoot tracer err = do
  for_ (T.lines (renderBootError err)) (logErrorIO tracer "Boot")
  exitFailure

-- | Apply a rollback request that arrived before the normal boot.
--
-- The CLI flag wins over the on-DB marker. On success this clears the
-- marker, so the next boot does not repeat the rollback. It also
-- drops ledger snapshots newer than the target, which keeps the next
-- boot's snapshot picker aligned with the rolled-back chain.
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

-- | Seed the ledger DB from genesis when the ledger is on, open the
-- dedup stores, and start chainsync at 'IntersectGenesis'.
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
-- Deletes rows past @last_committed_slot@ left by a prior crash,
-- rebuilds the dedup stores from PG, fills the cost-model cache, and
-- — with the ledger on — loads the chosen snapshot and seeds the HFC
-- interpreter. When the snapshot lags PG it sets the replay window,
-- so the consumer suppresses its PG-write path until the ledger
-- worker re-applies the gap.
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
    stores   <- runAppM envCtrl (rebuildDedupMaps tableDefs lsmSession)
    cmCache  <- runAppM envCtrl (populateCostModelCache tableDefs)
    gapCache <- runAppM envCtrl (populateGovActionProposalCache tableDefs)

    (replayBoundary, replayStart) <-
      resolveResumeReplay tracer topLevelCfg stateQueryVar hasLedgerEnv row rc

    intersection <- resolveIntersection tracer consumerCtrlConn rc
    let resumeState = (mkResumeExtractState row)
          { esCostModelCache         = cmCache
          , esGovActionProposalCache = gapCache
          }
    pure IngestBootState
      { ibsInitialExtractState = resumeState
      , ibsDedupStores         = stores
      , ibsIntersection        = intersection
      , ibsReplayBoundary      = replayBoundary
      , ibsReplayStart         = replayStart
      , ibsAddressIdCounter    = ssrAddressIdCounter row
      }

-- | Load the resume snapshot when the ledger is on, then compute the
-- replay window.
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
-- requirement. The snapshot supplies the slot; PG's @block@ table is
-- the oracle for the hash. Orphaned candidates drop out silently.
-- Panics when every candidate is orphaned.
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
-- point, plus the replay window edges when the on-disk snapshot lags
-- PG. Both edges are 'Just' together or 'Nothing' together.
data FollowRestartStart = FollowRestartStart
  { frsIntersectPoint  :: !CardanoPoint
  , frsReplayBootSlot  :: !(Maybe SlotNo)
  , frsReplayStartSlot :: !(Maybe SlotNo)
  }

-- | Gap size above which a Follow restart suggests
-- @--resync-from-genesis@ instead of a per-block replay.
resumeGapWarnBlocks :: Word64
resumeGapWarnBlocks = 500_000

-- | Warn when the resume point sits more than 'resumeGapWarnBlocks'
-- behind the node tip.
--
-- The receiver publishes @nodeTip - k@ on every 'MsgRollForward', so
-- this recovers the tip as @boundary + k@. It blocks until the
-- receiver publishes its first boundary, and returns silently when
-- the row carries no block number or the gap is below the threshold.
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

-- | Boot straight into Follow on a restart where Prep already marked
-- sync complete. Releases the Ingest LSM session, flips the phase,
-- and runs the Follow loop under the ledger-worker bracket.
--
-- With the ledger enabled the on-disk snapshot is the authoritative
-- restart point:
--
--   1. Walk the candidate snapshots newest-first. Take the first
--      whose slot has a matching @block.hash@ in PG.
--   2. Load that snapshot into the in-memory 'LedgerDB'.
--   3. When the snapshot slot is below @last_committed_slot@ — the
--      async snapshot writer trailed the consumer at shutdown —
--      configure a replay window. The ledger worker re-applies the
--      gap while Follow's consumer skips its PG-write path.
--   4. Start the ledger worker and intersect chainsync at the
--      snapshot's point.
--
-- With the ledger disabled there is no snapshot, so chainsync
-- intersects at the row's @last_committed_*@.
--
-- This returns when the shutdown signal fires; otherwise it blocks
-- forever.
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
    -- The Follow loop never touches the ingest LSM tables. Release the
    -- session so the directory lock drops before the long-running loop.
    closeLsmSession lsmSession
    runAppM coreEnv (setCurrentPhase (ceCurrentPhase coreEnv) FollowingVolatileTail)

    let row = frcSyncState frc
        tableDefs = concatMap pdTables (ceExtractors coreEnv)
    -- 'FollowRestart' mode skips the dedup-counter DELETE. The counter
    -- columns on 'SyncStateRow' are frozen at Ingest's last
    -- pending-boundary snapshot. Running the DELETE here would wipe
    -- every dedup row that Ingest's last two epochs and Follow wrote,
    -- and silently orphan the fact-table FKs that reference them.
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
      -- Inside the replay window the worker suppresses snapshot writes
      -- and 'accumulateEpochParams', because PG and disk already hold
      -- those epochs.
      withLedgerThreads hasLedgerEnv mReplayBoot stateQueryVar $
        bracket
          (setupOffChainPoolWorker tracer hasqlSettings (scExtractors (ceConfig coreEnv)))
          (mapM_ closeOffChainPoolWorker) $ \mPoolWorker ->
        bracket
          (setupOffChainVoteWorker tracer hasqlSettings (scExtractors (ceConfig coreEnv)))
          (mapM_ closeOffChainVoteWorker) $ \mVoteWorker -> do
          -- Fresh receiver state: this restart path bypasses Ingest, so
          -- there is no upstream env to inherit from. The queue depth
          -- matches the Ingest path in App.Run, which carries the
          -- sizing rationale.
          blockQueue       <- newTBQueueIO 300
          latestPointRef   <- newTVarIO Nothing
          rollbackBoundary <- newTVarIO Nothing
          latestTipBlock   <- newTVarIO Nothing

          let intersectReq = IntersectAt [intersectPoint]
              mkEnv conn resolver writer =
                FollowEnv
                  { feCore                = coreEnv
                  , feBlockQueue          = blockQueue
                  , feHasLedgerEnv        = hasLedgerEnv
                  , feStateQueryVar       = stateQueryVar
                  , feSystemStart         = systemStart
                  , feLatestReceivedPoint = latestPointRef
                  , feHasqlConnection     = conn
                  , feResolver            = resolver
                  , feWriter              = writer
                  , feControlConnection   = consumerCtrlConn
                  , feRollbackBoundary    = rollbackBoundary
                  , feLatestTipBlock      = latestTipBlock
                  , feReplayBootSlot      = mReplayBoot
                  , feReplayStartSlot     = mReplayStart
                  , feOffChainPoolWorker  = mPoolWorker
                  , feOffChainVoteWorker  = mVoteWorker
                  }

          let mLastBlock = ssrLastCommittedBlockNo (frcSyncState frc)
              kBlocks    = ceSecurityParam coreEnv
              consumedTracking =
                if uoConsumedByTxId (exUtxo (scExtractors (ceConfig coreEnv)))
                  then TrackConsumedBy
                  else SkipConsumedBy
          withAsync (checkResumeGap tracer kBlocks mLastBlock rollbackBoundary) $ \gapThread -> do
            link gapThread
            runFollowSession tracer "Boot" iomgr hasqlSettings topLevelCfg
              networkMagic socketPath intersectReq consumedTracking mShutdown mkEnv

-- | Open a dedicated Follow hasql connection, build its resolver and
-- writer, pass them to the caller's 'FollowEnv' builder, and run
-- 'Follow.run' against the result. The chainsync receiver runs as a
-- linked async for the duration.
--
-- Shared by 'runBootFollowRestart' and 'handoffToFollow'. Each caller
-- must keep the ledger-worker asyncs alive across this call.
runFollowSession
  :: AppTracer
  -> Text                                              -- ^ Log component
  -> IOManager
  -> HasqlSettings.Settings
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath                                          -- ^ socketPath
  -> IntersectionRequirement
  -> ConsumedTracking
  -> Maybe (IO ())                                     -- ^ mShutdown
  -> (Conn.Connection -> IdResolver IO -> Writer IO -> FollowEnv)
       -- ^ Receives the just-opened Follow connection with its
       --   resolver and writer.
  -> IO ()
runFollowSession
  tracer component iomgr hasqlSettings topLevelCfg networkMagic
  socketPath intersectReq consumedTracking mShutdown mkFollowEnv = do
    followCtrl <- openControlConnection hasqlSettings
    let followConn = unControlConnection followCtrl
    -- @synchronous_commit = off@: a per-block COMMIT does not wait on
    -- the WAL fsync. Chainsync replay from @last_committed_slot@
    -- covers crash recovery.
    runAppM followConn (setFollowSessionGUCs defaultFollowTuning)
    resolver <- FollowResolver.mkFollowResolver followConn consumedTracking
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

        racedFollow = case mShutdown of
          Nothing      -> followAction
          Just waitSig -> void (race waitSig followAction)

    withAsync (runAppM followEnv $ connectToNode iomgr topLevelCfg networkMagic socketPath intersectReq) $ \nodeThread -> do
      link nodeThread
      racedFollow

-- | Resolve the chainsync intersection point for a Follow restart. It
-- loads the ledger snapshot and computes the replay window when the
-- snapshot lags PG. Panics when every candidate snapshot is orphaned.
--
-- 'HasLedgerEnv' and 'frcMode' must agree on whether the ledger is
-- enabled. 'decideBoot' guarantees that, because it reads the same
-- @ledger.enabled@ config that drove 'mkHasLedgerEnv'. A mismatch is
-- a programmer error.
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

-- | Walk the candidates newest-first and return the first whose slot
-- has a matching @block.hash@ in PG, paired with that hash.
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
-- HFC interpreter from the resulting ledger state. Panics when the
-- load fails.
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
    -- Long enough that a fast load stays silent, short enough that a
    -- slow load does not read as a stalled boot.
    heartbeatSeconds = 15
