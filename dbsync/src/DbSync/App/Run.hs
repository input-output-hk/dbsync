{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Top-level orchestration body.
--
-- 'runApp' takes pre-parsed configuration ('AppArgs') and drives the
-- full sync lifecycle: schema check, boot decision, ledger
-- initialisation, Ingest → Prep → Follow handoff, and the receiver
-- / watchdog / ledger-worker async tree.
--
-- The executable's @main@ is a thin shim that parses the CLI, reads
-- the on-disk files, then calls 'runApp'. The test harness builds
-- the same 'AppArgs' from a 'MockNode' and calls 'runApp' directly.
module DbSync.App.Run
  ( runApp
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (newTBQueueIO, newTVarIO)
import qualified Control.Concurrent.STM as STM
import Control.Tracer (traceWith)
import Data.IORef (newIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hasql.Connection.Settings as HasqlSettings
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Cardano.Network.NodeToClient (IOManager, withIOManager)
import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot (..), listSnapshots)
import Ouroboros.Network.Magic (NetworkMagic)

import DbSync.App (buildCoreEnv, runStartup)
import DbSync.App.Args (AppArgs (..))
import DbSync.AppM (runAppM)
import DbSync.Block.Types (CardanoPoint)
import DbSync.Checkpoint.Manager (mkResumeExtractState)
import DbSync.Checkpoint.Resume (CleanupMode (..), deleteRowsPastSlot)
import DbSync.Checkpoint.SyncState
  ( ControlConnection (..)
  , SyncStateRow (..)
  , closeControlConnection
  , fetchBlockHashAtSlot
  , markSyncComplete
  , openControlConnection
  , readSyncState
  , rebuildDedupMaps
  , seedSyncState
  )
import DbSync.Env (TracerWithConn (..), TracerWithControl (..))
import DbSync.Config.Genesis
  ( GenesisConfig (..)
  , ShelleyConfig (..)
  , mkProtocolInfoCardano
  , mkTopLevelConfig
  )
import DbSync.Config.Types
  ( DatabaseConfig (..)
  , LedgerConfig (..)
  , SyncConfig (..)
  )
import DbSync.Db.Loader (LoaderStream (..), closeLoaderStream, mkLoaderStream)
import DbSync.Db.Schema.Init
  ( SchemaAction (..)
  , checkSchemaVersions
  , decideSchemaAction
  , dropSchema
  , initSchema
  , renderSchemaMismatch
  , showWalLevel
  )
import DbSync.Env (CoreEnv (..), FollowEnv (..), IngestEnv (..), mkFollowEnvFromIngest)
import DbSync.Extractor (ExtractState, ExtractorDef (..), freshExtractState)
import DbSync.Id.DedupMap (newMaps)
import DbSync.Phase.Ingest.Consumer (runConsumer)
import DbSync.Phase.Ingest.ReceiverStats (newReceiverStats)
import DbSync.Ledger.Snapshot (runLedgerStateWriteThread)
import DbSync.Ledger.State
  ( dropLedgerStateDir
  , initLedgerDbFromGenesis
  , initLedgerDbFromSnapshot
  , mkHasLedgerEnv
  , readCurrentStateUnsafe
  )
import DbSync.Ledger.Types (HasLedgerEnv (..), LedgerEnv (..), mkNoLedgerEnv)
import DbSync.Ledger.Worker (runLedgerWorker)
import DbSync.Node.Connection (IntersectionRequirement (..), connectToNode, getNetworkMagic)
import qualified DbSync.Phase.Following.Run as Follow
import qualified DbSync.Phase.Following.Rollback as Rollback
import DbSync.Db.Schema.Types (TableDef)
import DbSync.App.Boot
  ( BootDecision (..)
  , FastPathContext (..)
  , ResumeContext (..)
  , ResumeIntersection (..)
  , decideBoot
  , mkCardanoPoint
  , renderBootError
  , resumeContextFrom
  )
import DbSync.Db.Phase (SyncPhase (..))
import DbSync.Phase.Current (setCurrentPhase)
import qualified DbSync.Phase.Preparing.Run as Prep
import DbSync.Phase.Preparing.Tuning (defaultPrepTuning)
import DbSync.Address.Buffer (newAddressBufferRef)
import DbSync.Address.Worker (awaitDrained, closeAddressResolver, mkAddressResolver)
import DbSync.Phase.Following.Resolver (mkFollowResolver)
import DbSync.Phase.Following.Tuning (defaultFollowTuning, setFollowSessionGUCs)
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.StateQuery (StateQueryVar, newStateQueryVar, seedInterpreterFromLedgerState)
import DbSync.Trace.Timing (withHeartbeatIO)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (Watchdog, newWatchdog, runWatchdogIO)
import qualified DbSync.Phase.Ingest.Writer as IngestWriter
import qualified DbSync.Phase.Following.Writer as FollowingWriter

-- | Run the full sync lifecycle. Returns when:
--
--   * 'FollowingChainTip' returns because @aaShutdownSignal@ fired
--     (test-only path); or
--   * a linked async crashes, propagating its exception out.
--
-- After Ingest exits at the rollback boundary, Prep runs to
-- completion and 'handoffToFollow' opens a fresh receiver feeding
-- the existing block queue. The ledger worker and snapshot writer
-- stay alive across the boundary; the snapshot cadence flips from
-- /every 10 epochs/ (Ingest) to /every epoch/ (Follow) via the
-- 'leConsistentWithTip' flag.
runApp :: AppTracer -> AppArgs -> IO ()
runApp tracer args = do
  let validProfile = aaProfile args
      nodeCfg      = aaNodeConfig args
      genesisCfg   = aaGenesisConfig args
      socketPath   = aaSocketPath args
      mShutdown    = aaShutdownSignal args
      logError msg = traceWith tracer $ LogMsg Error   "App" msg Nothing
      logWarn  msg = traceWith tracer $ LogMsg Warning "App" msg Nothing
      logInfo  msg = traceWith tracer $ LogMsg Info    "App" msg Nothing
      topLevelCfg  = mkTopLevelConfig nodeCfg genesisCfg
      networkMagic = getNetworkMagic genesisCfg
      network      = sgNetworkId (scConfig (gcShelley genesisCfg))

  -- 1. Shared core environment + startup logging
  coreEnv <- buildCoreEnv tracer validProfile nodeCfg network
  runAppM coreEnv runStartup

  let ledgerStateDir = aaLedgerStateDir args </> "dbsync-ledger"
  logInfo $ "Ledger state dir: " <> toS ledgerStateDir
  logInfo $ "Socket: " <> toS socketPath

  -- 2. State-query interpreter handle for SlotDetails computation.
  --    Tests against the mock node pre-seed this; production starts
  --    with an empty one and the receiver fills it from the live
  --    node's LocalStateQuery.
  stateQueryVar <- maybe (newStateQueryVar topLevelCfg) pure (aaStateQueryVar args)

  -- 3. Database connection settings from profile.
  let dbCfg   = scDatabase validProfile
      connStr = TE.encodeUtf8 $ "dbname=" <> dcName dbCfg
      hasqlSettings =
        mconcat
          [ HasqlSettings.hostAndPort (dcHost dbCfg) (fromIntegral (dcPort dbCfg))
          , HasqlSettings.user (dcUser dbCfg)
          , HasqlSettings.password (dcPassword dbCfg)
          , HasqlSettings.dbname (dcName dbCfg)
          ]

  -- 4. Schema check + (re)init.
  let extractors       = ceExtractors coreEnv
      tableDefs        = concatMap pdTables extractors
      versions         = map (\e -> (pdName e, pdVersion e)) extractors
      connStrTxt       = TE.decodeUtf8 connStr
      schemaVersion    = 1 :: Int
      ledgerEnabledCfg = lcEnabled (scLedger validProfile)
  schemaState <- checkSchemaVersions versions connStrTxt
  needsSeed <- case decideSchemaAction (aaResyncFromGenesis args) schemaState of
    ActionSkipInit -> do
      logInfo "Schema present and matches expected versions; skipping init"
      pure False
    ActionRunInit -> do
      logInfo "Fresh database detected; creating schema"
      initSchema tableDefs versions connStrTxt
      logInfo "Schema ready"
      pure True
    ActionForceReinit -> do
      logInfo "--resync-from-genesis: dropping existing schema and re-initialising"
      dropSchema tableDefs versions connStrTxt
      when ledgerEnabledCfg $ do
        logInfo $ "--resync-from-genesis: wiping ledger state directory " <> toS ledgerStateDir
        dropLedgerStateDir ledgerStateDir
      initSchema tableDefs versions connStrTxt
      logInfo "Schema ready"
      pure True
    ActionAbort errs -> do
      logError "Schema mismatch — refusing to start. Use --resync-from-genesis to wipe and re-sync."
      for_ errs $ \err -> logError $ "  - " <> renderSchemaMismatch err
      exitFailure

  -- 5. Open the consumer's control connection.
  consumerCtrlConn <- openControlConnection hasqlSettings
  when needsSeed $ do
    runAppM consumerCtrlConn (seedSyncState schemaVersion ledgerEnabledCfg)
    logInfo "Sync-state seeded"
    -- Fresh sync only: surface a misconfigured wal_level so the
    -- operator can flip it before Ingest starts. wal_level=minimal
    -- skips WAL on the UNLOGGED→LOGGED flip in PreparingForVolatileTail
    -- for tables over wal_skip_threshold. Not a blocker — managed
    -- PG operators may not control this GUC.
    walLevel <- showWalLevel connStrTxt
    unless (walLevel == "minimal") $
      logWarn $ T.unlines
        [ "Postgres wal_level is '" <> walLevel <> "'. For fastest bulk-load,"
        , "set the following in postgresql.conf and restart the server:"
        , "  wal_level = minimal"
        , "  max_wal_senders = 0"
        , "  archive_mode = off"
        , "See profiles/postgres-tuning.conf for the full snippet."
        , "Note: replicas will need a full re-base after reverting to"
        , "wal_level = replica. Acceptable on a one-time fresh sync."
        ]

  -- 6. SystemStart and ledger plumbing.
  let systemStart = SystemStart (sgSystemStart $ scConfig $ gcShelley genesisCfg)
      pinfo       = mkProtocolInfoCardano nodeCfg genesisCfg
      ledgerCfg   = scLedger validProfile
  hasLedgerEnv <-
    if lcEnabled ledgerCfg
      then do
        createDirectoryIfMissing True ledgerStateDir
        logInfo $ "Ledger feature enabled; opening LSM session under " <> toS ledgerStateDir
        snapCtrlConn <- openControlConnection hasqlSettings
        mkHasLedgerEnv
          tracer
          pinfo
          ledgerStateDir
          network
          (sgMaxLovelaceSupply (scConfig (gcShelley genesisCfg)))
          systemStart
          (lcSnapshotNearTipEpoch ledgerCfg)
          True
          False
          (lcBackend ledgerCfg)
          snapCtrlConn
          (ceCurrentPhase coreEnv)
      else do
        logInfo "Ledger feature disabled (set ledger.enabled = true in profile to opt in); skipping LSM session"
        LedgerDisabled <$> mkNoLedgerEnv tracer pinfo systemStart network

  -- 7. Boot decision.
  bootDecision <-
    if needsSeed
      then pure BootFresh
      else do
        mRow <- runAppM consumerCtrlConn readSyncState
        snapshots <- case hasLedgerEnv of
          LedgerEnabled lenv -> listSnapshots (leSnapshotManager lenv)
          LedgerDisabled _   -> pure []
        case decideBoot mRow snapshots ledgerEnabledCfg of
          Left bootErr -> do
            for_ (T.lines (renderBootError bootErr)) $ \line ->
              logError line
            exitFailure
          Right d -> pure d

  -- 8. Resolve the boot decision.
  --
  -- 'BootFollowingFastPath' does its entire run inline and returns
  -- 'Nothing'; the rest of runApp (which is the Ingest pipeline) then
  -- short-circuits. The other two branches return 'Just' with the
  -- initial state the Ingest setup needs.
  mIngestState <-
    case bootDecision of
      BootFresh -> do
        case hasLedgerEnv of
          LedgerEnabled lenv -> do
            logInfo "Seeding ledger DB from genesis"
            runAppM lenv initLedgerDbFromGenesis
          LedgerDisabled _ -> pure ()
        maps <- newMaps
        pure $ Just (mkInitState, maps, IntersectGenesis, Nothing, Nothing, 1)

      BootResume rc -> do
        let row = rcSyncState rc
        logInfo $
          "Resuming from slot "
            <> show (ssrLastCommittedSlot row)
            <> ", block "
            <> show (ssrLastCommittedBlockNo row)
        deleted <- runAppM consumerCtrlConn (deleteRowsPastSlot IngestResume tableDefs row)
        when (deleted > 0) $
          logInfo $
            "Cleaned up " <> show deleted
              <> " rows past last_committed_slot from a prior crash"
        logInfo "Rebuilding dedup maps from PG..."
        maps <- runAppM (TracerWithControl tracer consumerCtrlConn)
                  (rebuildDedupMaps tableDefs)

        (replayBs, replaySt) <- case (hasLedgerEnv, rcChosenSnapshot rc) of
          (LedgerDisabled _, _) -> pure (Nothing, Nothing)
          (LedgerEnabled lenv, Just snap) -> do
            logInfo $ "Loading ledger snapshot at slot " <> show (dsNumber snap)
            loadResult <-
              withHeartbeatIO tracer "LedgerSnapshot"
                ("still loading snapshot at slot " <> show (dsNumber snap))
                snapshotHeartbeatSeconds $
                runAppM lenv (initLedgerDbFromSnapshot snap)
            case loadResult of
              Left err -> panic $ "Failed to load ledger snapshot: " <> err
              Right () -> do
                loadedExt <- runAppM lenv readCurrentStateUnsafe
                seedInterpreterFromLedgerState topLevelCfg loadedExt stateQueryVar
                let startSlot = dsNumber snap
                for_ (ssrLastCommittedSlot row) $ \endSlot ->
                  when (endSlot > startSlot) $
                    logInfo $
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
          (LedgerEnabled _, Nothing) ->
            panic "BootResume (ledger enabled) returned without a chosen snapshot"

        ireq <- resolveIntersection logInfo logError consumerCtrlConn rc
        pure $ Just
          ( mkResumeExtractState row
          , maps
          , ireq
          , replayBs
          , replaySt
          , ssrAddressIdCounter row
          )

      BootFollowingFastPath fpc -> do
        runAppM coreEnv (setCurrentPhase (ceCurrentPhase coreEnv) FollowingVolatileTail)
        watchdog <- newWatchdog (ceMinSeverity coreEnv)
        withIOManager $ \iomgr ->
          withLedgerThreads hasLedgerEnv Nothing stateQueryVar watchdog $
            runFollowFastPath
              tracer logInfo logError hasqlSettings coreEnv topLevelCfg networkMagic
              socketPath systemStart stateQueryVar hasLedgerEnv
              consumerCtrlConn fpc mShutdown iomgr watchdog
        pure Nothing

  for_ mIngestState $ \(initialExtractState, dedupMaps, intersectReq, replayBoundary, replayStart, initialAddressId) -> do

    -- 9. Build the ingest pipeline state.
    stRef            <- newIORef initialExtractState
    loaderStream     <- mkLoaderStream connStr tableDefs
    blockQueue       <- newTBQueueIO 500
    receiverStats    <- newReceiverStats
    watchdog         <- newWatchdog (ceMinSeverity coreEnv)
    addrBuffer       <- newAddressBufferRef
    addrResolver     <- mkAddressResolver tracer hasqlSettings initialAddressId
    latestPointRef   <- newIORef Nothing
    rollbackBoundary <- newTVarIO Nothing

    let resolver = mkIngestResolver stRef dedupMaps addrBuffer
        writer   = IngestWriter.mkWriter loaderStream

    let ingestEnv = IngestEnv
          { ieCore                    = coreEnv
          , ieBlockQueue              = blockQueue
          , ieLoaderStream              = loaderStream
          , ieDedupMaps               = dedupMaps
          , ieAddressBuffer           = addrBuffer
          , ieAddressResolver         = addrResolver
          , ieHasLedgerEnv            = hasLedgerEnv
          , ieStateQueryVar           = stateQueryVar
          , ieSystemStart             = systemStart
          , ieResolver                = resolver
          , ieWriter                  = writer
          , ieExtractState            = stRef
          , ieReceiverStats           = receiverStats
          , ieControlConnection       = consumerCtrlConn
          , ieLastCommittedSlotAtBoot = replayBoundary
          , ieReplayStartSlot         = replayStart
          , ieWatchdog                = watchdog
          , ieLatestReceivedPoint     = latestPointRef
          , ieRollbackBoundary        = rollbackBoundary
          }

    logInfo "Starting block ingestion..."

    -- Cleanup of Ingest-only resources. Runs whether the consumer
    -- exits cleanly at the rollback boundary or aborts with an
    -- exception.
    let shutdownIngest = do
          logInfo "Shutting down loader stream..."
          lsCommit loaderStream `catch` \(e :: SomeException) ->
            logError $ "Error during final commit: " <> show e
          closeLoaderStream loaderStream
          logInfo "Draining address resolver..."
          awaitDrained addrResolver `catch` \(e :: SomeException) ->
            logError $ "Error draining address resolver: " <> show e
          logInfo "Stopping address resolver..."
          closeAddressResolver addrResolver
            `catch` \(e :: SomeException) ->
              logError $ "Error closing address resolver: " <> show e

        ingestAction = runAppM ingestEnv runConsumer `finally` shutdownIngest

        shutdownPostIngest = do
          logInfo "Closing consumer control connection..."
          closeControlConnection consumerCtrlConn
            `catch` \(e :: SomeException) ->
              logError $ "Error closing consumer control connection: " <> show e
          case hasLedgerEnv of
            LedgerEnabled lenv -> do
              logInfo "Closing LSM session..."
              leClose lenv `catch` \(e :: SomeException) ->
                logError $ "Error closing LSM session: " <> show e
              logInfo "Closing snapshot-writer control connection..."
              closeControlConnection (leControlConnection lenv)
                `catch` \(e :: SomeException) ->
                  logError $ "Error closing snapshot control connection: " <> show e
            LedgerDisabled _ -> pure ()

        runPrepAndMarkComplete =
          bracket (openControlConnection hasqlSettings) closeControlConnection $ \prepConn -> do
            runAppM coreEnv (setCurrentPhase (ceCurrentPhase coreEnv) PreparingForVolatileTail)
            let prepEnv = TracerWithConn tracer (unControlConnection prepConn)
            runAppM prepEnv (Prep.run hasqlSettings defaultPrepTuning tableDefs)
            runAppM prepConn markSyncComplete

    let mLedgerQueue = case hasLedgerEnv of
          LedgerEnabled lenv -> Just (leLedgerQueue lenv)
          LedgerDisabled _   -> Nothing

    withIOManager (\iomgr ->
      withLedgerThreads hasLedgerEnv replayBoundary stateQueryVar watchdog $
        withAsync (runWatchdogIO tracer watchdog blockQueue mLedgerQueue) $ \watchdogThread -> do
          link watchdogThread
          withAsync (runAppM ingestEnv $ connectToNode iomgr topLevelCfg networkMagic socketPath intersectReq) $ \nodeThread -> do
            link nodeThread
            ingestAction
            cancel nodeThread
          -- Ingest receiver cancelled. Ledger worker and snapshot
          -- writer stay alive across Prep and into Follow.
          runPrepAndMarkComplete
          runAppM coreEnv (setCurrentPhase (ceCurrentPhase coreEnv) FollowingVolatileTail)
          handoffToFollow
            iomgr ingestEnv logInfo logError hasqlSettings
            topLevelCfg networkMagic socketPath mShutdown
      ) `finally` shutdownPostIngest

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Turn a 'ResumeContext' into the receiver's intersection
-- requirement. Mirrors upstream cardano-db-sync's
-- @verifySnapshotPoint@: the snapshot supplies /the slot/, PG\'s
-- @block@ table is the oracle for /the hash/. Orphaned candidates
-- are dropped silently; panics when every candidate is orphaned.
resolveIntersection
  :: (Text -> IO ())
  -> (Text -> IO ())
  -> ControlConnection
  -> ResumeContext
  -> IO IntersectionRequirement
resolveIntersection logInfo logError ctrl rc = case rcIntersection rc of
  ReadyPoint p ->
    pure (IntersectAt [p])
  NeedsPgHashes slots -> do
    candidates <- catMaybes <$> for slots (resolveSlot logInfo ctrl)
    case candidates of
      [] -> do
        logError $
          "All " <> show (length slots) <> " snapshot intersection candidates "
            <> "are orphaned in PG (no matching row in the block table). "
            <> "Snapshot slots tried: " <> show slots <> ". "
            <> "Recovery: restore PG from a backup that covers one of these "
            <> "slots, or restart with --resync-from-genesis."
        panic "resolveIntersection: no usable snapshot intersection points"
      _ ->
        pure (IntersectAt candidates)

resolveSlot
  :: (Text -> IO ())
  -> ControlConnection
  -> Word64
  -> IO (Maybe CardanoPoint)
resolveSlot logInfo ctrl slot = do
  mHash <- runAppM ctrl (fetchBlockHashAtSlot slot)
  case mHash of
    Nothing -> do
      logInfo $
        "Snapshot at slot " <> show slot
          <> " has no matching row in the block table; "
          <> "skipping as a chainsync intersection candidate."
      pure Nothing
    Just h ->
      pure (Just (mkCardanoPoint slot h))

-- | Initial extraction state for IngestChainHistory.
mkInitState :: ExtractState
mkInitState = freshExtractState

-- | Run the ledger worker + snapshot-writer asyncs for the duration
-- of the inner action. No-op when the ledger feature is disabled.
--
-- The two threads share the caller's 'Watchdog', so a single sampler
-- covers their progress alongside the receiver / consumer bumps.
-- Cancellation propagates to both async children when the inner
-- action exits or raises.
withLedgerThreads
  :: HasLedgerEnv
  -> Maybe SlotNo
  -> StateQueryVar
  -> Watchdog
  -> IO a
  -> IO a
withLedgerThreads (LedgerDisabled _) _ _ _ inner = inner
withLedgerThreads hasLE@(LedgerEnabled lenv) replayBoundary sqv wd inner =
  withAsync (runAppM lenv (runLedgerWorker replayBoundary sqv wd)) $ \w -> do
    link w
    withAsync (runLedgerStateWriteThread hasLE) $ \s -> do
      link s
      inner

-- | In-process Ingest → Prep → Follow handoff.
--
-- Called after 'PreparingForVolatileTail' has marked sync complete.
-- Reads the just-written sync state, builds a 'ResumeContext', and
-- spawns a fresh chainsync receiver bound to a 'FollowEnv' that
-- reuses the Ingest receiver-side state (block queue, watchdog,
-- ledger queue, control connection) via 'mkFollowEnvFromIngest'.
-- Any blocks the Ingest receiver queued past the rollback boundary
-- before being cancelled stay in the queue and are processed by the
-- Follow consumer.
--
-- Returns when the shutdown signal fires (test path) or the Follow
-- loop / a linked async terminates.
handoffToFollow
  :: IOManager
  -> IngestEnv
  -> (Text -> IO ())
  -> (Text -> IO ())
  -> HasqlSettings.Settings
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath
  -> Maybe (IO ())
  -> IO ()
handoffToFollow
  iomgr ie logInfo logError hasqlSettings topLevelCfg networkMagic
  socketPath mShutdown = do
    let consumerCtrlConn = ieControlConnection ie
    mRow <- runAppM consumerCtrlConn readSyncState
    row <- case mRow of
      Just r -> pure r
      Nothing ->
        panic "handoffToFollow: dbsync_sync_state row missing after markSyncComplete"
    let resumeDesc = case (ssrLastCommittedSlot row, ssrLastCommittedBlockNo row) of
          (Just s, Just b) ->
            "at slot " <> show s <> ", block " <> show b
          _ -> "at genesis"
    logInfo $
      "Prep complete; handing off to FollowingChainTip " <> resumeDesc
    -- The Ingest receiver was cancelled before Prep ran. A new one
    -- opens below and re-intersects from the latestPointRef the
    -- previous receiver wrote. ChainSync will respond with a
    -- protocol-mandated confirming MsgRollBackward to that point —
    -- the receiver tags it 'confirming intersect; not propagated' and
    -- does not enqueue a MsgRollback, so no DB rows are deleted.
    logInfo $
      "Reconnecting chainsync at post-Ingest position; the\
      \ \"Rollback to …\" line that follows is the protocol's\
      \ confirming rollback to the chosen intersection point\
      \ — no rows are deleted from PG."

    -- The Ingest receiver kept reading from the node while the
    -- consumer was draining the rollback boundary; whatever it
    -- queued past the consumer's exit is still in the block queue
    -- and Follow's consumer will INSERT those first before the new
    -- receiver's stream catches up.
    buffered <- atomically $ STM.lengthTBQueue (ieBlockQueue ie)
    when (buffered > 0) $
      logInfo $
        "FollowingChainTip starting with "
          <> show buffered
          <> " block(s) buffered from Ingest's tail"

    let rc = resumeContextFrom row Nothing
    intersectReq <- resolveIntersection logInfo logError consumerCtrlConn rc

    followCtrl <- openControlConnection hasqlSettings
    let followConn = unControlConnection followCtrl
    -- @synchronous_commit = off@: per-block COMMITs no longer wait
    -- on WAL fsync. Crash recovery is covered by chainsync replay
    -- from @last_committed_slot@.
    runAppM followConn (setFollowSessionGUCs defaultFollowTuning)
    resolver <- mkFollowResolver followConn
    let writer     = FollowingWriter.mkWriter followConn
        followEnv  = mkFollowEnvFromIngest ie followConn resolver writer

        followAction =
          runAppM followEnv Follow.run
            `finally` do
              logInfo "Closing Follow hasql connection..."
              closeControlConnection followCtrl
                `catch` \(e :: SomeException) ->
                  logError $ "Error closing Follow connection: " <> show e

        racedFollow = case mShutdown of
          Nothing      -> followAction
          Just waitSig -> void (race waitSig followAction)

    -- The receiver runs under 'followEnv' so its watchdog / block
    -- queue / rollback boundary refs are the same ones IngestEnv
    -- carried. The Ingest receiver was cancelled before Prep ran;
    -- this opens a fresh one starting at the post-Ingest commit
    -- point.
    withAsync (runAppM followEnv $ connectToNode iomgr topLevelCfg networkMagic socketPath intersectReq) $ \nodeThread -> do
      link nodeThread
      racedFollow

-- | Boot directly into 'FollowingChainTip' on a restart after Prep
-- has already marked sync complete. Builds the Follow state from
-- scratch (no 'IngestEnv' is in scope) and runs the receiver +
-- Follow loop. The caller is responsible for opening the
-- 'IOManager' and any ledger threads ('withLedgerThreads').
--
-- When ledger is enabled, the on-disk snapshot is the authoritative
-- restart point. The flow:
--
--   1. Walk the candidate snapshots newest-first; pick the first
--      whose slot has a matching @block.hash@ in PG.
--   2. Load that snapshot into the in-memory 'LedgerDB'.
--   3. If the snapshot's slot is below @last_committed_slot@, the
--      asynchronous snapshot writer fell behind the consumer's
--      commits before shutdown. Roll PG back to the snapshot's
--      point using the same cascade chain reorgs use; this brings
--      PG and ledger into alignment.
--   4. Intersect chainsync at the (now-aligned) restart point.
--
-- When ledger is disabled there is no snapshot to load; chainsync
-- intersects directly at the row's @last_committed_*@.
--
-- When the optional shutdown signal fires, the Follow loop is
-- cancelled and this returns normally; otherwise it blocks forever
-- (production behaviour).
runFollowFastPath
  :: AppTracer
  -> (Text -> IO ())
  -> (Text -> IO ())
  -> HasqlSettings.Settings
  -> CoreEnv
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath
  -> SystemStart
  -> StateQueryVar
  -> HasLedgerEnv
  -> ControlConnection
  -> FastPathContext
  -> Maybe (IO ())
  -> IOManager
  -> Watchdog
  -> IO ()
runFollowFastPath
  tracer logInfo logError hasqlSettings coreEnv topLevelCfg networkMagic
  socketPath systemStart stateQueryVar hasLedgerEnv consumerCtrlConn fpc
  mShutdown iomgr watchdog = do

    logInfo "Boot: sync_complete=true; entering FollowingVolatileTail"

    let row = fpcSyncState fpc
        tableDefs = concatMap pdTables (ceExtractors coreEnv)
    -- 'FollowRestart' mode skips the dedup-counter DELETE. The counter
    -- columns on 'SyncStateRow' are frozen at Ingest's last
    -- pending-boundary snapshot; running them here would wipe every
    -- dedup row Ingest's last two epochs and Follow wrote, silently
    -- orphaning the fact-table FKs that reference them.
    deleted <- runAppM consumerCtrlConn (deleteRowsPastSlot FollowRestart tableDefs row)
    when (deleted > 0) $
      logInfo $
        "Cleaned up " <> show deleted
          <> " rows past last_committed_slot from a prior Follow crash"

    -- Pick the chainsync restart point. When ledger is enabled, this
    -- also loads the chosen snapshot and rolls PG back to it if it
    -- lags @last_committed_slot@.
    intersectPoint <- prepareFastPathStart
      tracer logInfo consumerCtrlConn tableDefs
      hasLedgerEnv stateQueryVar topLevelCfg fpc

    followCtrl <- openControlConnection hasqlSettings
    let followConn = unControlConnection followCtrl
    -- @synchronous_commit = off@: per-block COMMITs no longer wait
    -- on WAL fsync. Crash recovery is covered by chainsync replay
    -- from @last_committed_slot@.
    runAppM followConn (setFollowSessionGUCs defaultFollowTuning)

    blockQueue       <- newTBQueueIO 500
    receiverStats    <- newReceiverStats
    latestPointRef   <- newIORef Nothing
    rollbackBoundary <- newTVarIO Nothing

    resolver <- mkFollowResolver followConn
    let writer    = FollowingWriter.mkWriter followConn
        followEnv =
          FollowEnv
            { feCore                = coreEnv
            , feBlockQueue          = blockQueue
            , feHasLedgerEnv        = hasLedgerEnv
            , feStateQueryVar       = stateQueryVar
            , feSystemStart         = systemStart
            , feReceiverStats       = receiverStats
            , feWatchdog            = watchdog
            , feLatestReceivedPoint = latestPointRef
            , feHasqlConnection     = followConn
            , feResolver            = resolver
            , feWriter              = writer
            , feControlConnection   = consumerCtrlConn
            , feRollbackBoundary    = rollbackBoundary
            }

    let intersectReq = IntersectAt [intersectPoint]

        mLedgerQueue = case hasLedgerEnv of
          LedgerEnabled lenv -> Just (leLedgerQueue lenv)
          LedgerDisabled _   -> Nothing

        followAction =
          runAppM followEnv Follow.run
            `finally` do
              logInfo "Closing Follow hasql connection..."
              closeControlConnection followCtrl
                `catch` \(e :: SomeException) ->
                  logError $ "Error closing Follow connection: " <> show e

        -- When 'mShutdown' is provided, race it against the Follow
        -- loop so a test can stop the app cleanly.
        racedFollow = case mShutdown of
          Nothing      -> followAction
          Just waitSig -> void (race waitSig followAction)

    withAsync (runWatchdogIO tracer watchdog blockQueue mLedgerQueue) $ \watchdogThread -> do
      link watchdogThread
      withAsync (runAppM followEnv $ connectToNode iomgr topLevelCfg networkMagic socketPath intersectReq) $ \nodeThread -> do
        link nodeThread
        racedFollow

-- | Cadence between snapshot-load heartbeat lines. Tuned so a fast
-- load doesn't emit any heartbeats while a slow one still gives the
-- operator visibility within the first minute.
snapshotHeartbeatSeconds :: Int
snapshotHeartbeatSeconds = 15

-- | Resolve the chainsync intersection point for the fast-path
-- restart, loading the ledger snapshot and rolling PG back to it
-- when needed.
--
-- Ledger disabled: returns the row's last-committed point.
--
-- Ledger enabled:
--
--   * Walks 'fpcCandidateSnapshots' newest-first, picking the first
--     whose slot has a matching @block.hash@ in PG. Orphaned
--     candidates (snapshot exists but PG has no block at that slot)
--     are skipped with a log line.
--   * Loads the chosen snapshot into the in-memory 'LedgerDB' and
--     seeds the HFC interpreter from the resulting ledger state.
--   * If the snapshot's slot is below @last_committed_slot@, runs
--     'Rollback.rollbackToPoint' on the snapshot's point. The
--     cascade DELETEs every row past the point and advances
--     @dbsync_sync_state.last_committed_*@ in one PG transaction,
--     leaving the database aligned with the ledger.
--
-- Panics when every candidate is orphaned in PG — the
-- state-directory and PG database have drifted apart and the
-- operator's recovery is @--resync-from-genesis@.
prepareFastPathStart
  :: AppTracer
  -> (Text -> IO ())
  -> ControlConnection
  -> [TableDef]
  -> HasLedgerEnv
  -> StateQueryVar
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> FastPathContext
  -> IO CardanoPoint
prepareFastPathStart tracer logInfo ctrl tableDefs hasLE sqv topLevelCfg fpc =
  case hasLE of
    LedgerDisabled _ -> ledgerDisabledStart
    LedgerEnabled lenv -> ledgerEnabledStart lenv
  where
    row = fpcSyncState fpc

    ledgerDisabledStart =
      case (ssrLastCommittedSlot row, ssrLastCommittedBlockHash row) of
        (Just s, Just h) ->
          pure (mkCardanoPoint s h)
        _ ->
          panic
            "Follow fast-path: ledger disabled but dbsync_sync_state has\
            \ no (slot, hash). The boot decision should have rejected\
            \ this earlier; this is an internal invariant violation."

    ledgerEnabledStart lenv = do
      (snap, snapHash) <- pickValidatedSnapshot logInfo ctrl (fpcCandidateSnapshots fpc)
      let snapSlot  = dsNumber snap
          snapPoint = mkCardanoPoint snapSlot snapHash
      logInfo $ "Loading ledger snapshot at slot " <> show snapSlot
      loadResult <-
        withHeartbeatIO tracer "LedgerSnapshot"
          ("still loading snapshot at slot " <> show snapSlot)
          snapshotHeartbeatSeconds $
          runAppM lenv (initLedgerDbFromSnapshot snap)
      case loadResult of
        Left err -> panic $ "Failed to load ledger snapshot: " <> err
        Right () -> do
          loadedExt <- runAppM lenv readCurrentStateUnsafe
          seedInterpreterFromLedgerState topLevelCfg loadedExt sqv
      -- Rollback PG to the snapshot's point when the snapshot lags
      -- the last-committed slot. The snapshot writer is asynchronous
      -- and can fall behind the consumer at shutdown; we re-align by
      -- deleting the committed rows the ledger doesn't know about.
      case ssrLastCommittedSlot row of
        Just lastSlot | lastSlot > snapSlot -> do
          logInfo $
            "Rolling back PG from slot " <> show lastSlot
              <> " to snapshot slot " <> show snapSlot
              <> " (" <> show (lastSlot - snapSlot)
              <> " slots) to align with ledger state"
          let rollbackEnv = TracerWithConn tracer (unControlConnection ctrl)
          runAppM rollbackEnv (Rollback.rollbackToPoint tableDefs snapPoint)
        _ -> pure ()
      pure snapPoint

-- | Walk the candidate snapshots newest-first. Return the first one
-- whose slot has a matching @block.hash@ in PG, paired with that
-- hash. Logs each skipped orphan.
pickValidatedSnapshot
  :: (Text -> IO ())
  -> ControlConnection
  -> [DiskSnapshot]
  -> IO (DiskSnapshot, ByteString)
pickValidatedSnapshot logInfo ctrl = go
  where
    go [] =
      panic
        "Follow fast-path: every candidate snapshot is orphaned in PG\
        \ (no matching row in the block table). The state directory\
        \ and PG database have drifted apart. Restart with\
        \ --resync-from-genesis."
    go (snap : rest) = do
      mHash <- runAppM ctrl (fetchBlockHashAtSlot (dsNumber snap))
      case mHash of
        Just h  -> pure (snap, h)
        Nothing -> do
          logInfo $
            "Snapshot at slot " <> show (dsNumber snap)
              <> " is orphaned in PG (no matching block row);\
                 \ trying older candidate"
          go rest
