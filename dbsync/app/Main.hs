module Main
  ( main
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (newTBQueueIO)
import Control.Tracer (traceWith)
import Data.IORef (newIORef)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import DbSync.App (buildCoreEnv, runStartup)
import DbSync.AppM (runAppM)
import DbSync.Block.Types (CardanoPoint)
import DbSync.Checkpoint.Manager (mkResumeExtractState)
import DbSync.Checkpoint.Resume (deleteRowsPastSlot)
import DbSync.Checkpoint.SyncState
  ( ControlConnection
  , SyncStateRow (..)
  , closeControlConnection
  , fetchBlockHashAtSlot
  , openControlConnection
  , readSyncState
  , rebuildDedupMaps
  , seedSyncState
  )
import DbSync.Cli (parseCliArgs, CliArgs (..))
import DbSync.Config (parseConfig)
import DbSync.Config.Genesis (readCardanoGenesisConfig, mkProtocolInfoCardano, mkTopLevelConfig, GenesisConfig (..), ShelleyConfig (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import DbSync.Config.Node (parseDbSyncNodeConfig, parseNodeConfig)
import DbSync.Config.Types
  ( DatabaseConfig (..)
  , DbSyncNodeConfig (..)
  , LedgerConfig (..)
  , SyncConfig (..)
  )
import qualified Hasql.Connection.Settings as HasqlSettings
import DbSync.Config.Validation (validateConfig)
import DbSync.Copy.Writer (CopyWriter (..), mkCopyWriter, closeCopyWriter)
import DbSync.Db.Schema.Init
  ( SchemaAction (..)
  , checkSchemaVersions
  , decideSchemaAction
  , dropSchema
  , initSchema
  , renderSchemaMismatch
  )
import DbSync.Env (CoreEnv (..), IngestEnv (..))
import DbSync.Extractor (ExtractState, ExtractorDef (..), freshExtractState)
import DbSync.Id.DedupMap (newMaps)
import DbSync.Ingest.Consumer (runConsumer)
import DbSync.Ingest.ReceiverStats (newReceiverStats)
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
import DbSync.Phase.Boot
  ( BootDecision (..)
  , ResumeContext (..)
  , ResumeIntersection (..)
  , decideBoot
  , mkCardanoPoint
  , renderBootError
  )
import DbSync.Resolver.AddressBuffer (newAddressBufferRef)
import DbSync.Resolver.AddressWorker (awaitDrained, closeAddressResolver, mkAddressResolver)
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.StateQuery (newStateQueryVar, seedInterpreterFromLedgerState)
import DbSync.Trace.Backend (mkStdErrTracer)
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Watchdog (newWatchdog, runWatchdog)
import DbSync.Writer.CopyAdapter (mkCopyWriterAdapter)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Cardano.Network.NodeToClient (withIOManager)

import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot (..), listSnapshots)

main :: IO ()
main = do
  -- 1. Parse CLI + create tracer immediately
  args <- parseCliArgs
  tracer <- mkStdErrTracer Info

  let logError msg = traceWith tracer $ LogMsg Error "App" msg Nothing
      logInfo msg  = traceWith tracer $ LogMsg Info "App" msg Nothing

  -- 2. Parse profile (database, sync options, logging)
  profileResult <- parseConfig (caProfile args)
  profile <- case profileResult of
    Left err -> do
      logError $ "Error parsing profile: " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 3. Validate profile
  validProfile <- case validateConfig profile of
    Left errs -> do
      logError "Profile validation errors:"
      for_ errs $ \err -> logError $ "  - " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 4. Parse db-sync-config.json → extract NodeConfigFile → parse node config
  dbSyncCfgResult <- parseDbSyncNodeConfig (caDbSyncConfig args)
  dbSyncCfg <- case dbSyncCfgResult of
    Left err -> do
      logError $ "Error parsing db-sync-config.json: " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 5. Resolve NodeConfigFile relative to db-sync-config.json directory, then parse
  let configDir = takeDirectory (caDbSyncConfig args)
      nodeConfigPath = configDir </> dscNodeConfigFile dbSyncCfg
  nodeCfgResult <- parseNodeConfig nodeConfigPath
  nodeCfg <- case nodeCfgResult of
    Left err -> do
      logError $ "Error parsing node config (" <> toS nodeConfigPath <> "): " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 6. Read genesis files → TopLevelConfig.
  --    Done before buildCoreEnv so the chain's Network can be sourced
  --    from the Shelley genesis and stored on CoreEnv.
  genesisResult <- readCardanoGenesisConfig nodeCfg configDir
  genesisCfg <- case genesisResult of
    Left err -> do
      logError $ "Error reading genesis files: " <> show err
      exitFailure
    Right gc -> do
      logInfo "Genesis files loaded successfully"
      pure gc

  let topLevelCfg = mkTopLevelConfig nodeCfg genesisCfg
      networkMagic = getNetworkMagic genesisCfg
      network      = sgNetworkId (scConfig (gcShelley genesisCfg))

  -- 7. Build the shared core environment
  coreEnv <- buildCoreEnv tracer validProfile nodeCfg network

  -- 8. Startup logging
  runAppM coreEnv runStartup

  -- LSM session lives in <--ledger-state-dir>/dbsync-ledger/.
  let ledgerStateDir = caLedgerStateDir args </> "dbsync-ledger"
  logInfo $ "Ledger state dir: " <> toS ledgerStateDir
  logInfo $ "Socket: " <> toS (caSocketPath args)

  -- 9. Build StateQueryVar for epoch/slot computation via HardFork Interpreter.
  -- The TopLevelConfig is passed in so the locally-observed fallback
  -- summary can extract per-era 'EraParams' from consensus rather than
  -- shipping its own copy.
  stateQueryVar <- newStateQueryVar topLevelCfg

  -- 10. Database connection settings from profile. libpq for COPY
  -- streaming, hasql for control-plane queries.
  let dbCfg = scDatabase validProfile
      connStr = TE.encodeUtf8 $ "dbname=" <> dcName dbCfg
      hasqlSettings =
        mconcat
          [ HasqlSettings.hostAndPort (dcHost dbCfg) (fromIntegral (dcPort dbCfg))
          , HasqlSettings.user (dcUser dbCfg)
          , HasqlSettings.password (dcPassword dbCfg)
          , HasqlSettings.dbname (dcName dbCfg)
          ]

  -- 11. Schema check + (re)init
  --
  -- Decision matrix:
  --   --resync-from-genesis → wipe everything and re-init.
  --   schema_version absent → fresh DB, run init.
  --   schema_version matches → skip init, resume.
  --   schema_version mismatched → abort with operator-facing diagnostics.
  let extractors = ceExtractors coreEnv
      tableDefs  = concatMap pdTables extractors
      versions   = map (\e -> (pdName e, pdVersion e)) extractors
      connStrTxt = TE.decodeUtf8 connStr
      schemaVersion = 1 :: Int
      ledgerEnabledCfg = lcEnabled (scLedger validProfile)
  schemaState <- checkSchemaVersions versions connStrTxt
  needsSeed <- case decideSchemaAction (caResyncFromGenesis args) schemaState of
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
      when (lcEnabled (scLedger validProfile)) $ do
        logInfo $ "--resync-from-genesis: wiping ledger state directory " <> toS ledgerStateDir
        dropLedgerStateDir ledgerStateDir
      initSchema tableDefs versions connStrTxt
      logInfo "Schema ready"
      pure True
    ActionAbort errs -> do
      logError "Schema mismatch — refusing to start. Use --resync-from-genesis to wipe and re-sync."
      for_ errs $ \err -> logError $ "  - " <> renderSchemaMismatch err
      exitFailure

  -- 12. Open the consumer's control connection. Closed in the
  -- shutdown finally after the COPY writer has drained.
  consumerCtrlConn <- openControlConnection hasqlSettings

  -- Seed dbsync_sync_state on freshly-created schemas.
  when needsSeed $ do
    seedSyncState consumerCtrlConn schemaVersion ledgerEnabledCfg
    logInfo "Sync-state seeded"

  -- 13. SystemStart and ledger plumbing.
  let systemStart = SystemStart (sgSystemStart $ scConfig $ gcShelley genesisCfg)
      pinfo       = mkProtocolInfoCardano nodeCfg genesisCfg

  -- Ledger is opt-in via profile (ledger.enabled = true). The LSM
  -- session is opened here so the boot decision can list disk
  -- snapshots via the snapshot manager.
  let ledgerCfg = scLedger validProfile
  hasLedgerEnv <-
    if lcEnabled ledgerCfg
      then do
        createDirectoryIfMissing True ledgerStateDir
        logInfo $
          "Ledger feature enabled; opening LSM session under "
            <> toS ledgerStateDir
        -- PG connection used by the snapshot-writer thread.
        snapCtrlConn <- openControlConnection hasqlSettings
        mkHasLedgerEnv
          tracer
          pinfo
          ledgerStateDir
          network
          (sgMaxLovelaceSupply (scConfig (gcShelley genesisCfg)))
          systemStart
          580                                              -- snapshot near-tip-epoch threshold
          True                                             -- has rewards
          False                                            -- abort on panic
          (lcBackend ledgerCfg)
          snapCtrlConn
      else do
        logInfo "Ledger feature disabled (set ledger.enabled = true in profile to opt in); skipping LSM session"
        LedgerDisabled <$> mkNoLedgerEnv tracer pinfo systemStart network

  -- Compute the boot decision.
  bootDecision <-
    if needsSeed
      then pure BootFresh
      else do
        mRow <- readSyncState consumerCtrlConn
        snapshots <- case hasLedgerEnv of
          LedgerEnabled lenv -> listSnapshots (leSnapshotManager lenv)
          LedgerDisabled _   -> pure []
        case decideBoot mRow snapshots ledgerEnabledCfg of
          Left bootErr -> do
            for_ (T.lines (renderBootError bootErr)) $ \line ->
              logError line
            exitFailure
          Right d -> pure d

  -- Resolve the boot decision into the initial extract state, dedup
  -- maps, the receiver's intersection requirement, and the
  -- last-committed-slot the consumer should treat as a replay
  -- boundary. The latter is only meaningful for ledger-enabled
  -- resumes, where the receiver intersects at the snapshot slot
  -- (≤ last_committed_slot) and the consumer must skip the replay
  -- window.
  (initialExtractState, dedupMaps, intersectReq, replayBoundary, replayStart, initialAddressId) <- case bootDecision of
    BootFresh -> do
      case hasLedgerEnv of
        LedgerEnabled lenv -> do
          logInfo "Seeding ledger DB from genesis"
          initLedgerDbFromGenesis lenv
        LedgerDisabled _ -> pure ()
      maps <- newMaps
      pure (mkInitState, maps, IntersectGenesis, Nothing, Nothing, 1)

    BootResume rc -> do
      let row = rcSyncState rc
      logInfo $
        "Resuming from slot "
          <> show (ssrLastCommittedSlot row)
          <> ", block "
          <> show (ssrLastCommittedBlockNo row)
      deleted <- deleteRowsPastSlot consumerCtrlConn tableDefs row
      when (deleted > 0) $
        logInfo $
          "Cleaned up " <> show deleted
            <> " rows past last_committed_slot from a prior crash"
      logInfo "Rebuilding dedup maps from PG..."
      maps <- rebuildDedupMaps consumerCtrlConn tableDefs

      -- Ledger-enabled resume: load the snapshot, seed the cached
      -- HFC interpreter from it, announce the replay window.
      -- Ledger-disabled resume: nothing to do here.
      (replayBs, replaySt) <- case (hasLedgerEnv, rcChosenSnapshot rc) of
        (LedgerDisabled _, _) -> pure (Nothing, Nothing)
        (LedgerEnabled lenv, Just snap) -> do
          logInfo $ "Loading ledger snapshot at slot " <> show (dsNumber snap)
          loadResult <- initLedgerDbFromSnapshot lenv snap
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
          -- 'decideBoot' only reaches this branch with a chosen snapshot.
          panic "BootResume (ledger enabled) returned without a chosen snapshot"

      intersectReq <- resolveIntersection logInfo logError consumerCtrlConn rc

      pure
        ( mkResumeExtractState row
        , maps
        , intersectReq
        , replayBs
        , replaySt
        , ssrAddressIdCounter row
        )

    BootFollowingFastPath _ ->
      -- The historic sync has already finished (sync_complete = true).
      -- Skip Ingest setup entirely and hand off to the Follow phase.
      -- The Follow phase isn't implemented yet, so we report the
      -- situation and exit cleanly rather than re-running Ingest.
      panic "Historic sync is complete (sync_complete = true); Follow phase is not yet implemented"

  -- 14. Build the ingest pipeline state
  stRef         <- newIORef initialExtractState
  copyWriter    <- mkCopyWriter connStr tableDefs
  blockQueue    <- newTBQueueIO 500
  receiverStats <- newReceiverStats
  watchdog      <- newWatchdog
  addrBuffer    <- newAddressBufferRef
  addrResolver  <- mkAddressResolver tracer hasqlSettings initialAddressId

  let resolver = mkIngestResolver stRef dedupMaps addrBuffer
      writer   = mkCopyWriterAdapter copyWriter

  let ingestEnv = IngestEnv
        { ieCore                    = coreEnv
        , ieBlockQueue              = blockQueue
        , ieCopyWriter              = copyWriter
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
        }

  -- 13. Start block reception + consumer (+ ledger worker if enabled)
  logInfo "Starting block ingestion..."

  let runIngestPipeline iomgr =
        runAppM ingestEnv runConsumer
          `finally` do
            logInfo "Shutting down COPY writer..."
            cwCommit copyWriter `catch` \(e :: SomeException) ->
              logError $ "Error during final commit: " <> show e
            closeCopyWriter copyWriter
            logInfo "Draining address resolver..."
            awaitDrained addrResolver `catch` \(e :: SomeException) ->
              logError $ "Error draining address resolver: " <> show e
            logInfo "Stopping address resolver..."
            closeAddressResolver addrResolver
              `catch` \(e :: SomeException) ->
                logError $ "Error closing address resolver: " <> show e
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
        where _ = iomgr  -- silence unused-binding (referenced via the closure of nodeThread)

  -- Every spawned 'Async' is 'link'ed to the main thread: a child crash
  -- propagates here and brings the app down with a visible stack trace,
  -- instead of leaving us hung on a queue while the worker has died
  -- silently.
  --
  -- The watchdog is started under every case so it can flag a hang
  -- regardless of which sub-thread is stuck. It only logs (no
  -- side-effects on the pipeline), so a crash inside it should be
  -- visible but recoverable; 'link'ing it means a watchdog bug would
  -- bring the app down loudly rather than silently degrading visibility.
  let mLedgerQueue = case hasLedgerEnv of
        LedgerEnabled lenv -> Just (leLedgerQueue lenv)
        LedgerDisabled _   -> Nothing
      mAppliedQueue = case hasLedgerEnv of
        LedgerEnabled lenv -> Just (leAppliedQueue lenv)
        LedgerDisabled _   -> Nothing
  withIOManager $ \iomgr ->
    withAsync (runWatchdog tracer watchdog blockQueue mLedgerQueue mAppliedQueue) $ \watchdogThread -> do
      link watchdogThread
      withAsync (runAppM ingestEnv $ connectToNode iomgr topLevelCfg networkMagic (caSocketPath args) intersectReq) $ \nodeThread -> do
        link nodeThread
        case hasLedgerEnv of
          LedgerEnabled lenv ->
            withAsync (runAppM lenv (runLedgerWorker replayBoundary stateQueryVar watchdog)) $ \workerThread -> do
              link workerThread
              withAsync (runLedgerStateWriteThread hasLedgerEnv) $ \snapWriter -> do
                link snapWriter
                runIngestPipeline iomgr
          LedgerDisabled _ ->
            runIngestPipeline iomgr

-- | Turn a 'ResumeContext' into the receiver's intersection
-- requirement. Mirrors upstream cardano-db-sync's
-- @verifySnapshotPoint@: the snapshot supplies /the slot/, PG\'s
-- @block@ table is the oracle for /the hash/. Orphaned candidates
-- (no PG row at the slot) are dropped silently. Panics when /every/
-- candidate is orphaned — recovery is operator-driven (restore PG
-- from a backup covering one of these slots, or
-- @--resync-from-genesis@).
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

-- | 'Just' the canonical @(slot, hash)@ point for a snapshot slot,
-- or 'Nothing' when PG has no matching block (orphaned snapshot).
resolveSlot
  :: (Text -> IO ())
  -> ControlConnection
  -> Word64
  -> IO (Maybe CardanoPoint)
resolveSlot logInfo ctrl slot = do
  mHash <- fetchBlockHashAtSlot ctrl slot
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
-- Dedup maps are created separately via 'newMaps' (mutable hash tables).
mkInitState :: ExtractState
mkInitState = freshExtractState
