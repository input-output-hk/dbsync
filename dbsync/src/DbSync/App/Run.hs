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
import Control.Tracer (traceWith)
import Data.IORef (newIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hasql.Connection.Settings as HasqlSettings
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Cardano.Network.NodeToClient (withIOManager)
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
import DbSync.Checkpoint.Resume (deleteRowsPastSlot)
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
import DbSync.Copy.Writer (CopyWriter (..), closeCopyWriter, mkCopyWriter)
import DbSync.Db.Schema.Init
  ( SchemaAction (..)
  , checkSchemaVersions
  , decideSchemaAction
  , dropSchema
  , initSchema
  , renderSchemaMismatch
  )
import DbSync.Env (CoreEnv (..), FollowEnv (..), IngestEnv (..))
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
import qualified DbSync.Phase.FollowingChainTip as Follow
import DbSync.Phase.Boot
  ( BootDecision (..)
  , ResumeContext (..)
  , ResumeIntersection (..)
  , decideBoot
  , mkCardanoPoint
  , renderBootError
  )
import qualified DbSync.Phase.PreparingForChainTip as Prep
import DbSync.Resolver.AddressBuffer (newAddressBufferRef)
import DbSync.Resolver.AddressWorker (awaitDrained, closeAddressResolver, mkAddressResolver)
import DbSync.Resolver.Follow (mkFollowResolver)
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.StateQuery (StateQueryVar, newStateQueryVar, seedInterpreterFromLedgerState)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (newWatchdog, runWatchdog)
import DbSync.Writer.CopyAdapter (mkCopyWriterAdapter)
import DbSync.Writer.InsertAdapter (mkInsertWriter)

-- | Run the full sync lifecycle. Returns when:
--
--   * 'IngestChainHistory' exits cleanly at the rollback boundary
--     and 'PreparingForChainTip' completes (the natural exit point
--     today; a follow-up will chain straight into Follow); or
--   * 'FollowingChainTip' fast-path returns (only when
--     'aaShutdownSignal' fires for the test harness); or
--   * a linked async crashes, propagating its exception out.
runApp :: AppTracer -> AppArgs -> IO ()
runApp tracer args = do
  let validProfile = aaProfile args
      nodeCfg      = aaNodeConfig args
      genesisCfg   = aaGenesisConfig args
      socketPath   = aaSocketPath args
      mShutdown    = aaShutdownSignal args
      logError msg = traceWith tracer $ LogMsg Error "App" msg Nothing
      logInfo  msg = traceWith tracer $ LogMsg Info  "App" msg Nothing
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
    seedSyncState consumerCtrlConn schemaVersion ledgerEnabledCfg
    logInfo "Sync-state seeded"

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
          580
          True
          False
          (lcBackend ledgerCfg)
          snapCtrlConn
      else do
        logInfo "Ledger feature disabled (set ledger.enabled = true in profile to opt in); skipping LSM session"
        LedgerDisabled <$> mkNoLedgerEnv tracer pinfo systemStart network

  -- 7. Boot decision.
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

  -- 8. Resolve the boot decision.
  (initialExtractState, dedupMaps, intersectReq, replayBoundary, replayStart, initialAddressId) <-
    case bootDecision of
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
            panic "BootResume (ledger enabled) returned without a chosen snapshot"

        ireq <- resolveIntersection logInfo logError consumerCtrlConn rc
        pure
          ( mkResumeExtractState row
          , maps
          , ireq
          , replayBs
          , replaySt
          , ssrAddressIdCounter row
          )

      BootFollowingFastPath rc -> do
        runFollowFastPath
          tracer logInfo logError hasqlSettings coreEnv topLevelCfg networkMagic
          socketPath systemStart stateQueryVar hasLedgerEnv
          consumerCtrlConn rc mShutdown
        -- runFollowFastPath either races the shutdown signal and
        -- returns, or blocks forever. If we get here the signal
        -- fired or a linked async crashed; either way nothing else
        -- to do in runApp.
        exitSuccess

  -- 9. Build the ingest pipeline state.
  stRef            <- newIORef initialExtractState
  copyWriter       <- mkCopyWriter connStr tableDefs
  blockQueue       <- newTBQueueIO 500
  receiverStats    <- newReceiverStats
  watchdog         <- newWatchdog (ceMinSeverity coreEnv)
  addrBuffer       <- newAddressBufferRef
  addrResolver     <- mkAddressResolver tracer hasqlSettings initialAddressId
  latestPointRef   <- newIORef Nothing
  rollbackBoundary <- newTVarIO Nothing

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
        , ieLatestReceivedPoint     = latestPointRef
        , ieRollbackBoundary        = rollbackBoundary
        }

  logInfo "Starting block ingestion..."

  -- Cleanup of Ingest-only resources. Runs whether the consumer exits
  -- cleanly at the rollback boundary or aborts with an exception.
  let shutdownIngest = do
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
          Prep.run tracer (unControlConnection prepConn) tableDefs
          markSyncComplete prepConn

  let mLedgerQueue = case hasLedgerEnv of
        LedgerEnabled lenv -> Just (leLedgerQueue lenv)
        LedgerDisabled _   -> Nothing

  withIOManager (\iomgr ->
    withAsync (runWatchdog tracer watchdog blockQueue mLedgerQueue) $ \watchdogThread -> do
      link watchdogThread
      withAsync (runAppM ingestEnv $ connectToNode iomgr topLevelCfg networkMagic socketPath intersectReq) $ \nodeThread -> do
        link nodeThread
        case hasLedgerEnv of
          LedgerEnabled lenv ->
            withAsync (runAppM lenv (runLedgerWorker replayBoundary stateQueryVar watchdog)) $ \workerThread -> do
              link workerThread
              withAsync (runLedgerStateWriteThread hasLedgerEnv) $ \snapWriter -> do
                link snapWriter
                ingestAction
                cancel snapWriter
                cancel workerThread
                cancel nodeThread
                runPrepAndMarkComplete
          LedgerDisabled _ -> do
            ingestAction
            cancel nodeThread
            runPrepAndMarkComplete) `finally` shutdownPostIngest

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
mkInitState :: ExtractState
mkInitState = freshExtractState

-- | Boot directly into 'FollowingChainTip', skipping
-- 'IngestChainHistory' / 'PreparingForChainTip'. When the optional
-- shutdown signal fires, the Follow loop is cancelled and
-- 'runFollowFastPath' returns normally; otherwise it blocks
-- forever (production behaviour).
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
  -> ResumeContext
  -> Maybe (IO ())
  -> IO ()
runFollowFastPath
  tracer logInfo logError hasqlSettings coreEnv topLevelCfg networkMagic
  socketPath systemStart stateQueryVar hasLedgerEnv consumerCtrlConn rc
  mShutdown = do

    logInfo "Boot: sync_complete=true; starting FollowingChainTip"

    let row = rcSyncState rc
        tableDefs = concatMap pdTables (ceExtractors coreEnv)
    deleted <- deleteRowsPastSlot consumerCtrlConn tableDefs row
    when (deleted > 0) $
      logInfo $
        "Cleaned up " <> show deleted
          <> " rows past last_committed_slot from a prior Follow crash"

    followCtrl <- openControlConnection hasqlSettings
    let followConn = unControlConnection followCtrl

    blockQueue       <- newTBQueueIO 500
    receiverStats    <- newReceiverStats
    watchdog         <- newWatchdog (ceMinSeverity coreEnv)
    latestPointRef   <- newIORef Nothing
    rollbackBoundary <- newTVarIO Nothing

    resolver <- mkFollowResolver followConn
    let writer    = mkInsertWriter followConn
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

    intersectReq <- resolveIntersection logInfo logError consumerCtrlConn rc

    let mLedgerQueue = case hasLedgerEnv of
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

    withIOManager $ \iomgr ->
      withAsync (runWatchdog tracer watchdog blockQueue mLedgerQueue) $ \watchdogThread -> do
        link watchdogThread
        withAsync (runAppM followEnv $ connectToNode iomgr topLevelCfg networkMagic socketPath intersectReq) $ \nodeThread -> do
          link nodeThread
          racedFollow
