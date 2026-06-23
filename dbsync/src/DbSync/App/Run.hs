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
import Data.IORef (newIORef)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hasql.Connection.Settings as HasqlSettings
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Cardano.Chain.Genesis as Byron
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Network.NodeToClient (IOManager, withIOManager)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Consensus.Config (TopLevelConfig)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (listSnapshots)
import Ouroboros.Network.Magic (NetworkMagic)

import DbSync.App.Setup
  ( buildCoreEnv
  , runStartup
  , setupOffChainPoolWorker
  , setupOffChainVoteWorker
  )
import DbSync.App.Args (AppArgs (..))
import DbSync.AppM (runAppM)
import DbSync.SyncState.Row
  ( ControlConnection (..)
  , SyncStateRow (..)
  , closeControlConnection
  , markSyncComplete
  , openControlConnection
  , readSyncState
  , seedSyncState
  )
import DbSync.App.Env
    ( CoreWithConn(..),
      CoreEnv(..),
      IngestEnv(..),
      mkFollowEnvFromIngest )
import DbSync.App.Config.Genesis
  ( GenesisConfig (..)
  , ShelleyConfig (..)
  , mkProtocolInfoCardano
  , mkTopLevelConfig
  )
import DbSync.App.Config.Types
    ( DatabaseConfig(..),
      LedgerConfig(..),
      SyncConfig(..),
      DbSyncOptions(..),
      UtxoOption(..) )
import DbSync.Db.Loader (LoaderStream (..), closeLoaderStream, mkLoaderStream)
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Db.Schema.Migration (MigrationOutcome (..), runMigrations)
import DbSync.Db.Schema.Init
  ( SchemaAction (..)
  , checkExtractorPresence
  , decideSchemaAction
  , dropSchema
  , initSchema
  , renderSchemaMismatch
  , showWalLevel
  , truncateDataTables
  )
import DbSync.Schema.Version (Fingerprint, currentSchemaVersion, unFingerprint)
import DbSync.Extractor.Registry (declaredSchemaFingerprint)
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Phase.Ingest.DedupStore (DedupStores, closeStores)
import DbSync.Phase.Ingest.Consumer (runConsumer)
import DbSync.Phase.Ingest.Genesis (insertByronGenesisDist)
import DbSync.Phase.Ingest.PipelineStats (emptyPipelineStats)
import DbSync.Phase.Ingest.ReceiverStats (newReceiverStats)
import DbSync.Worker.Ledger.Fingerprint
  ( FingerprintCheck (..)
  , checkFingerprint
  , computeFingerprint
  , writeFingerprint
  )
import DbSync.Worker.Ledger.Event (RewardsCapture (..))
import DbSync.Worker.Ledger.State (dropLedgerStateDir, mkHasLedgerEnv)
import DbSync.Worker.Ledger.Types
  ( HasLedgerEnv (..)
  , LedgerEnv (..)
  , PanicPolicy (..)
  , mkNoLedgerEnv
  )
import DbSync.Worker.Ledger.Worker (withLedgerThreads)
import DbSync.ChainSync.Connection
  ( IntersectionRequirement (..)
  , connectToNode
  , getNetworkMagic
  )

import DbSync.App.Boot
  ( BootDecision (..)
  , BootError (..)
  , IngestBootState (..)
  , abortBoot
  , decideBoot
  , handlePreBootRollback
  , resolveFreshBoot
  , resolveIntersection
  , resolveResumeBoot
  , resumeContextFrom
  , runBootFollowRestart
  , runFollowSession
  )
import DbSync.Phase.Type (SyncPhase (..))
import DbSync.Phase.Current (setCurrentPhase)
import qualified DbSync.Phase.Preparing.Run as Prep
import DbSync.Phase.Preparing.Tuning (defaultPrepTuning)
import DbSync.Worker.OffChain.Pool
  ( OffChainPoolWorker
  , closeOffChainPoolWorker
  )
import DbSync.Worker.OffChain.Vote
  ( OffChainVoteWorker
  , closeOffChainVoteWorker
  )
import DbSync.Worker.TxOut.AddressBuffer (newAddressBufferRef)
import DbSync.Worker.TxOut.Worker
  ( TxOutWorker
  , awaitTxOutDrained
  , closeTxOutWorker
  , mkTxOutWorker
  )

import DbSync.Phase.Ingest.FdLimit (raiseFdLimit)
import DbSync.Phase.Ingest.Indexes (createIngestResolveIndexes)
import DbSync.Phase.Ingest.LsmSession
  ( LsmSession
  , closeAndDeleteLsmSession
  , closeLsmSession
  , lsmSessionTracerFromApp
  , openLsmSession
  )
import DbSync.Phase.Ingest.Resolver (mkIngestResolver)
import DbSync.Phase.Ingest.UtxoStore (UtxoStore, closeUtxoStore, openUtxoStore)
import DbSync.Worker.TxOut.ConsumedByBuffer (newConsumedByBufferRef)
import DbSync.StateQuery (StateQueryVar, newStateQueryVar)
import DbSync.Trace.Types
  ( AppTracer
  , logErrorIO
  , logInfoIO
  , logWarnIO
  )
import DbSync.Trace.Pulse (newPulse, runPulseIO)
import DbSync.Trace.Watchdog
  ( WatchdogIngestSamples (..)
  , newWatchdog
  , runWatchdogIO
  )
import qualified DbSync.Phase.Ingest.Writer as IngestWriter

-- | Drive the full sync lifecycle from a pre-parsed 'AppArgs'.
--
-- Returns when 'aaShutdownSignal' fires (test path) or when a linked
-- async crashes and propagates out. The body is a fixed sequence of
-- setup steps followed by one of two terminal paths:
--
--   * 'BootFollowRestart' → 'runBootFollowRestart' takes over and
--     never returns to runApp.
--   * 'BootFresh' / 'BootResume' → 'runIngestThenFollow' runs the
--     Ingest → Prep → Follow pipeline.
runApp :: AppTracer -> AppArgs -> IO ()
runApp tracer args = do
  let validProfile = aaProfile args
      nodeCfg      = aaNodeConfig args
      genesisCfg   = aaGenesisConfig args
      socketPath   = aaSocketPath args
      mShutdown    = aaShutdownSignal args
      topLevelCfg  = mkTopLevelConfig nodeCfg genesisCfg
      networkMagic = getNetworkMagic genesisCfg
      network      = sgNetworkId (scConfig (gcShelley genesisCfg))

  -- 1. Raise the open-file soft limit before any LSM session opens.
  raiseFdLimit tracer

  -- 2. Shared core environment + startup log line.
  coreEnv <- buildCoreEnv tracer validProfile nodeCfg network
  runAppM coreEnv runStartup

  let ledgerStateDir = aaLedgerStateDir args </> "dbsync-ledger"
  logInfoIO tracer "App" $ "Ledger state dir: " <> toS ledgerStateDir
  logInfoIO tracer "App" $ "Socket: " <> toS socketPath

  -- 3. State-query interpreter handle. Tests pre-seed via 'aaStateQueryVar';
  -- production starts empty and the receiver fills it from LocalStateQuery.
  stateQueryVar <- maybe (newStateQueryVar topLevelCfg) pure (aaStateQueryVar args)

  -- 4. Database connection settings from profile.
  let dbCfg   = scDatabase validProfile
      connStr = TE.encodeUtf8 $ "dbname=" <> dcName dbCfg
      hasqlSettings =
        mconcat
          [ HasqlSettings.hostAndPort (dcHost dbCfg) (fromIntegral (dcPort dbCfg))
          , HasqlSettings.user (dcUser dbCfg)
          , HasqlSettings.password (dcPassword dbCfg)
          , HasqlSettings.dbname (dcName dbCfg)
          ]

  -- 5. Schema check + (re)init. 'freshInit' is 'True' when this run
  -- created the schema (fresh DB or wipe + re-init); used below to
  -- seed the sync-state row and to skip the pre-boot rollback check.
  let extractors       = ceExtractors coreEnv
      tableDefs        = concatMap pdTables extractors
      extractorNames   = map pdName extractors
      connStrTxt       = TE.decodeUtf8 connStr
      schemaVersion    = currentSchemaVersion
      ledgerEnabledCfg = lcEnabled (scLedger validProfile)
  freshInit <- setupSchema
    tracer ledgerEnabledCfg ledgerStateDir
    tableDefs extractorNames connStrTxt (aaResyncFromGenesis args)

  -- 6. Open the consumer's control connection; seed @dbsync_sync_state@
  -- on a fresh schema, then apply any schema migrations between the
  -- database's recorded version and this binary's. The gate is a no-op on
  -- a fresh schema and aborts on a newer database or uncovered drift.
  consumerCtrlConn <- openControlConnection hasqlSettings
  when freshInit $
    setupFreshSyncState
      tracer consumerCtrlConn connStrTxt
      schemaVersion declaredSchemaFingerprint ledgerEnabledCfg extractorNames
  runMigrationGate
    tracer consumerCtrlConn schemaVersion declaredSchemaFingerprint extractorNames

  -- 7. SystemStart + ledger subsystem (fingerprint check, LSM
  -- session, snapshot manager).
  let systemStart = SystemStart (sgSystemStart $ scConfig $ gcShelley genesisCfg)
      pinfo       = mkProtocolInfoCardano nodeCfg genesisCfg
      ledgerCfg   = scLedger validProfile
  hasLedgerEnv <- setupLedgerEnv
    tracer hasqlSettings coreEnv ledgerCfg ledgerStateDir
    genesisCfg pinfo systemStart network

  -- 8. Apply any outstanding rollback request (CLI flag or on-DB
  -- marker). Skipped after 'freshInit' — nothing's committed yet.
  unless freshInit $
    handlePreBootRollback
      tracer coreEnv consumerCtrlConn tableDefs hasLedgerEnv
      (aaRollbackToSlot args)

  -- 9. Boot decision. 'decideBoot' still runs after a fresh seed so a
  -- stale 'dbsync-ledger/' against an empty PG aborts with
  -- 'BootSnapshotsWithoutPgState'.
  bootDecision <- do
    mRow <- runAppM consumerCtrlConn readSyncState
    snapshots <- case hasLedgerEnv of
      LedgerEnabled lenv -> listSnapshots (leSnapshotManager lenv)
      LedgerDisabled _   -> pure []
    case decideBoot mRow snapshots ledgerEnabledCfg of
      Left bootErr -> abortBoot tracer bootErr
      Right d      -> pure d

  -- 10. Open the shared LSM session and guarantee its release.
  -- The bracket covers the boot dispatch and the Ingest pipeline
  -- so a cancellation between 'openLsmSession' and the pipeline's
  -- own 'finally' still releases the on-disk lock. The closer is
  -- idempotent, so a later 'closeAndDeleteLsmSession' inside
  -- 'runPrep' is harmless.
  bracket
    (openLsmSession (lsmSessionTracerFromApp tracer) ledgerStateDir)
    (\sess ->
       closeLsmSession sess `catch` \(e :: SomeException) ->
         logErrorIO tracer "App" $
           "Error closing ingest LSM session in runApp bracket: " <> show e)
    $ \lsmSession -> do
      -- 11. Dispatch on the boot decision. 'BootFollowRestart' runs to
      -- completion inline; the other two return the state for
      -- 'runIngestThenFollow' below.
      mIngestState <-
        case bootDecision of
          BootFresh -> do
            -- A fresh boot on a schema we did not just create means the
            -- prior leg crashed mid-Ingest before its first
            -- epoch-boundary commit, so @last_committed_slot@ is NULL.
            -- Orphan rows from that leg survive 'setupSchema' and would
            -- collide with the genesis re-COPY; purge them first.
            unless freshInit $ do
              logInfoIO tracer "Boot"
                "Fresh boot on a non-empty schema; purging orphan rows from an aborted pre-boundary leg"
              truncateDataTables tableDefs connStrTxt
            Just <$> resolveFreshBoot tracer hasLedgerEnv lsmSession
          BootResume rc ->
            Just <$> resolveResumeBoot
              tracer topLevelCfg
              stateQueryVar hasLedgerEnv consumerCtrlConn tableDefs lsmSession rc
          BootFollowRestart frc -> do
            runBootFollowRestart
              tracer hasqlSettings coreEnv topLevelCfg networkMagic
              socketPath systemStart stateQueryVar hasLedgerEnv consumerCtrlConn
              lsmSession frc mShutdown
            pure Nothing

      -- 12. Ingest → Prep → Follow. No-op on 'BootFollowRestart' ('Nothing').
      for_ mIngestState $
        runIngestThenFollow
          tracer hasqlSettings connStr coreEnv validProfile
          topLevelCfg networkMagic socketPath systemStart stateQueryVar
          hasLedgerEnv consumerCtrlConn lsmSession tableDefs mShutdown
          (gcByron genesisCfg)

-- ---------------------------------------------------------------------------
-- * Setup steps (in execution order)
-- ---------------------------------------------------------------------------

-- | Run the schema-version gate: compare the database's applied version to
-- this binary's, apply the intervening migration files, and abort on a
-- database newer than the binary or on drift no migration covers.
runMigrationGate
  :: AppTracer
  -> ControlConnection
  -> Int          -- ^ schema version this binary targets
  -> Fingerprint
  -> [Text]       -- ^ enabled extractor names
  -> IO ()
runMigrationGate tracer ctrlConn target declaredFp extractors = do
  outcome <-
    runMigrations
      (unControlConnection ctrlConn) target (unFingerprint declaredFp) extractors
  case outcome of
    NoMigrationNeeded -> pure ()
    MigrationsToApply versions ->
      logInfoIO tracer "Migration" $
        "Applied schema migrations: versions " <> show (NE.toList versions)
    DbNewerThanBinary database binary ->
      abortBoot tracer (BootSchemaNewerThanBinary database binary)
    SchemaDriftUncovered stored declared ->
      abortBoot tracer (BootSchemaDriftUncovered stored declared)

-- | Step 5: dispatch the schema decision.
--
-- Classifies the boot via 'decideSchemaAction' and runs the
-- matching side-effect (init / drop+init / no-op / abort). The
-- ledger state directory is wiped on @--resync-from-genesis@ so a
-- stale on-disk ledger can't attach to a freshly-seeded PG.
--
-- Returns 'True' if this run (re)created the schema; the caller
-- uses that to drive sync-state seeding and to skip the pre-boot
-- rollback check.
setupSchema
  :: AppTracer
  -> Bool                         -- ^ @ledger.enabled@ from config
  -> FilePath                     -- ^ ledger state directory
  -> [TableDef]
  -> [Text]                       -- ^ enabled extractor names
  -> Text                         -- ^ psql connection string
  -> Bool                         -- ^ @--resync-from-genesis@ flag
  -> IO Bool
setupSchema tracer ledgerEnabledCfg ledgerStateDir
            tableDefs extractorNames connStrTxt resyncFromGenesis = do
  schemaState <- checkExtractorPresence extractorNames connStrTxt
  case decideSchemaAction resyncFromGenesis schemaState of
    ActionSkipInit -> do
      logInfoIO tracer "App" "Schema present and matches enabled extractors; skipping init"
      pure False
    ActionRunInit -> do
      logInfoIO tracer "App" "Fresh database detected; creating schema"
      initSchema tableDefs connStrTxt
      logInfoIO tracer "App" "Schema ready"
      pure True
    ActionForceReinit -> do
      logInfoIO tracer "App" "--resync-from-genesis: dropping existing schema and re-initialising"
      dropSchema tableDefs connStrTxt
      when ledgerEnabledCfg $ do
        logInfoIO tracer "App" $
          "--resync-from-genesis: wiping ledger state directory " <> toS ledgerStateDir
        dropLedgerStateDir ledgerStateDir
      initSchema tableDefs connStrTxt
      logInfoIO tracer "App" "Schema ready"
      pure True
    ActionAbort errs -> do
      logErrorIO tracer "App"
        "Schema mismatch — refusing to start. Use --resync-from-genesis to wipe and re-sync."
      for_ errs $ \err ->
        logErrorIO tracer "App" $ "  - " <> renderSchemaMismatch err
      exitFailure

-- | Step 6: insert the initial @dbsync_sync_state@ row on a fresh
-- boot and warn the operator if @wal_level@ isn't tuned for
-- bulk-load.
--
-- The WAL hint is best-effort — managed PG operators may not have
-- the privilege to change it. Production runs at @wal_level=minimal@
-- skip WAL entirely for the UNLOGGED → LOGGED flip in
-- 'PreparingForVolatileTail' for tables above @wal_skip_threshold@.
setupFreshSyncState
  :: AppTracer
  -> ControlConnection
  -> Text                         -- ^ psql connection string
  -> Int                          -- ^ schema version
  -> Fingerprint                  -- ^ declared schema fingerprint
  -> Bool                         -- ^ @ledger.enabled@
  -> [Text]                       -- ^ enabled extractor names
  -> IO ()
setupFreshSyncState tracer ctrl connStrTxt schemaVersion fingerprint ledgerEnabledCfg extractorNames = do
  runAppM ctrl (seedSyncState schemaVersion fingerprint ledgerEnabledCfg extractorNames)
  logInfoIO tracer "App" "Sync-state seeded"
  walLevel <- showWalLevel connStrTxt
  unless (walLevel == "minimal") $
    logWarnIO tracer "App" $ T.unlines
      [ "Postgres wal_level is '" <> walLevel <> "'. For fastest bulk-load,"
      , "set the following in postgresql.conf and restart the server:"
      , "  wal_level = minimal"
      , "  max_wal_senders = 0"
      , "  archive_mode = off"
      , "See profiles/postgres-tuning.conf for the full snippet."
      , "Note: replicas will need a full re-base after reverting to"
      , "wal_level = replica. Acceptable on a one-time fresh sync."
      ]

-- | Step 7: build the 'HasLedgerEnv' for this run.
--
-- Ledger off — returns 'LedgerDisabled' with the minimal env (just
-- enough to deserialise blocks).
--
-- Ledger on:
--
--   1. Verify the fingerprint in the state directory matches this
--      chain's network magic + system start; abort on mismatch so
--      the operator doesn't silently corrupt PG by attaching a
--      different chain's ledger.
--   2. Open the LSM session under @\<ledgerStateDir\>/dbsync-ledger@.
--   3. Stamp a fresh fingerprint when there was none on disk.
setupLedgerEnv
  :: AppTracer
  -> HasqlSettings.Settings
  -> CoreEnv
  -> LedgerConfig
  -> FilePath                     -- ^ ledger state directory
  -> GenesisConfig
  -> Consensus.ProtocolInfo (CardanoBlock StandardCrypto)
  -> SystemStart
  -> Network
  -> IO HasLedgerEnv
setupLedgerEnv tracer hasqlSettings coreEnv ledgerCfg
               ledgerStateDir genesisCfg pinfo systemStart network = do
  let expectedFp = computeFingerprint genesisCfg
  fpCheck <-
    if lcEnabled ledgerCfg
      then checkFingerprint ledgerStateDir expectedFp
      else pure FingerprintMatch
  case fpCheck of
    FingerprintMatch -> pure ()
    FingerprintFresh -> pure ()
    FingerprintMismatch onDisk expected ->
      abortBoot tracer (BootLedgerStateFingerprintMismatch onDisk expected)
    FingerprintMissing dir ->
      abortBoot tracer (BootLedgerStateFingerprintMissing dir)

  hasLedgerEnv <-
    if lcEnabled ledgerCfg
      then do
        createDirectoryIfMissing True ledgerStateDir
        logInfoIO tracer "App" $
          "Ledger feature enabled; opening LSM session under " <> toS ledgerStateDir
        snapCtrlConn <- openControlConnection hasqlSettings
        mkHasLedgerEnv
          tracer
          pinfo
          ledgerStateDir
          network
          (sgMaxLovelaceSupply (scConfig (gcShelley genesisCfg)))
          systemStart
          (lcSnapshotNearTipEpoch ledgerCfg)
          CaptureRewards
          LogAndContinue
          (lcBackend ledgerCfg)
          snapCtrlConn
          (ceCurrentPhase coreEnv)
      else do
        logInfoIO tracer "App"
          "Ledger feature disabled (set ledger.enabled = true in profile to opt in); skipping LSM session"
        LedgerDisabled <$> mkNoLedgerEnv tracer pinfo systemStart network
  when (lcEnabled ledgerCfg && fpCheck == FingerprintFresh) $
    writeFingerprint ledgerStateDir expectedFp
  pure hasLedgerEnv

-- ---------------------------------------------------------------------------
-- * Ingest pipeline
-- ---------------------------------------------------------------------------

-- | Run the Ingest → Prep → Follow pipeline for one boot.
--
--   1. Build the 'IngestEnv' from the resolved 'IngestBootState'.
--   2. Start the chainsync receiver + watchdog + pulse asyncs and
--      run 'runConsumer' until it exits at the rollback boundary.
--   3. Cancel the receiver, run 'PreparingForVolatileTail' in its
--      own connection, mark sync complete.
--   4. Flip to 'FollowingVolatileTail' and hand off to
--      'handoffToFollow'.
--
-- Shutdown is layered so an exception at any step still releases
-- the loader stream, tx-out worker, dedup stores, UTxO store, and
-- both LSM sessions. The ledger worker + snapshot-writer asyncs
-- stay alive across the Ingest → Prep transition and into Follow.
runIngestThenFollow
  :: AppTracer
  -> HasqlSettings.Settings
  -> ByteString                                       -- ^ libpq connStr for loader streams
  -> CoreEnv
  -> SyncConfig
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath                                         -- ^ socketPath
  -> SystemStart
  -> StateQueryVar
  -> HasLedgerEnv
  -> ControlConnection                                -- ^ consumer's control connection
  -> LsmSession
  -> [TableDef]
  -> Maybe (IO ())                                    -- ^ optional shutdown signal
  -> Byron.Config                                     -- ^ Byron genesis, for the fresh-boot distribution
  -> IngestBootState
  -> IO ()
runIngestThenFollow
  tracer hasqlSettings connStr coreEnv validProfile
  topLevelCfg networkMagic socketPath systemStart stateQueryVar
  hasLedgerEnv consumerCtrlConn lsmSession tableDefs mShutdown byronCfg
  IngestBootState
    { ibsInitialExtractState = initialExtractState
    , ibsDedupStores         = dedupStores
    , ibsIntersection        = intersectReq
    , ibsReplayBoundary      = replayBoundary
    , ibsReplayStart         = replayStart
    , ibsAddressIdCounter    = initialAddressId
    } = do
    -- Resolver indexes on the still-UNLOGGED Ingest tables. Without
    -- these the per-epoch bulk @UPDATE tx_out@ + @SELECT address@ in
    -- 'DbSync.Worker.TxOut.Worker' hash-joins the full heap.
    -- Idempotent.
    createIngestResolveIndexes tracer (unControlConnection consumerCtrlConn)

    -- Allocate per-pipeline state. 'lsmSession' and 'dedupStores'
    -- were opened by the caller.
    extractStateRef  <- newIORef initialExtractState
    loaderStream     <- mkLoaderStream connStr tableDefs
    -- 150 ≈ 1.5 consumer drain batches of headroom. Deliberately not
    -- deeper: each slot can pin a fully-decoded block (several
    -- hundred KB of heap for a dense Conway block), and during bulk
    -- sync the consumer is the bottleneck so a deep queue just sits
    -- full — at 500 that was a few hundred MB of standing heap.
    blockQueue       <- newTBQueueIO 150
    receiverStats    <- newReceiverStats
    pipelineStats    <- newIORef emptyPipelineStats
    watchdog         <- newWatchdog (ceMinSeverity coreEnv)
    pulse            <- newPulse (ceMinSeverity coreEnv)
    addrBuffer       <- newAddressBufferRef
    txOutWorker      <- mkTxOutWorker tracer hasqlSettings initialAddressId
    mPoolWorker      <-
      setupOffChainPoolWorker tracer hasqlSettings (scOptions validProfile)
    mVoteWorker      <-
      setupOffChainVoteWorker tracer hasqlSettings (scOptions validProfile)
    utxoStore        <- openUtxoStore lsmSession
    let consumedByOn = uoConsumedByTxId (pcUtxo (scOptions validProfile))
    mConsumedByBuf <-
      if consumedByOn then Just <$> newConsumedByBufferRef else pure Nothing
    latestPointRef   <- newIORef Nothing
    rollbackBoundary <- newTVarIO Nothing
    latestTipBlock   <- newTVarIO Nothing

    let resolver = mkIngestResolver extractStateRef dedupStores addrBuffer utxoStore mConsumedByBuf
        writer   = IngestWriter.mkWriter loaderStream

    let ingestEnv = IngestEnv
          { ieCore                    = coreEnv
          , ieBlockQueue              = blockQueue
          , ieLoaderStream            = loaderStream
          , ieDedupStores             = dedupStores
          , ieAddressBuffer           = addrBuffer
          , ieTxOutWorker             = txOutWorker
          , ieOffChainPoolWorker      = mPoolWorker
          , ieOffChainVoteWorker      = mVoteWorker
          , ieLsmSession              = lsmSession
          , ieUtxoStore               = utxoStore
          , ieConsumedByBuffer        = mConsumedByBuf
          , ieHasLedgerEnv            = hasLedgerEnv
          , ieStateQueryVar           = stateQueryVar
          , ieSystemStart             = systemStart
          , ieResolver                = resolver
          , ieWriter                  = writer
          , ieExtractState            = extractStateRef
          , ieReceiverStats           = receiverStats
          , iePipelineStats           = pipelineStats
          , ieControlConnection       = consumerCtrlConn
          , ieLastCommittedSlotAtBoot = replayBoundary
          , ieReplayStartSlot         = replayStart
          , ieWatchdog                = watchdog
          , iePulse                   = pulse
          , ieLatestReceivedPoint     = latestPointRef
          , ieRollbackBoundary        = rollbackBoundary
          , ieLatestTipBlock          = latestTipBlock
          }

    -- On a fresh sync the Byron genesis UTxO distribution is not a
    -- chain block, so it must be written before the consumer applies
    -- block 1 — both so that block's @previous_id@ resolves and so
    -- later Byron txs spending genesis outputs can find their inputs.
    case (intersectReq, replayBoundary) of
      (IntersectGenesis, Nothing) ->
        runAppM ingestEnv (insertByronGenesisDist byronCfg)
      _ -> pure ()

    logInfoIO tracer "App" "Starting block ingestion..."

    -- The consumer runs until it observes a block crossing the
    -- rollback boundary (@nodeTip − k@). 'shutdownIngest' releases
    -- the resources only the Ingest pipeline owns; the ledger
    -- worker and snapshot writer (started by 'withLedgerThreads'
    -- below) survive into Prep and Follow.
    let shutdownIngest             = closeIngestResources tracer loaderStream txOutWorker utxoStore dedupStores
        ingestAction               = runAppM ingestEnv runConsumer `finally` shutdownIngest
        shutdownPostIngest         = closePipelineResources tracer consumerCtrlConn lsmSession hasLedgerEnv mPoolWorker mVoteWorker
        runPrepAndMarkComplete     = runPrep tracer coreEnv hasqlSettings tableDefs lsmSession
        mLedgerQueue               = case hasLedgerEnv of
          LedgerEnabled lenv -> Just (leLedgerQueue lenv)
          LedgerDisabled _   -> Nothing
        watchdogSamples            = WatchdogIngestSamples
          { wisPipelineStats = pipelineStats
          , wisReceiverStats = receiverStats
          , wisUtxoStore     = utxoStore
          }

    -- The receiver / consumer / ledger-worker tree:
    --   * 'withLedgerThreads' spawns 'LedgerWorker' + snapshot writer.
    --     They live across the Ingest → Prep → Follow boundary so the
    --     in-RAM 'LedgerDB' keeps ticking while Prep runs.
    --   * 'withAsyncs' spawns the watchdog + pulse samplers.
    --   * The innermost 'withAsync' is the chainsync receiver. We
    --     cancel it before Prep so the consumer queue stops growing.
    withIOManager (\iomgr ->
      withLedgerThreads hasLedgerEnv replayBoundary stateQueryVar watchdog $
        withAsyncs
          [ runWatchdogIO tracer watchdog blockQueue mLedgerQueue (Just watchdogSamples)
          , runPulseIO tracer pulse watchdog blockQueue 500 loaderStream
          ]
          $ do
          withAsync (runAppM ingestEnv $ connectToNode iomgr topLevelCfg networkMagic socketPath intersectReq) $ \nodeThread -> do
            link nodeThread
            ingestAction
            cancel nodeThread
          runPrepAndMarkComplete
          runAppM coreEnv (setCurrentPhase (ceCurrentPhase coreEnv) FollowingVolatileTail)
          handoffToFollow
            iomgr ingestEnv tracer hasqlSettings
            topLevelCfg networkMagic socketPath mShutdown
      ) `finally` shutdownPostIngest

-- | Release the resources only 'IngestChainHistory' owns. Called by
-- the consumer's 'finally' so a mid-flight crash doesn't leak the
-- loader stream or the tx-out worker.
closeIngestResources
  :: AppTracer
  -> LoaderStream
  -> TxOutWorker
  -> UtxoStore
  -> DedupStores
  -> IO ()
closeIngestResources tracer loaderStream txOutWorker utxoStore dedupStores = do
  logInfoIO tracer "App" "Shutting down loader stream..."
  lsCommit loaderStream `catch` \(e :: SomeException) ->
    logErrorIO tracer "App" $ "Error during final commit: " <> show e
  closeLoaderStream loaderStream
  logInfoIO tracer "App" "Draining tx_out worker..."
  awaitTxOutDrained txOutWorker `catch` \(e :: SomeException) ->
    logErrorIO tracer "App" $ "Error draining tx_out worker: " <> show e
  logInfoIO tracer "App" "Stopping tx_out worker..."
  closeTxOutWorker txOutWorker
    `catch` \(e :: SomeException) ->
      logErrorIO tracer "App" $ "Error closing tx_out worker: " <> show e
  logInfoIO tracer "App" "Closing UTxO store table..."
  closeUtxoStore utxoStore
    `catch` \(e :: SomeException) ->
      logErrorIO tracer "App" $ "Error closing UTxO store: " <> show e
  logInfoIO tracer "App" "Closing dedup store tables..."
  closeStores dedupStores
    `catch` \(e :: SomeException) ->
      logErrorIO tracer "App" $ "Error closing dedup stores: " <> show e

-- | Release pipeline-wide resources after the Follow loop exits.
-- Idempotent on the LSM session: 'runPrep' may have already
-- deleted it via 'closeAndDeleteLsmSession'.
closePipelineResources
  :: AppTracer
  -> ControlConnection
  -> LsmSession
  -> HasLedgerEnv
  -> Maybe OffChainPoolWorker
  -> Maybe OffChainVoteWorker
  -> IO ()
closePipelineResources tracer consumerCtrlConn lsmSession hasLedgerEnv mPoolWorker mVoteWorker = do
  logInfoIO tracer "App" "Closing consumer control connection..."
  closeControlConnection consumerCtrlConn
    `catch` \(e :: SomeException) ->
      logErrorIO tracer "App" $ "Error closing consumer control connection: " <> show e
  for_ mPoolWorker $ \w -> do
    logInfoIO tracer "App" "Stopping off-chain pool worker..."
    closeOffChainPoolWorker w
      `catch` \(e :: SomeException) ->
        logErrorIO tracer "App" $ "Error closing off-chain pool worker: " <> show e
  for_ mVoteWorker $ \w -> do
    logInfoIO tracer "App" "Stopping off-chain vote worker..."
    closeOffChainVoteWorker w
      `catch` \(e :: SomeException) ->
        logErrorIO tracer "App" $ "Error closing off-chain vote worker: " <> show e
  logInfoIO tracer "App" "Closing ingest LSM session..."
  closeLsmSession lsmSession
    `catch` \(e :: SomeException) ->
      logErrorIO tracer "App" $ "Error closing ingest LSM session: " <> show e
  case hasLedgerEnv of
    LedgerEnabled lenv -> do
      logInfoIO tracer "App" "Closing ledger LSM session..."
      leClose lenv `catch` \(e :: SomeException) ->
        logErrorIO tracer "App" $ "Error closing ledger LSM session: " <> show e
      logInfoIO tracer "App" "Closing snapshot-writer control connection..."
      closeControlConnection (leControlConnection lenv)
        `catch` \(e :: SomeException) ->
          logErrorIO tracer "App" $ "Error closing snapshot control connection: " <> show e
    LedgerDisabled _ -> pure ()

-- | Run 'PreparingForVolatileTail' against a fresh hasql connection
-- and flip @sync_complete@ true. Wipes the ingest LSM scratch
-- directory on success — Follow doesn't consult it.
runPrep
  :: AppTracer
  -> CoreEnv
  -> HasqlSettings.Settings
  -> [TableDef]
  -> LsmSession
  -> IO ()
runPrep tracer coreEnv hasqlSettings tableDefs lsmSession = do
  bracket (openControlConnection hasqlSettings) closeControlConnection $ \prepConn -> do
    runAppM coreEnv (setCurrentPhase (ceCurrentPhase coreEnv) PreparingForVolatileTail)
    let prepEnv = CoreWithConn coreEnv (unControlConnection prepConn)
    runAppM prepEnv (Prep.run hasqlSettings defaultPrepTuning tableDefs)
    runAppM prepConn markSyncComplete
  logInfoIO tracer "App" "Removing ingest LSM scratch directory..."
  closeAndDeleteLsmSession lsmSession

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Run a list of background 'IO' actions concurrently with the
-- inner @body@. Each background action is spawned via 'withAsync'
-- and 'link'ed to the calling thread, so an exception in any of
-- them propagates immediately. When @body@ exits — whether cleanly
-- or via an exception — every background async is cancelled by the
-- enclosing 'withAsync's' bracket.
--
-- All background actions share the same lifetime (that of @body@).
-- Use nested 'withAsync' calls directly when threads need to die
-- at different points.
withAsyncs :: [IO ()] -> IO a -> IO a
withAsyncs []       body = body
withAsyncs (a : as) body =
  withAsync a $ \th -> do
    link th
    withAsyncs as body

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
  -> AppTracer
  -> HasqlSettings.Settings
  -> TopLevelConfig (CardanoBlock StandardCrypto)
  -> NetworkMagic
  -> FilePath
  -> Maybe (IO ())
  -> IO ()
handoffToFollow
  iomgr ie tracer hasqlSettings topLevelCfg networkMagic
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
    logInfoIO tracer "App" $
      "Prep complete; handing off to FollowingChainTip " <> resumeDesc
    -- The Ingest receiver was cancelled before Prep ran. A new one
    -- opens below and re-intersects from the latestPointRef the
    -- previous receiver wrote. ChainSync will respond with a
    -- protocol-mandated confirming MsgRollBackward to that point —
    -- the receiver tags it 'confirming intersect; not propagated' and
    -- does not enqueue a MsgRollback, so no DB rows are deleted.
    logInfoIO tracer "App"
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
      logInfoIO tracer "App" $
        "FollowingChainTip starting with "
          <> show buffered
          <> " block(s) buffered from Ingest's tail"

    let rc = resumeContextFrom row Nothing
    intersectReq <- resolveIntersection tracer consumerCtrlConn rc

    -- The receiver runs under 'followEnv' so its watchdog / block
    -- queue / rollback boundary refs are the same ones IngestEnv
    -- carried. The Ingest receiver was cancelled before Prep ran;
    -- 'runFollowSession' opens a fresh one starting at the
    -- post-Ingest commit point.
    runFollowSession tracer "App" iomgr hasqlSettings topLevelCfg
      networkMagic socketPath intersectReq mShutdown
      (mkFollowEnvFromIngest ie)


