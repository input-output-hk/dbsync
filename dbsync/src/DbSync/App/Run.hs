{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Top-level orchestration body. 'runApp' takes a pre-parsed
-- 'AppArgs' and drives the whole sync lifecycle: schema check, boot
-- decision, ledger init, the Ingest → Prep → Follow handoff, and the
-- receiver / ledger-worker async tree.
module DbSync.App.Run
  ( runApp

    -- * Exported for the network-gate integration tests
  , runNetworkGate
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (newTBQueueIO, newTVarIO)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Backtrace (BacktraceMechanism (..), setBacktraceMechanismState)
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
import Ouroboros.Network.Magic (NetworkMagic (..))

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
  , readNetwork
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
import DbSync.App.Config.Database (DatabaseConfig (..))
import DbSync.App.Config.Types
    ( LedgerConfig(..),
      SyncConfig(..),
      Extractors(..),
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
import DbSync.Phase.Following.Resolver (ConsumedTracking (..))
import DbSync.Phase.Ingest.DedupStore (DedupStores, closeStores)
import DbSync.Phase.Ingest.Consumer (runConsumer)
import DbSync.Phase.Ingest.Gauge (withPipelineGauge)
import DbSync.Phase.Ingest.Genesis (insertByronGenesisDist)
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
  , networkNameFromMagic
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
  , nullLsmSessionTracer
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
import qualified DbSync.Phase.Ingest.Writer as IngestWriter

-- | Returns when 'aaShutdownSignal' fires, or when a linked async
-- crashes and propagates out. Setup steps run in a fixed order, then
-- the boot decision picks one of two terminal paths:
--
--   * 'BootFollowRestart' → 'runBootFollowRestart' takes over and
--     never returns here.
--   * 'BootFresh' / 'BootResume' → 'runIngestThenFollow' runs the
--     Ingest → Prep → Follow pipeline.
runApp :: AppTracer -> AppArgs -> IO ()
runApp tracer args = do
  let validConfig = aaConfig args
      nodeCfg      = aaNodeConfig args
      genesisCfg   = aaGenesisConfig args
      socketPath   = aaSocketPath args
      mShutdown    = aaShutdownSignal args
      topLevelCfg  = mkTopLevelConfig nodeCfg genesisCfg
      networkMagic = getNetworkMagic genesisCfg
      network      = sgNetworkId (scConfig (gcShelley genesisCfg))

  -- Collect a real backtrace at each throw site. IPE frames need
  -- -finfo-table-map, set in cabal.project. The throwers capture their
  -- own SrcInfo, so a HasCallStack annotation would only repeat that
  -- one frozen frame; keep the IPE stack alone.
  setBacktraceMechanismState IPEBacktrace True
  setBacktraceMechanismState HasCallStackBacktrace False

  -- 1. Raise the open-file soft limit before any LSM session opens.
  raiseFdLimit tracer

  -- 2. Shared core environment + startup log line.
  coreEnv <- buildCoreEnv tracer validConfig nodeCfg network
  runAppM coreEnv runStartup

  let ledgerStateDir = aaLedgerStateDir args </> "dbsync-ledger"
  logInfoIO tracer "App" $ "Ledger state dir: " <> toS ledgerStateDir
  logInfoIO tracer "App" $ "Socket: " <> toS socketPath

  -- 3. State-query interpreter handle. Tests pre-seed it; production
  -- starts empty and the receiver fills it from LocalStateQuery.
  stateQueryVar <- maybe (newStateQueryVar topLevelCfg) pure (aaStateQueryVar args)

  -- 4. Database connection settings from the pg-config file.
  let dbCfg   = aaDatabase args
      connStr = TE.encodeUtf8 $ "dbname=" <> dcName dbCfg
      hasqlSettings =
        mconcat
          [ HasqlSettings.hostAndPort (dcHost dbCfg) (fromIntegral (dcPort dbCfg))
          , HasqlSettings.user (dcUser dbCfg)
          , HasqlSettings.password (dcPassword dbCfg)
          , HasqlSettings.dbname (dcName dbCfg)
          ]

  -- 5. Schema check and (re)init. 'freshInit' is 'True' when this run
  -- created the schema. The steps below use it to seed the sync-state
  -- row and to skip the pre-boot rollback check.
  let extractors       = ceExtractors coreEnv
      tableDefs        = concatMap pdTables extractors
      extractorNames   = map pdName extractors
      connStrTxt       = TE.decodeUtf8 connStr
      schemaVersion    = currentSchemaVersion
      ledgerEnabledCfg = lcEnabled (scLedger validConfig)
  freshInit <- setupSchema
    tracer ledgerEnabledCfg ledgerStateDir
    tableDefs extractorNames connStrTxt (aaResyncFromGenesis args)

  -- 6. Open the consumer's control connection, seed
  -- @dbsync_sync_state@ on a fresh schema, then run the two gates.
  consumerCtrlConn <- openControlConnection hasqlSettings
  when freshInit $
    setupFreshSyncState
      tracer consumerCtrlConn connStrTxt
      schemaVersion declaredSchemaFingerprint ledgerEnabledCfg extractorNames
      networkMagic
  runMigrationGate
    tracer consumerCtrlConn schemaVersion declaredSchemaFingerprint extractorNames
  runNetworkGate tracer consumerCtrlConn networkMagic

  -- 7. SystemStart and the ledger subsystem.
  let systemStart = SystemStart (sgSystemStart $ scConfig $ gcShelley genesisCfg)
      pinfo       = mkProtocolInfoCardano nodeCfg genesisCfg
      ledgerCfg   = scLedger validConfig
  hasLedgerEnv <- setupLedgerEnv
    tracer hasqlSettings coreEnv ledgerCfg ledgerStateDir
    genesisCfg pinfo systemStart network

  -- 8. Apply any outstanding rollback request. Skipped after
  -- 'freshInit', because nothing is committed yet.
  unless freshInit $
    handlePreBootRollback
      tracer coreEnv consumerCtrlConn tableDefs hasLedgerEnv
      (aaRollbackToSlot args)

  -- 9. Boot decision. This still runs after a fresh seed, so a stale
  -- 'dbsync-ledger/' against an empty PG aborts with
  -- 'BootSnapshotsWithoutPgState'.
  bootDecision <- do
    mRow <- runAppM consumerCtrlConn readSyncState
    snapshots <- case hasLedgerEnv of
      LedgerEnabled lenv -> listSnapshots (leSnapshotManager lenv)
      LedgerDisabled _   -> pure []
    case decideBoot mRow snapshots ledgerEnabledCfg of
      Left bootErr -> abortBoot tracer bootErr
      Right d      -> pure d

  -- 10. Open the shared LSM session and guarantee its release. The
  -- bracket spans the boot dispatch and the Ingest pipeline, so a
  -- cancellation between 'openLsmSession' and the pipeline's own
  -- 'finally' still drops the on-disk lock. The closer is idempotent,
  -- so the later 'closeAndDeleteLsmSession' in 'runPrep' is harmless.
  -- Swap 'nullLsmSessionTracer' for 'lsmSessionTracerFromApp' to
  -- debug the store itself.
  bracket
    (openLsmSession nullLsmSessionTracer ledgerStateDir)
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
            -- A fresh boot on a schema this run did not create means
            -- the prior leg crashed mid-Ingest, before its first
            -- epoch-boundary commit, so @last_committed_slot@ is NULL.
            -- Orphan rows from that leg survive 'setupSchema' and
            -- would collide with the genesis re-COPY. Purge them.
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
          tracer hasqlSettings connStr coreEnv validConfig
          topLevelCfg networkMagic socketPath systemStart stateQueryVar
          hasLedgerEnv consumerCtrlConn lsmSession tableDefs mShutdown
          (gcByron genesisCfg)

-- ---------------------------------------------------------------------------
-- * Setup steps (in execution order)
-- ---------------------------------------------------------------------------

-- | Compare the database's applied schema version to this binary's and
-- apply the intervening migration files. Aborts when the database is
-- newer than the binary, or when no migration covers the drift.
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

-- | Abort when the database's recorded network magic differs from the
-- configured genesis. Passes quietly while the sync-state row is
-- missing, because 'decideBoot' classifies that case. The comparison
-- runs in the 'Int64' domain of the stored column, so a tampered
-- out-of-range value cannot alias into a match.
runNetworkGate :: AppTracer -> ControlConnection -> NetworkMagic -> IO ()
runNetworkGate tracer ctrlConn networkMagic = do
  mStored <- runAppM ctrlConn readNetwork
  for_ mStored $ \(storedMagic, _storedName) ->
    when (storedMagic /= fromIntegral (unNetworkMagic networkMagic)) $
      abortBoot tracer $
        BootNetworkMismatch
          (NetworkMagic (fromIntegral storedMagic))
          networkMagic
  logInfoIO tracer "App" $
    "Network: " <> networkNameFromMagic networkMagic
      <> " (magic " <> show (unNetworkMagic networkMagic) <> ")"

-- | Classify the boot with 'decideSchemaAction' and run the matching
-- effect: init, drop and init, no-op, or abort.
-- @--resync-from-genesis@ also wipes the ledger state directory, so a
-- stale on-disk ledger cannot attach to a freshly seeded PG.
--
-- Returns 'True' when this run created the schema.
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

-- | Insert the initial @dbsync_sync_state@ row and warn when
-- @wal_level@ is not tuned for bulk load.
--
-- The WAL hint is best-effort: a managed PG operator may lack the
-- privilege to change it. At @wal_level=minimal@ the UNLOGGED →
-- LOGGED flip in 'PreparingForVolatileTail' skips WAL entirely for
-- tables above @wal_skip_threshold@.
setupFreshSyncState
  :: AppTracer
  -> ControlConnection
  -> Text                         -- ^ psql connection string
  -> Int                          -- ^ schema version
  -> Fingerprint                  -- ^ declared schema fingerprint
  -> Bool                         -- ^ @ledger.enabled@
  -> [Text]                       -- ^ enabled extractor names
  -> NetworkMagic
  -> IO ()
setupFreshSyncState tracer ctrl connStrTxt schemaVersion fingerprint ledgerEnabledCfg extractorNames networkMagic = do
  runAppM ctrl $
    seedSyncState
      schemaVersion fingerprint ledgerEnabledCfg extractorNames
      (unNetworkMagic networkMagic) (networkNameFromMagic networkMagic)
  logInfoIO tracer "App" "Sync-state seeded"
  walLevel <- showWalLevel connStrTxt
  unless (walLevel == "minimal") $
    logWarnIO tracer "App" $ T.unlines
      [ "Postgres wal_level is '" <> walLevel <> "'. For fastest bulk-load,"
      , "set the following in postgresql.conf and restart the server:"
      , "  wal_level = minimal"
      , "  max_wal_senders = 0"
      , "  archive_mode = off"
      , "See scripts/postgres-tuning.conf for the full snippet."
      , "Note: replicas will need a full re-base after reverting to"
      , "wal_level = replica. Acceptable on a one-time fresh sync."
      ]

-- | Build the 'HasLedgerEnv' for this run.
--
-- Ledger off: return 'LedgerDisabled' with the minimal env, which is
-- enough to deserialise blocks.
--
-- Ledger on:
--
--   1. Check that the fingerprint in the state directory matches this
--      chain's network magic and system start. Abort on a mismatch,
--      so attaching another chain's ledger cannot corrupt PG.
--   2. Open the LSM session under @\<ledgerStateDir\>/dbsync-ledger@.
--   3. Stamp a fresh fingerprint when the disk carried none.
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
          "Ledger feature disabled (set ledger.enabled = true in the config to opt in); skipping LSM session"
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
--   2. Start the chainsync receiver async and run 'runConsumer' until
--      it exits at the rollback boundary.
--   3. Cancel the receiver, run 'PreparingForVolatileTail' on its own
--      connection, and mark sync complete.
--   4. Flip to 'FollowingVolatileTail' and call 'handoffToFollow'.
--
-- Shutdown is layered, so an exception at any step still releases the
-- loader stream, the tx-out worker, the dedup stores, the UTxO store,
-- and both LSM sessions. The ledger worker and the snapshot writer
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
  tracer hasqlSettings connStr coreEnv validConfig
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
    -- them the per-epoch bulk @UPDATE tx_out@ and @SELECT address@ in
    -- 'DbSync.Worker.TxOut.Worker' hash-join the full heap.
    -- Idempotent.
    createIngestResolveIndexes tracer (unControlConnection consumerCtrlConn)

    -- Allocate per-pipeline state. The caller opened 'lsmSession' and
    -- 'dedupStores'.
    extractStateRef  <- newIORef initialExtractState
    loaderStream     <- mkLoaderStream connStr tableDefs
    -- Cap 300: about three consumer drain batches of headroom, which
    -- absorbs a consumer boundary stall without starving the receiver.
    -- Not deeper, because each slot pins a fully-decoded block and the
    -- consumer is the bulk-sync bottleneck, so a deep queue sits full.
    blockQueue       <- newTBQueueIO 300
    addrBuffer       <- newAddressBufferRef
    txOutWorker      <- mkTxOutWorker tracer hasqlSettings initialAddressId
    mPoolWorker      <-
      setupOffChainPoolWorker tracer hasqlSettings (scExtractors validConfig)
    mVoteWorker      <-
      setupOffChainVoteWorker tracer hasqlSettings (scExtractors validConfig)
    utxoStore        <- openUtxoStore lsmSession
    let consumedByOn = uoConsumedByTxId (exUtxo (scExtractors validConfig))
    mConsumedByBuf <-
      if consumedByOn then Just <$> newConsumedByBufferRef else pure Nothing
    latestPointRef   <- newTVarIO Nothing
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
          , ieControlConnection       = consumerCtrlConn
          , ieLastCommittedSlotAtBoot = replayBoundary
          , ieReplayStartSlot         = replayStart
          , ieLatestReceivedPoint     = latestPointRef
          , ieRollbackBoundary        = rollbackBoundary
          , ieLatestTipBlock          = latestTipBlock
          }

    -- The Byron genesis UTxO distribution is not a chain block, so it
    -- must land before the consumer applies block 1. That makes block
    -- 1's @previous_id@ resolve, and lets later Byron txs that spend
    -- genesis outputs find their inputs.
    case (intersectReq, replayBoundary) of
      (IntersectGenesis, Nothing) ->
        runAppM ingestEnv (insertByronGenesisDist byronCfg)
      _ -> pure ()

    logInfoIO tracer "App" "Starting block ingestion..."

    -- The consumer runs until a block crosses the rollback boundary
    -- (@nodeTip − k@). 'shutdownIngest' releases only the resources
    -- the Ingest pipeline owns; the ledger worker and snapshot writer
    -- survive into Prep and Follow.
    let shutdownIngest             = closeIngestResources tracer loaderStream txOutWorker utxoStore dedupStores
        ingestAction               = runAppM ingestEnv runConsumer `finally` shutdownIngest
        shutdownPostIngest         = closePipelineResources tracer consumerCtrlConn lsmSession hasLedgerEnv mPoolWorker mVoteWorker
        runPrepAndMarkComplete     = runPrep tracer coreEnv hasqlSettings tableDefs lsmSession

    -- The receiver / consumer / ledger-worker tree:
    --   * 'withLedgerThreads' spawns the ledger worker and the
    --     snapshot writer. They live across the Ingest → Prep → Follow
    --     boundary, so the in-RAM 'LedgerDB' keeps ticking during Prep.
    --   * The innermost 'withAsync' is the chainsync receiver. Cancel
    --     it before Prep so the consumer queue stops growing.
    withIOManager (\iomgr ->
      withLedgerThreads hasLedgerEnv replayBoundary stateQueryVar $ do
          withAsync (runAppM ingestEnv $ connectToNode iomgr topLevelCfg networkMagic socketPath intersectReq) $ \nodeThread -> do
            link nodeThread
            withPipelineGauge ingestEnv ingestAction
            cancel nodeThread
          runPrepAndMarkComplete
          runAppM coreEnv (setCurrentPhase (ceCurrentPhase coreEnv) FollowingVolatileTail)
          handoffToFollow
            iomgr ingestEnv tracer hasqlSettings
            topLevelCfg networkMagic socketPath mShutdown
      ) `finally` shutdownPostIngest

-- | Release the resources only 'IngestChainHistory' owns. The
-- consumer's 'finally' calls it, so a mid-flight crash cannot leak
-- the loader stream or the tx-out worker.
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
-- Idempotent on the LSM session, because 'runPrep' may have already
-- deleted it.
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
-- and set @sync_complete@. On success it wipes the ingest LSM scratch
-- directory, which Follow never reads.
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

-- | In-process Ingest → Prep → Follow handoff, called once
-- 'PreparingForVolatileTail' marked sync complete.
--
-- It reads the just-written sync state, builds a 'ResumeContext', and
-- spawns a fresh chainsync receiver against a 'FollowEnv' that reuses
-- the Ingest receiver state. Blocks the cancelled Ingest receiver
-- queued past the rollback boundary stay in the queue, and the Follow
-- consumer processes them.
--
-- Returns when the shutdown signal fires, or when the Follow loop or
-- a linked async ends.
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
    -- opens below and re-intersects at the point the previous receiver
    -- wrote. ChainSync answers with a protocol-mandated confirming
    -- MsgRollBackward to that point. The receiver tags it as a
    -- confirming intersect and enqueues no MsgRollback, so no rows are
    -- deleted.
    logInfoIO tracer "App"
      "Reconnecting chainsync at post-Ingest position; the\
      \ \"Rollback to …\" line that follows is the protocol's\
      \ confirming rollback to the chosen intersection point\
      \ — no rows are deleted from PG."

    -- The Ingest receiver kept reading from the node while the
    -- consumer drained the rollback boundary. Whatever it queued past
    -- the consumer's exit is still in the block queue, and Follow's
    -- consumer INSERTs those before the new receiver's stream arrives.
    buffered <- atomically $ STM.lengthTBQueue (ieBlockQueue ie)
    when (buffered > 0) $
      logInfoIO tracer "App" $
        "FollowingChainTip starting with "
          <> show buffered
          <> " block(s) buffered from Ingest's tail"

    let rc = resumeContextFrom row Nothing
    intersectReq <- resolveIntersection tracer consumerCtrlConn rc

    -- The receiver runs under 'followEnv', so its block queue and
    -- rollback-boundary refs are the ones 'IngestEnv' carried.
    let consumedTracking =
          if uoConsumedByTxId (exUtxo (scExtractors (ceConfig (ieCore ie))))
            then TrackConsumedBy
            else SkipConsumedBy
    runFollowSession tracer "App" iomgr hasqlSettings topLevelCfg
      networkMagic socketPath intersectReq consumedTracking mShutdown
      (mkFollowEnvFromIngest ie)


