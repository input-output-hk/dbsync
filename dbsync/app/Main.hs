module Main
  ( main
  ) where

import Cardano.Prelude

import Control.Concurrent.STM (newTBQueueIO)
import Control.Tracer (traceWith)
import Data.IORef (newIORef)
import System.FilePath (takeDirectory, (</>))

import DbSync.App (buildCoreEnv, runStartup)
import DbSync.AppM (runAppM)
import DbSync.Cli (parseCliArgs, CliArgs (..))
import DbSync.Config (parseConfig)
import DbSync.Config.Genesis (readCardanoGenesisConfig, mkProtocolInfoCardano, mkTopLevelConfig, GenesisConfig (..), ShelleyConfig (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import DbSync.Config.Node (parseDbSyncNodeConfig, parseNodeConfig)
import DbSync.Config.Types (DbSyncNodeConfig (..), DatabaseConfig (..), SyncConfig (..))
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
import DbSync.Extractor (ExtractState (..), ExtractorDef (..))
import DbSync.Id.Counter (IdCounters (..), mkIdCounter)
import DbSync.Id.DedupMap (newMaps)
import DbSync.Ingest.Consumer (runConsumer)
import DbSync.Ingest.ReceiverStats (newReceiverStats)
import DbSync.Ledger.Types (HasLedgerEnv (..), mkNoLedgerEnv)
import DbSync.Node.Connection (connectToNode, getNetworkMagic)
import DbSync.Resolver.Ingest (mkIngestResolver)
import DbSync.StateQuery (newStateQueryVar)
import DbSync.Trace.Backend (mkStdErrTracer)
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Writer.CopyAdapter (mkCopyWriterAdapter)

import qualified Data.Text.Encoding as TE

import Cardano.Network.NodeToClient (withIOManager)

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

  -- 6. Build the shared core environment
  coreEnv <- buildCoreEnv tracer validProfile nodeCfg

  -- 7. Startup logging
  runAppM coreEnv runStartup

  -- 8. Read genesis files → TopLevelConfig
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

  logInfo $ "State dir: " <> toS (caStateDir args)
  logInfo $ "Socket: " <> toS (caSocketPath args)

  -- 9. Build StateQueryVar for epoch/slot computation via HardFork Interpreter.
  -- The TopLevelConfig is passed in so the locally-observed fallback
  -- summary can extract per-era 'EraParams' from consensus rather than
  -- shipping its own copy.
  stateQueryVar <- newStateQueryVar topLevelCfg

  -- 10. Database connection string from profile
  let dbCfg = scDatabase validProfile
      connStr = TE.encodeUtf8 $ "dbname=" <> dcName dbCfg

  -- 11. Schema check + (re)init
  --
  -- Decision matrix:
  --   --force-resync → drop everything and re-init.
  --   schema_version absent → fresh DB, run init.
  --   schema_version matches → skip init, resume.
  --   schema_version mismatched → abort with operator-facing diagnostics.
  let extractors = ceExtractors coreEnv
      tableDefs  = concatMap pdTables extractors
      versions   = map (\e -> (pdName e, pdVersion e)) extractors
      connStrTxt = TE.decodeUtf8 connStr
  schemaState <- checkSchemaVersions versions connStrTxt
  case decideSchemaAction (caForceResync args) schemaState of
    ActionSkipInit -> do
      logInfo "Schema present and matches expected versions; skipping init"
    ActionRunInit -> do
      logInfo "Fresh database detected; creating schema"
      initSchema tableDefs versions connStrTxt
      logInfo "Schema ready"
    ActionForceReinit -> do
      logInfo "--force-resync: dropping existing schema and re-initialising"
      dropSchema tableDefs versions connStrTxt
      initSchema tableDefs versions connStrTxt
      logInfo "Schema ready"
    ActionAbort errs -> do
      logError "Schema mismatch — refusing to start. Use --force-resync to wipe and re-sync."
      for_ errs $ \err -> logError $ "  - " <> renderSchemaMismatch err
      exitFailure

  -- 12. Build the ingest pipeline state
  stRef         <- newIORef mkInitState
  dedupMaps     <- newMaps
  copyWriter    <- mkCopyWriter connStr tableDefs
  blockQueue    <- newTBQueueIO 500
  receiverStats <- newReceiverStats

  let resolver = mkIngestResolver stRef dedupMaps
      writer   = mkCopyWriterAdapter copyWriter

  -- SystemStart needed by the state query interpreter
  let systemStart = SystemStart (sgSystemStart $ scConfig $ gcShelley genesisCfg)
      pinfo       = mkProtocolInfoCardano nodeCfg genesisCfg
      network     = sgNetworkId (scConfig (gcShelley genesisCfg))

  hasLedgerEnv <- LedgerDisabled <$> mkNoLedgerEnv tracer pinfo systemStart network

  let ingestEnv = IngestEnv
        { ieCore          = coreEnv
        , ieBlockQueue    = blockQueue
        , ieCopyWriter    = copyWriter
        , ieDedupMaps     = dedupMaps
        , ieHasLedgerEnv  = hasLedgerEnv
        , ieStateQueryVar = stateQueryVar
        , ieSystemStart   = systemStart
        , ieResolver      = resolver
        , ieWriter        = writer
        , ieExtractState  = stRef
        , ieReceiverStats = receiverStats
        }

  -- 13. Start block reception + consumer
  logInfo "Starting block ingestion..."

  withIOManager $ \iomgr ->
    withAsync (runAppM ingestEnv $ connectToNode iomgr topLevelCfg networkMagic (caSocketPath args)) $ \_nodeThread -> do
      -- Consumer runs on the main thread; node receiver runs on async thread
      -- If either throws, the other is cancelled (withAsync guarantee)
      runAppM ingestEnv runConsumer
        `finally` do
          logInfo "Shutting down COPY writer..."
          cwCommit copyWriter `catch` \(e :: SomeException) ->
            logError $ "Error during final commit: " <> show e
          closeCopyWriter copyWriter

-- | Initial extraction state for IngestChainHistory.
-- Dedup maps are created separately via 'newMaps' (mutable hash tables).
mkInitState :: ExtractState
mkInitState = ExtractState
  { esIdCounters = IdCounters
      { icBlockId            = mkIdCounter 1
      , icTxId               = mkIdCounter 1
      , icTxOutId            = mkIdCounter 1
      , icTxInId             = mkIdCounter 1
      , icCollateralTxInId   = mkIdCounter 1
      , icReferenceTxInId    = mkIdCounter 1
      , icTxMetadataId       = mkIdCounter 1
      , icMaTxMintId         = mkIdCounter 1
      , icMaTxOutId          = mkIdCounter 1
      , icSlotLeaderId       = mkIdCounter 1
      , icStakeAddressId     = mkIdCounter 1
      , icPoolHashId         = mkIdCounter 1
      , icMultiAssetId       = mkIdCounter 1
      , icScriptId              = mkIdCounter 1
      , icStakeRegistrationId   = mkIdCounter 1
      , icStakeDeregistrationId = mkIdCounter 1
      , icDelegationId          = mkIdCounter 1
      , icWithdrawalId          = mkIdCounter 1
      , icPoolUpdateId          = mkIdCounter 1
      , icPoolMetadataRefId     = mkIdCounter 1
      , icPoolOwnerId           = mkIdCounter 1
      , icPoolRetireId          = mkIdCounter 1
      , icPoolRelayId           = mkIdCounter 1
      , icTxCborId              = mkIdCounter 1
      , icEpochSyncStatsId      = mkIdCounter 1
      }
  , esLastBlockId = Nothing
  }
