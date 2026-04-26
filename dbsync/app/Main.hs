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
import DbSync.Config.Genesis (readCardanoGenesisConfig, mkTopLevelConfig, GenesisConfig (..), ShelleyConfig (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import DbSync.Config.Node (parseDbSyncNodeConfig, parseNodeConfig)
import DbSync.Config.Types (DbSyncNodeConfig (..), DatabaseConfig (..), SyncConfig (..))
import DbSync.Config.Validation (validateConfig)
import DbSync.Copy.Writer (CopyWriter (..), mkCopyWriter, closeCopyWriter)
import DbSync.Db.Schema.Init (initSchema)
import DbSync.Env (CoreEnv (..))
import DbSync.Extractor (ExtractState (..), ExtractorDef (..))
import DbSync.Id.Counter (IdCounters (..), mkIdCounter)
import DbSync.Id.DedupMap (newMaps)
import DbSync.Ingest.Consumer (runConsumer)
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

  -- 6. Build environment
  env <- buildCoreEnv tracer validProfile nodeCfg

  -- 7. Startup logging
  runAppM env runStartup

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

  -- 9. Build StateQueryVar for epoch/slot computation via HardFork Interpreter
  stateQueryVar <- newStateQueryVar

  -- 10. Database connection string from profile
  let dbCfg = scDatabase validProfile
      connStr = TE.encodeUtf8 $ "dbname=" <> dcName dbCfg

  -- 11. Schema creation (idempotent)
  let extractors = ceExtractors env
      tableDefs = concatMap pdTables extractors
      versions = map (\e -> (pdName e, pdVersion e)) extractors
  logInfo "Creating schema..."
  initSchema tableDefs versions (TE.decodeUtf8 connStr)
  logInfo "Schema ready"

  -- 12. Build the ingest pipeline
  stRef <- newIORef mkInitState
  dedupMaps <- newMaps
  copyWriter <- mkCopyWriter connStr tableDefs
  let resolver = mkIngestResolver stRef dedupMaps
      writer   = mkCopyWriterAdapter copyWriter

  -- 13. Start block reception + consumer
  blockQueue <- newTBQueueIO 500
  logInfo "Starting block ingestion..."

  -- SystemStart needed by the state query interpreter
  let systemStart = SystemStart (sgSystemStart $ scConfig $ gcShelley genesisCfg)

  withIOManager $ \iomgr ->
    withAsync (connectToNode tracer iomgr topLevelCfg networkMagic (caSocketPath args) blockQueue stateQueryVar) $ \_nodeThread -> do
      -- Consumer runs on the main thread; node receiver runs on async thread
      -- If either throws, the other is cancelled (withAsync guarantee)
      runConsumer tracer stateQueryVar systemStart extractors blockQueue resolver writer copyWriter stRef
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
