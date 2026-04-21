module Main
  ( main
  ) where

import Cardano.Prelude

import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Exit (exitFailure)

import Control.Concurrent.STM (newTBQueueIO)
import DbSync.App (buildCoreEnv, runStartup)
import DbSync.Cli (parseCliArgs, CliArgs (..))
import DbSync.Config (parseConfig)
import DbSync.Config.Genesis (readCardanoGenesisConfig, mkTopLevelConfig)
import DbSync.Config.Node (parseDbSyncNodeConfig, parseNodeConfig)
import DbSync.Config.Types (DbSyncNodeConfig (..))
import DbSync.Node.Connection (connectToNode, getNetworkMagic)
import Ouroboros.Network.NodeToClient (withIOManager)
import System.FilePath (takeDirectory, (</>))
import DbSync.Config.Validation (validateConfig)
import DbSync.Trace.Backend (mkStdErrTracer)
import DbSync.Trace.Types (Severity (..))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- 1. Parse CLI
  args <- parseCliArgs

  -- 2. Parse profile (database, sync options, logging)
  profileResult <- parseConfig (caProfile args)
  profile <- case profileResult of
    Left err -> do
      putTextLn $ "Error parsing profile: " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 3. Validate profile
  validProfile <- case validateConfig profile of
    Left errs -> do
      putTextLn "Profile validation errors:"
      for_ errs $ \err -> putTextLn $ "  - " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 4. Parse db-sync-config.json → extract NodeConfigFile → parse node config
  dbSyncCfgResult <- parseDbSyncNodeConfig (caDbSyncConfig args)
  dbSyncCfg <- case dbSyncCfgResult of
    Left err -> do
      putTextLn $ "Error parsing db-sync-config.json: " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 5. Resolve NodeConfigFile relative to db-sync-config.json directory, then parse
  let configDir = takeDirectory (caDbSyncConfig args)
      nodeConfigPath = configDir </> dscNodeConfigFile dbSyncCfg
  nodeCfgResult <- parseNodeConfig nodeConfigPath
  nodeCfg <- case nodeCfgResult of
    Left err -> do
      putTextLn $ "Error parsing node config (" <> toS nodeConfigPath <> "): " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 6. Build environment
  tracer <- mkStdErrTracer Info
  env <- buildCoreEnv tracer validProfile nodeCfg

  -- 7. Startup logging
  runStartup env

  -- 8. Read genesis files → TopLevelConfig
  genesisResult <- readCardanoGenesisConfig nodeCfg configDir
  genesisCfg <- case genesisResult of
    Left err -> do
      putTextLn $ "Error reading genesis files: " <> show err
      exitFailure
    Right gc -> pure gc

  let topLevelCfg = mkTopLevelConfig nodeCfg genesisCfg
      networkMagic = getNetworkMagic genesisCfg

  putTextLn $ "State dir: " <> toS (caStateDir args)
  putTextLn $ "Socket: " <> toS (caSocketPath args)

  -- 9. Connect to node and start receiving blocks
  blockQueue <- newTBQueueIO 100
  withIOManager $ \iomgr ->
    connectToNode tracer iomgr topLevelCfg networkMagic (caSocketPath args) blockQueue

