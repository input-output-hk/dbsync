module Main
  ( main
  ) where

import Cardano.Prelude

import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Exit (exitFailure)

import DbSync.App (buildCoreEnv, runStartup)
import DbSync.Cli (parseCliArgs, CliArgs (..))
import DbSync.Config (parseConfig)
import DbSync.Config.Node (parseNodeConfig)
import DbSync.Config.Validation (validateConfig)
import DbSync.Trace.Backend (mkStdErrTracer)
import DbSync.Trace.Types (Severity (..))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- 1. Parse CLI
  args <- parseCliArgs

  -- 2. Parse db-sync config
  syncCfgResult <- parseConfig (caConfig args)
  syncCfg <- case syncCfgResult of
    Left err -> do
      putTextLn $ "Error parsing config: " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 3. Validate config
  validCfg <- case validateConfig syncCfg of
    Left errs -> do
      putTextLn "Config validation errors:"
      for_ errs $ \err -> putTextLn $ "  - " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 4. Parse node config
  nodeCfgResult <- parseNodeConfig (caNodeConfig args)
  nodeCfg <- case nodeCfgResult of
    Left err -> do
      putTextLn $ "Error parsing node config: " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 5. Build environment
  tracer <- mkStdErrTracer Info
  env <- buildCoreEnv tracer validCfg nodeCfg

  -- 6. Startup logging
  runStartup env

  -- TODO: phase detection and phase execution
  putTextLn "cardano-db-sync: startup complete, phase detection not yet implemented"

