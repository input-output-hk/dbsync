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
  --    TODO: currently parses --node-config path directly as node config.
  --    Need to add db-sync-config.json parsing to extract NodeConfigFile first.
  nodeCfgResult <- parseNodeConfig (caNodeConfig args)
  nodeCfg <- case nodeCfgResult of
    Left err -> do
      putTextLn $ "Error parsing node config: " <> show err
      exitFailure
    Right cfg -> pure cfg

  -- 5. Build environment
  tracer <- mkStdErrTracer Info
  env <- buildCoreEnv tracer validProfile nodeCfg

  -- 6. Startup logging
  runStartup env

  -- TODO: read genesis files → TopLevelConfig → connect to node
  putTextLn $ "State dir: " <> toS (caStateDir args)
  putTextLn $ "Socket: " <> toS (caSocketPath args)
  putTextLn "cardano-db-sync: startup complete, genesis reading not yet implemented"

