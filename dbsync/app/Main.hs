module Main
  ( main
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)
import System.FilePath (takeDirectory, (</>))

import DbSync.App.Args (AppArgs (..))
import DbSync.App.Run (runApp)
import DbSync.Cli (CliArgs (..), parseCliArgs)
import DbSync.Config (parseConfig)
import DbSync.Config.Genesis (GenesisConfig, readCardanoGenesisConfig)
import DbSync.Config.Node (parseDbSyncNodeConfig, parseNodeConfig)
import DbSync.Config.Types
  ( DbSyncNodeConfig (..)
  , LoggingConfig (..)
  , NodeConfig
  , SyncConfig (..)
  )
import DbSync.Config.Validation (validateConfig)
import DbSync.Trace.Backend (mkStdErrTracer)
import DbSync.Trace.Types (LogMsg (..), Severity (..), severityFromText)

main :: IO ()
main = do
  -- Bootstrap tracer (Info) so profile-parse errors surface before
  -- the profile-configured tracer is built.
  args       <- parseCliArgs
  bootTracer <- mkStdErrTracer Info
  let bootLogError msg = traceWith bootTracer $ LogMsg Error "App" msg Nothing

  -- 1. Profile (database, sync options, ledger flag, logging).
  validProfile <- loadProfile bootLogError (caProfile args)

  -- 2. Rebuild the tracer at the profile-configured severity. The
  --    watchdog + per-epoch diagnostics gate on the same value.
  let minSeverity = severityFromText (lgLevel (scLogging validProfile))
  tracer <- mkStdErrTracer minSeverity
  let logError msg = traceWith tracer $ LogMsg Error "App" msg Nothing
      logInfo  msg = traceWith tracer $ LogMsg Info  "App" msg Nothing

  -- 3. db-sync-config (cardano-book shape: pulls out NodeConfigFile).
  dbSyncCfg <- loadDbSyncConfig logError (caDbSyncConfig args)

  -- 4. cardano-node config (era boundaries, genesis hashes).
  let configDir = takeDirectory (caDbSyncConfig args)
      nodePath  = configDir </> dscNodeConfigFile dbSyncCfg
  nodeCfg <- loadNodeConfig logError nodePath

  -- 5. Genesis files (all eras).
  genesisCfg <- loadGenesis logError logInfo nodeCfg configDir

  runApp tracer AppArgs
    { aaProfile           = validProfile
    , aaNodeConfig        = nodeCfg
    , aaGenesisConfig     = genesisCfg
    , aaSocketPath        = caSocketPath args
    , aaLedgerStateDir    = caLedgerStateDir args
    , aaResyncFromGenesis = caResyncFromGenesis args
    , aaRollbackToSlot    = caRollbackToSlot args
    , aaShutdownSignal    = Nothing
    , aaStateQueryVar     = Nothing
    }

-- ---------------------------------------------------------------------------
-- * Config loading helpers
-- ---------------------------------------------------------------------------

loadProfile :: (Text -> IO ()) -> FilePath -> IO SyncConfig
loadProfile logError path = do
  profileResult <- parseConfig path
  profile <- case profileResult of
    Left err -> logError ("Error parsing profile: " <> show err) >> exitFailure
    Right cfg -> pure cfg
  case validateConfig profile of
    Left errs -> do
      logError "Profile validation errors:"
      for_ errs $ \err -> logError $ "  - " <> show err
      exitFailure
    Right cfg -> pure cfg

loadDbSyncConfig :: (Text -> IO ()) -> FilePath -> IO DbSyncNodeConfig
loadDbSyncConfig logError path = do
  result <- parseDbSyncNodeConfig path
  case result of
    Left err -> do
      logError $ "Error parsing db-sync-config.json: " <> show err
      exitFailure
    Right cfg -> pure cfg

loadNodeConfig :: (Text -> IO ()) -> FilePath -> IO NodeConfig
loadNodeConfig logError path = do
  result <- parseNodeConfig path
  case result of
    Left err -> do
      logError $ "Error parsing node config (" <> toS path <> "): " <> show err
      exitFailure
    Right cfg -> pure cfg

loadGenesis
  :: (Text -> IO ())
  -> (Text -> IO ())
  -> NodeConfig
  -> FilePath
  -> IO GenesisConfig
loadGenesis logError logInfo nodeCfg configDir = do
  result <- readCardanoGenesisConfig nodeCfg configDir
  case result of
    Left err -> do
      logError $ "Error reading genesis files: " <> show err
      exitFailure
    Right gc -> do
      logInfo "Genesis files loaded successfully"
      pure gc
