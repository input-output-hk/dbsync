{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

import Cardano.Prelude

import qualified Control.Exception as Exception
import Control.Tracer (traceWith)
import System.FilePath (takeDirectory, (</>))

import DbSync.App.Args (AppArgs (..))
import DbSync.App.Run (runApp)
import DbSync.App.Cli (CliArgs (..), parseCliArgs)
import DbSync.App.Config.Types (parseConfig)
import DbSync.App.Config.Genesis (GenesisConfig, readCardanoGenesisConfig)
import DbSync.App.Config.Node (parseDbSyncNodeConfig, parseNodeConfig)
import DbSync.App.Config.Types
  ( DbSyncNodeConfig (..)
  , LoggingConfig (..)
  , NodeConfig
  , SyncConfig (..)
  )
import DbSync.App.Config.Validation (validateConfig)
import DbSync.Error.Render (renderCrash)
import DbSync.Trace.Backend (mkStdErrTracer)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..), severityFromText)

#ifdef GHC_DEBUG
import GHC.Debug.Stub (withGhcDebug)
#endif

main :: IO ()
#ifdef GHC_DEBUG
-- Serve the ghc-debug socket so a client can pause and inspect the heap.
main = withGhcDebug realMain
#else
main = realMain
#endif

realMain :: IO ()
realMain = do
  -- Bootstrap tracer so profile-parse errors get logged before the
  -- profile-configured tracer exists.
  args       <- parseCliArgs
  bootTracer <- mkStdErrTracer Info
  let bootLogError msg = traceWith bootTracer $ LogMsg Error "App" msg

  -- 1. Profile (database, sync options, ledger flag, logging).
  validProfile <- loadProfile bootLogError (caProfile args)

  -- 2. Rebuild the tracer at the profile-configured severity.
  let minSeverity = severityFromText (lgLevel (scLogging validProfile))
  tracer <- mkStdErrTracer minSeverity
  let logError msg = traceWith tracer $ LogMsg Error "App" msg
      logInfo  msg = traceWith tracer $ LogMsg Info  "App" msg

  -- 3. db-sync-config: provides the cardano-node config path.
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
    `Exception.catch` handleFatalError tracer

-- ---------------------------------------------------------------------------
-- * Top-level error handling
-- ---------------------------------------------------------------------------

-- | Render an unhandled exception escaping 'runApp' into the app log
-- and exit non-zero. 'ExitCode's pass through so an intentional exit
-- keeps its status.
handleFatalError :: AppTracer -> SomeException -> IO ()
handleFatalError tracer e = case fromException e of
  Just ec -> Exception.throwIO (ec :: ExitCode)
  Nothing -> do
    traceWith tracer $ LogMsg Error "App" (renderCrash e)
    exitFailure

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
