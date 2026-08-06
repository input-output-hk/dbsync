{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

import Cardano.Prelude

import qualified Control.Exception as Exception
import Control.Tracer (traceWith)
import System.FilePath (takeDirectory)

import DbSync.App.Args (AppArgs (..))
import DbSync.App.Banner (printBanner)
import DbSync.App.Run (runApp)
import DbSync.App.Cli (CliArgs (..), parseCliArgs)
import DbSync.App.Config.Database (DatabaseConfig, parseDatabaseConfig)
import DbSync.App.Config.Genesis (GenesisConfig, readCardanoGenesisConfig)
import DbSync.App.Config.Node (parseNodeConfig)
import DbSync.App.Config.Types
  ( LoggingConfig (..)
  , NodeConfig
  , SyncConfig (..)
  , parseConfig
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
  args <- parseCliArgs
  printBanner

  -- Bootstrap tracer so config-parse errors get logged before the
  -- configured tracer exists.
  bootTracer <- mkStdErrTracer Info
  let bootLogError msg = traceWith bootTracer $ LogMsg Error "App" msg

  -- 1. Behaviour config (sync mode, ledger flag, db profile, logging).
  validConfig <- loadConfig bootLogError (caConfig args)

  -- 2. Rebuild the tracer at the configured severity.
  let minSeverity = severityFromText (lgLevel (scLogging validConfig))
  tracer <- mkStdErrTracer minSeverity
  let logError msg = traceWith tracer $ LogMsg Error "App" msg
      logInfo  msg = traceWith tracer $ LogMsg Info  "App" msg

  -- 3. PostgreSQL connection settings (password_file resolved here).
  dbConfig <- loadPgConfig logError (caPgConfig args)

  -- 4. cardano-node config (era boundaries, genesis hashes).
  nodeCfg <- loadNodeConfig logError (caNodeConfig args)

  -- 5. Genesis files (all eras), resolved relative to the node config.
  let configDir = takeDirectory (caNodeConfig args)
  genesisCfg <- loadGenesis logError logInfo nodeCfg configDir

  runApp tracer AppArgs
    { aaConfig            = validConfig
    , aaDatabase          = dbConfig
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

loadConfig :: (Text -> IO ()) -> FilePath -> IO SyncConfig
loadConfig logError path = do
  result <- parseConfig path
  cfg <- case result of
    Left err -> logError ("Error parsing config: " <> show err) >> exitFailure
    Right cfg -> pure cfg
  case validateConfig cfg of
    Left errs -> do
      logError "Config validation errors:"
      for_ errs $ \err -> logError $ "  - " <> show err
      exitFailure
    Right valid -> pure valid

loadPgConfig :: (Text -> IO ()) -> FilePath -> IO DatabaseConfig
loadPgConfig logError path = do
  result <- parseDatabaseConfig path
  case result of
    Left err -> do
      logError $ "Error parsing pg-config (" <> toS path <> "): " <> show err
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
