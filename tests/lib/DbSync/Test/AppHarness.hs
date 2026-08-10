{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Build an 'AppArgs' from a 'MockNode' so tests can call
-- 'DbSync.App.Run.runApp' directly against the same code path
-- production uses.
--
-- All 'AppArgs' point at the @dbsync_test@ database. Tests that
-- need different behaviour settings build their own 'SyncConfig'
-- via 'configWithExtractors'.
module DbSync.Test.AppHarness
  ( -- * Config builders
    defaultTestConfig
  , ledgerEnabledTestConfig
  , configWithExtractors
  , allImplementedExtractors

    -- * Config introspection
  , configTableNames
  , configTableDefs
  , configExpectedIndexes

    -- * AppArgs builders
  , mkAppArgsFromMockNode
  , mkAppArgsFromMockNodeResume
  , withTempDir

    -- * Tracer selection
  , quietTracer
  , verboseTracer

    -- * Sync-state polling
  , waitForSyncComplete
  , waitFor

    -- * Shutdown signal plumbing
  , newShutdown
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..), diffUTCTime, getCurrentTime)
import Data.Time.Calendar (fromGregorian)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)

import qualified Data.List as List

import DbSync.App (buildExtractors)
import DbSync.App.Args (AppArgs (..))
import DbSync.Db.Schema.SyncState (syncStateTableDef)
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Indexes (uniqueConstraintIndexName)
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Trace.Backend (mkNullTracer, mkStdErrTracer)
import DbSync.Trace.Types (AppTracer, Severity)
import DbSync.App.Config.Database (DatabaseConfig (..))
import DbSync.App.Config.Types
  ( LedgerConfig (..)
  , LogFormat (..)
  , LoggingConfig (..)
  , MetricsConfig (..)
  , SyncConfig (..)
  , OptionFlag (..)
  , Extractors (..)
  , SyncMode (..)
  , SyncSettings (..)
  , UtxoOption (..)
  , defaultLedgerBackend
  , defaultSnapshotNearTipEpoch
  , defaultUtxoOption
  )
import DbSync.Test.Database (queryTestDb, testDbName)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockChain (MockChain (..))
import DbSync.Test.MockNode (MockNode (..))
import DbSync.Test.PgAssertions (tableColumn)

-- ---------------------------------------------------------------------------
-- * Config builders
-- ---------------------------------------------------------------------------

-- | The standard test config: every currently-implemented
-- extractor enabled. Matches what an "everything" production
-- config would do for the extractors the codebase has landed.
defaultTestConfig :: SyncConfig
defaultTestConfig = configWithExtractors allImplementedExtractors

-- | Same as 'defaultTestConfig' but with the ledger feature on.
-- Tests that exercise the LedgerWorker / snapshot writer / Follow
-- restart snapshot loading need ledger enabled.
--
-- The snapshot near-tip threshold is lowered to @2@ so snapshots
-- fire on the short fixture chains; production default of @580@
-- would mean no snapshot ever lands during a typical test run.
ledgerEnabledTestConfig :: SyncConfig
ledgerEnabledTestConfig =
  defaultTestConfig
    { scLedger = LedgerConfig
        { lcEnabled              = True
        , lcBackend              = defaultLedgerBackend
        , lcSnapshotNearTipEpoch = 2
        }
    }

-- | All extractors with a real (non-stub) implementation today —
-- see 'DbSync.App.resolveExtractor'. Skipped: @scripts_datums@,
-- @governance@, @current_state@ (stubs).
allImplementedExtractors :: Extractors
allImplementedExtractors = Extractors
  { exUtxo                  = defaultUtxoOption { uoEnabled = True }
  , exMultiAsset            = OptionFlag True
  , exMetadata              = OptionFlag True
  , exStakeDelegation       = OptionFlag True
  , exStakeDelegationLedger = OptionFlag False
  , exPool                  = OptionFlag True
  , exScriptsDatums         = OptionFlag False
  , exGovernance            = OptionFlag False
  , exCbor                  = OptionFlag True
  , exEpochSyncStats        = OptionFlag True
  , exEpochBoundary         = OptionFlag True
  , exPoolStats             = OptionFlag False
  , exEpoch                 = OptionFlag True
  , exCurrentState          = OptionFlag False
  , exOffChainPools         = OptionFlag False
  , exOffChainVotes         = OptionFlag False
  }

-- | A ledger-off config with a caller-supplied 'Extractors'.
configWithExtractors :: Extractors -> SyncConfig
configWithExtractors opts = SyncConfig
  { scSync = SyncSettings
      { ssMode = SyncModeAuto
      }
  , scLedger = LedgerConfig
      { lcEnabled              = False
      , lcBackend              = defaultLedgerBackend
      , lcSnapshotNearTipEpoch = defaultSnapshotNearTipEpoch
      }
  , scExtractors = opts
  , scMetrics = MetricsConfig { mcPrometheusPort = 9999 }
  , scLogging = LoggingConfig
      { lgLevel  = "info"
      , lgFormat = LogFormatText
      }
  }

-- ---------------------------------------------------------------------------
-- * Config introspection
-- ---------------------------------------------------------------------------

-- | Names of every table the enabled extractors in this config own.
-- Tests use this to iterate the schema-flip / index / sequence
-- assertions instead of hard-coding a stale list.
--
-- Returns the empty list if the config is malformed (e.g. an
-- extractor depends on something disabled). The same configuration
-- would refuse to run via 'runApp', so test calls that drove a real
-- sync first don't hit this case.
configTableNames :: SyncConfig -> [Text]
configTableNames = map tdName . configTableDefs

-- | The 'TableDef's behind 'configTableNames'. Tests that need to drop
-- or re-create the schema for a config need the definitions, not just
-- the names.
configTableDefs :: SyncConfig -> [TableDef]
configTableDefs cfg = case buildExtractors (scExtractors cfg) of
  Right exts -> concatMap pdTables exts
  Left _err  -> []

-- | Names of every index that 'PreparingForVolatileTail' should have
-- created by the time it marks @sync_complete = true@.
--
-- Derived from 'TableDef' metadata on the active extractor tables:
-- tables without a declared primary key yield @<table>_pkey_idx@ on
-- @id@ (a declared PK carries its constraint index from CREATE
-- TABLE, exactly as 'DbSync.Db.Statement.Indexes.tableIndexStatements'
-- treats it); each entry in 'tdUniqueConstraints' yields
-- @<table>_unique_N_idx@.
--
-- The resolve-support scaffolding is intentionally absent: Prep
-- drops it before the UNLOGGED → LOGGED flip (see
-- 'DbSync.Db.Statement.Indexes.resolveScaffoldingIndexNames').
configExpectedIndexes :: SyncConfig -> [Text]
configExpectedIndexes cfg = case buildExtractors (scExtractors cfg) of
  Left _err  -> []
  Right exts ->
    List.nub (concatMap tableIndexNames (concatMap pdTables exts))

-- | Index names a single 'TableDef' contributes to the post-Prep
-- schema. Mirrors 'DbSync.Db.Statement.Indexes.tableIndexStatements'
-- without invoking the SQL builder.
tableIndexNames :: TableDef -> [Text]
tableIndexNames td =
  pkIdx <> uniqueIdxs
  where
    pkIdx = case tdPrimaryKey td of
      Just _  -> []
      Nothing -> [tdName td <> "_pkey_idx"]
    uniqueIdxs =
      zipWith
        (\n _ -> uniqueConstraintIndexName td n)
        [1 ..]
        (tdUniqueConstraints td)

-- ---------------------------------------------------------------------------
-- * AppArgs builders
-- ---------------------------------------------------------------------------

-- | Build 'AppArgs' pointing at the mock node, with
-- @aaResyncFromGenesis = True@ — the first-run scenario. Reuses the
-- 'MockChain'\'s pre-seeded state-query handle so 'parseBlock' can
-- compute slot details without waiting on the mock server's
-- stubbed LocalStateQuery responder (which never replies).
mkAppArgsFromMockNode
  :: SyncConfig
  -> MockNode
  -> FilePath            -- ^ scratch dir for ledger state (ignored when ledger off)
  -> Maybe (IO ())       -- ^ optional shutdown signal
  -> AppArgs
mkAppArgsFromMockNode = mkAppArgsWithResync True

-- | Same as 'mkAppArgsFromMockNode' but with
-- @aaResyncFromGenesis = False@. Used by restart tests: the second
-- 'runApp' invocation must resume against the existing DB and
-- ledger directory rather than wiping them.
mkAppArgsFromMockNodeResume
  :: SyncConfig
  -> MockNode
  -> FilePath
  -> Maybe (IO ())
  -> AppArgs
mkAppArgsFromMockNodeResume = mkAppArgsWithResync False

mkAppArgsWithResync
  :: Bool
  -> SyncConfig
  -> MockNode
  -> FilePath
  -> Maybe (IO ())
  -> AppArgs
mkAppArgsWithResync resync cfg mn ledgerDir mShutdown = AppArgs
  { aaConfig            = cfg
  , aaDatabase          = testDatabaseConfig
  , aaNodeConfig        = mcNodeConfig (mnChain mn)
  , aaGenesisConfig     = mcGenesisConfig (mnChain mn)
  , aaSocketPath        = mnSocketPath mn
  , aaLedgerStateDir    = ledgerDir
  , aaResyncFromGenesis = resync
  , aaRollbackToSlot    = Nothing
  , aaShutdownSignal    = mShutdown
  , aaStateQueryVar     = Just (mcStateQueryVar (mnChain mn))
  }

-- | Connection to the local @dbsync_test@ database used by every
-- test 'AppArgs'.
testDatabaseConfig :: DatabaseConfig
testDatabaseConfig = DatabaseConfig
  { dcHost     = "localhost"
  , dcPort     = 5432
  , dcName     = testDbName
  , dcUser     = ""
  , dcPassword = ""
  }

-- | Allocate a tmp dir under @/tmp@ for the action; remove it on
-- exit (best-effort).
withTempDir :: Text -> (FilePath -> IO a) -> IO a
withTempDir prefix = bracket alloc cleanup
  where
    alloc = do
      now <- getCurrentTime
      let stamp :: Integer
          stamp = floor (realToFrac (diffUTCTime now epoch) * 1_000_000 :: Double)
          path  = "/tmp/" <> T.unpack prefix <> "-" <> show stamp
      createDirectoryIfMissing True path
      pure path

    cleanup path =
      removeDirectoryRecursive path
        `catch` \(_ :: SomeException) -> pure ()

    epoch :: UTCTime
    epoch = UTCTime (fromGregorian 1970 1 1) 0

-- ---------------------------------------------------------------------------
-- * Tracer selection
-- ---------------------------------------------------------------------------

-- | Discards every trace. Default choice for stable specs that
-- don't need to debug the app's behaviour.
quietTracer :: IO AppTracer
quietTracer = pure mkNullTracer

-- | Writes traces to stderr at or above the given severity. Use
-- when diagnosing a failing spec: @verboseTracer Info@ surfaces the
-- per-step Prep timing, ChainSync handshake, and shutdown lines;
-- @verboseTracer Debug@ also enables the per-epoch dedup/RAM
-- diagnostics.
verboseTracer :: Severity -> IO AppTracer
verboseTracer = mkStdErrTracer

-- ---------------------------------------------------------------------------
-- * Sync-state polling
-- ---------------------------------------------------------------------------

-- | Poll @dbsync_sync_state.sync_complete@ until it reads true or
-- @timeoutSecs@ elapses.
waitForSyncComplete :: Int -> IO ()
waitForSyncComplete = waitFor "sync_complete=true" syncCompletePredicate
  where
    syncCompletePredicate = do
      result <-
        queryTestDb
          ( "SELECT " <> tableColumn syncStateTableDef "sync_complete"
              <> " FROM " <> tdName syncStateTableDef <> " LIMIT 1"
          )
          `catch` \(_ :: SomeException) -> pure ""
      pure (T.strip result == "t")

-- ---------------------------------------------------------------------------
-- * Shutdown signal plumbing
-- ---------------------------------------------------------------------------

-- | A one-shot shutdown signal expressed as a @(fire, wait)@ pair.
--
-- @fire@ is what the test calls when it wants 'runApp' to exit.
-- @wait@ blocks until the signal has fired; the test passes it as
-- @aaShutdownSignal@ via 'mkAppArgsFromMockNode' and 'runApp' races
-- it against the 'FollowingChainTip' loop.
newShutdown :: IO (IO (), IO ())
newShutdown = do
  mv <- newEmptyMVar
  pure (putMVar mv (), takeMVar mv)
