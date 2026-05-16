{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Build an 'AppArgs' from a 'MockNode' so tests can call
-- 'DbSync.App.Run.runApp' directly against the same code path
-- production uses.
--
-- 'minimalProfile' points at the @dbsync_test@ database, leaves the
-- ledger off, and enables no optional extractors. Tests that need
-- different settings build their own 'SyncConfig'.
module DbSync.Test.AppHarness
  ( -- * Profile builders
    minimalProfile
  , defaultTestProfile
  , ledgerEnabledTestProfile
  , profileWithOptions
  , allImplementedExtractors

    -- * Profile introspection
  , profileTableNames
  , profileExpectedIndexes

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
import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Indexes (uniqueConstraintIndexName)
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Trace.Backend (mkNullTracer, mkStdErrTracer)
import DbSync.Trace.Types (AppTracer, Severity)
import DbSync.Config.Types
  ( DatabaseConfig (..)
  , LedgerConfig (..)
  , LogFormat (..)
  , LoggingConfig (..)
  , MetricsConfig (..)
  , SyncConfig (..)
  , SyncOption (..)
  , SyncOptions (..)
  , SyncMode (..)
  , SyncSettings (..)
  , defaultLedgerBackend
  , defaultSyncOptions
  )
import DbSync.Test.Database (queryTestDb, testDbName)
import DbSync.Test.Helpers (waitFor)
import DbSync.Test.MockChain (MockChain (..))
import DbSync.Test.MockNode (MockNode (..))

-- ---------------------------------------------------------------------------
-- * Profile builders
-- ---------------------------------------------------------------------------

-- | Profile with no optional extractors. The 'core' extractor is
-- still loaded unconditionally but its Shelley+ slot-leader path
-- writes 'pool_hash' rows — so any chain that crosses into Shelley
-- needs at least 'pool' enabled. Prefer 'defaultTestProfile' unless
-- a test specifically exercises a pre-Shelley-only chain.
minimalProfile :: SyncConfig
minimalProfile = profileWithOptions defaultSyncOptions

-- | The standard test profile: every currently-implemented
-- extractor enabled. Matches what an "everything" production
-- profile would do for the extractors the codebase has landed.
defaultTestProfile :: SyncConfig
defaultTestProfile = profileWithOptions allImplementedExtractors

-- | Same as 'defaultTestProfile' but with the ledger feature on.
-- Tests that exercise the LedgerWorker / snapshot writer / Follow
-- fast-path snapshot loading need ledger enabled.
ledgerEnabledTestProfile :: SyncConfig
ledgerEnabledTestProfile =
  defaultTestProfile
    { scLedger = LedgerConfig
        { lcEnabled = True
        , lcBackend = defaultLedgerBackend
        }
    }

-- | All extractors with a real (non-stub) implementation today —
-- see 'DbSync.App.resolveExtractor'. Skipped: @scripts_datums@,
-- @governance@, @current_state@ (stubs).
allImplementedExtractors :: SyncOptions
allImplementedExtractors = SyncOptions
  { pcUtxo            = SyncOption True
  , pcMultiAsset      = SyncOption True
  , pcMetadata        = SyncOption True
  , pcStakeDelegation = SyncOption True
  , pcPool            = SyncOption True
  , pcScriptsDatums   = SyncOption False
  , pcGovernance      = SyncOption False
  , pcCbor            = SyncOption True
  , pcEpochSyncStats  = SyncOption True
  , pcEpochBoundary   = SyncOption True
  , pcCurrentState    = SyncOption False
  }

-- | Same as 'minimalProfile' but with caller-supplied 'SyncOptions'.
profileWithOptions :: SyncOptions -> SyncConfig
profileWithOptions opts = SyncConfig
  { scDatabase = DatabaseConfig
      { dcHost     = "localhost"
      , dcPort     = 5432
      , dcName     = testDbName
      , dcUser     = ""
      , dcPassword = ""
      }
  , scSync = SyncSettings
      { ssMode            = SyncModeAuto
      , ssCheckpointDir   = "/tmp/dbsync-test-checkpoints"
      , ssCopyConnections = 4
      }
  , scLedger = LedgerConfig
      { lcEnabled = False
      , lcBackend = defaultLedgerBackend
      }
  , scOptions = opts
  , scMetrics = MetricsConfig { mcPrometheusPort = 9999 }
  , scLogging = LoggingConfig
      { lgLevel  = "info"
      , lgFormat = LogFormatText
      }
  }

-- ---------------------------------------------------------------------------
-- * Profile introspection
-- ---------------------------------------------------------------------------

-- | Names of every table the enabled extractors on this profile own.
-- Tests use this to iterate the schema-flip / index / sequence
-- assertions instead of hard-coding a stale list.
--
-- Returns the empty list if the profile is malformed (e.g. an
-- extractor depends on something disabled). The same configuration
-- would refuse to run via 'runApp', so test calls that drove a real
-- sync first don't hit this case.
profileTableNames :: SyncConfig -> [Text]
profileTableNames cfg = case buildExtractors (scOptions cfg) of
  Right exts -> map tdName (concatMap pdTables exts)
  Left _err  -> []

-- | Names of every index that 'PreparingForVolatileTail' should have
-- created by the time it marks @sync_complete = true@.
--
-- Derived from two sources:
--
--   * 'TableDef' metadata on the active extractor tables — primary
--     keys yield @<table>_pkey_idx@; each entry in
--     'tdUniqueConstraints' yields @<table>_unique_N_idx@.
--   * 'preResolveIndexNames' below — the static perf indexes
--     'DbSync.Phase.PreparingForVolatileTail.PreResolveIndexes' builds
--     unconditionally, irrespective of which extractors are on.
--
-- Names are deduplicated since the @tx_unique_1_idx@ entry is built
-- by both paths (pre-resolve emits it with @IF NOT EXISTS@ so the
-- schema-driven pass is a no-op).
profileExpectedIndexes :: SyncConfig -> [Text]
profileExpectedIndexes cfg = case buildExtractors (scOptions cfg) of
  Left _err  -> []
  Right exts ->
    List.nub (schemaIndexes <> preResolveIndexNames)
    where
      schemaIndexes = concatMap tableIndexNames (concatMap pdTables exts)

-- | Index names a single 'TableDef' contributes to the post-Prep
-- schema. Mirrors 'DbSync.Db.Statement.Indexes.tableIndexStatements'
-- without invoking the SQL builder.
tableIndexNames :: TableDef -> [Text]
tableIndexNames td =
  pkIdx <> uniqueIdxs
  where
    pkIdx = case tdPrimaryKey td of
      Nothing -> []
      Just _  -> [tdName td <> "_pkey_idx"]
    uniqueIdxs =
      zipWith
        (\n _ -> uniqueConstraintIndexName td n)
        [1 ..]
        (tdUniqueConstraints td)

-- | Indexes the pre-resolve perf-index pass builds unconditionally.
-- Kept in sync by hand with
-- 'DbSync.Db.Statement.Indexes.preResolveIndexStatements'; if that
-- module grows a new entry, append the matching name here.
preResolveIndexNames :: [Text]
preResolveIndexNames =
  [ "tx_unique_1_idx"
  , "tx_out_tx_id_index_idx"
  , "tx_in_tx_out_idx"
  , "collateral_tx_in_tx_in_id_idx"
  , "collateral_tx_out_tx_id_idx"
  , "tx_in_tx_in_id_idx"
  , "withdrawal_tx_id_idx"
  ]

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
mkAppArgsWithResync resync profile mn ledgerDir mShutdown = AppArgs
  { aaProfile           = profile
  , aaNodeConfig        = mcNodeConfig (mnChain mn)
  , aaGenesisConfig     = mcGenesisConfig (mnChain mn)
  , aaSocketPath        = mnSocketPath mn
  , aaLedgerStateDir    = ledgerDir
  , aaResyncFromGenesis = resync
  , aaShutdownSignal    = mShutdown
  , aaStateQueryVar     = Just (mcStateQueryVar (mnChain mn))
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
-- @verboseTracer Debug@ also enables the Watchdog and per-epoch
-- dedup/RAM diagnostics.
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
        queryTestDb "SELECT sync_complete FROM dbsync_sync_state LIMIT 1"
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
