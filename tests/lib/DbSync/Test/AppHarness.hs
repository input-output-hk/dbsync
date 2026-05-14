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
  , profileWithOptions
  , allImplementedExtractors

    -- * AppArgs builders
  , mkAppArgsFromMockNode
  , withTempDir

    -- * Tracer selection
  , quietTracer
  , verboseTracer

    -- * Sync-state polling
  , waitForSyncComplete
  , waitFor
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..), diffUTCTime, getCurrentTime)
import Data.Time.Calendar (fromGregorian)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)

import DbSync.App.Args (AppArgs (..))
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
-- * AppArgs builders
-- ---------------------------------------------------------------------------

-- | Assemble 'AppArgs' pointing at the supplied mock node.
-- | Build 'AppArgs' pointing at the mock node. Reuses the
-- 'MockChain'\'s pre-seeded state-query handle so 'parseBlock' can
-- compute slot details without waiting on the mock server's
-- stubbed LocalStateQuery responder (which never replies).
mkAppArgsFromMockNode
  :: SyncConfig
  -> MockNode
  -> FilePath            -- ^ scratch dir for ledger state (ignored when ledger off)
  -> Maybe (IO ())       -- ^ optional shutdown signal
  -> AppArgs
mkAppArgsFromMockNode profile mn ledgerDir mShutdown = AppArgs
  { aaProfile           = profile
  , aaNodeConfig        = mcNodeConfig (mnChain mn)
  , aaGenesisConfig     = mcGenesisConfig (mnChain mn)
  , aaSocketPath        = mnSocketPath mn
  , aaLedgerStateDir    = ledgerDir
  , aaResyncFromGenesis = True
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

-- | Poll @predicate@ every 200ms up to @timeoutSecs@. Panics if it
-- never returns 'True'.
waitFor :: Text -> IO Bool -> Int -> IO ()
waitFor what predicate timeoutSecs = do
  start <- getCurrentTime
  go start
  where
    go startedAt = do
      ok <- predicate
      if ok
        then pure ()
        else do
          now <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime now startedAt) :: Double
          if elapsed >= fromIntegral timeoutSecs
            then panic $ "waitFor " <> what <> ": timed out after "
                            <> T.pack (show timeoutSecs) <> "s"
            else do
              threadDelay 200_000
              go startedAt
