-- | Application entry point.
--
-- Orchestrates the full db-sync lifecycle: environment setup,
-- startup logging, phase detection, and phase transitions
-- (Ingest -> Preparing -> Following).
module DbSync.App.Setup
  ( -- * Environment construction
    buildCoreEnv

    -- * Extractor list construction (exported for testing)
  , buildExtractors

    -- * Constants
  , cardanoSecurityParam

    -- * Startup
  , runStartup

    -- * Worker setup
  , setupOffChainPoolWorker
  , setupOffChainVoteWorker
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network)
import Control.Tracer (traceWith)
import qualified Data.Map.Strict as Map
import qualified Hasql.Connection.Settings as HasqlSettings

import DbSync.App.Config.Types
  ( LoggingConfig (..)
  , NodeConfig
  , OptionFlag (..)
  , DbSyncOptions (..)
  , SyncConfig (..)
  , UtxoOption (..)
  )
import DbSync.Phase.Type (SyncPhase (..))
import DbSync.App.Env (CoreEnv (..))
import DbSync.Error (throwInternal)
import DbSync.Metrics (Metrics (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Phase.Current (newCurrentPhase)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.Registry (allKnownExtractors)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..), severityFromText)
import DbSync.AppM (CoreM)
import DbSync.Worker.OffChain.Http (newRestrictedManager)
import DbSync.Worker.OffChain.Pool
  ( OffChainPoolWorker
  , defaultOffChainPoolConfig
  , httpPoolFetcher
  , mkOffChainPoolWorker
  )
import DbSync.Worker.OffChain.Vote
  ( OffChainVoteConfig (..)
  , OffChainVoteWorker
  , defaultOffChainVoteConfig
  , httpVoteFetcher
  , mkOffChainVoteWorker
  )

-- ---------------------------------------------------------------------------
-- * Environment construction
-- ---------------------------------------------------------------------------

-- | Build the shared core environment from parsed configs.
--
-- The phase holder is seeded with 'IngestChainHistory'; the
-- orchestrator in 'DbSync.App.Run' overwrites it immediately after
-- the boot decision so the displayed value is correct from the
-- first subsystem log onwards.
buildCoreEnv :: AppTracer -> SyncConfig -> NodeConfig -> Network -> IO CoreEnv
buildCoreEnv tracer syncCfg nodeCfg network = do
  extractors <- case buildExtractors (scOptions syncCfg) of
    Left err  -> throwInternal err
    Right xs  -> pure xs
  curPhase <- newCurrentPhase IngestChainHistory
  pure CoreEnv
    { ceTracer        = tracer
    , ceMinSeverity   = severityFromText (lgLevel (scLogging syncCfg))
    , ceMetrics       = placeholderMetrics
    , ceConfig        = syncCfg
    , ceNodeConfig    = nodeCfg
    , ceExtractors    = extractors
    , ceNetwork       = network
    , ceCurrentPhase  = curPhase
    , ceSecurityParam = cardanoSecurityParam
    }

-- | Build the list of enabled extractors from config, in declaration
-- order.
--
-- 'coreExtractor' is unconditional — every other extractor's tables
-- reference its block / tx / slot_leader rows — and so leads the list.
-- Optional extractors come from @db_options@ and are resolved against
-- 'allKnownExtractors'; a name with no implementation yet (e.g.
-- @current_state@) gets a no-op stub so its enablement is still
-- recorded and the schema reflects it once the work lands.
--
-- Returns 'Either' for call-site symmetry; construction no longer fails.
buildExtractors :: DbSyncOptions -> Either Text [ExtractorDef]
buildExtractors pc =
  Right (coreExtractor : mapMaybe mkProj optionalExtractors)
  where
    mkProj :: (Text, Bool) -> Maybe ExtractorDef
    mkProj (name, enabled)
      | enabled   = Just (resolveExtractor name)
      | otherwise = Nothing

    -- | Resolve a named extractor to its real implementation, or a stub
    -- if it isn't implemented yet.
    resolveExtractor :: Text -> ExtractorDef
    resolveExtractor name =
      Map.findWithDefault (stubExtractor name) name knownByName

    knownByName :: Map.Map Text ExtractorDef
    knownByName = Map.fromList [(pdName e, e) | e <- allKnownExtractors]

    -- | (extractor name, enabled?). 'utxo' reads from the structured
    -- 'UtxoOption'; the rest read the flat 'OptionFlag' bool.
    optionalExtractors :: [(Text, Bool)]
    optionalExtractors =
      [ ("utxo",                    uoEnabled (pcUtxo pc))
      , ("multi_asset",             prEnabled (pcMultiAsset pc))
      , ("metadata",                prEnabled (pcMetadata pc))
      , ("stake_delegation",        prEnabled (pcStakeDelegation pc))
      , ("stake_delegation_ledger", prEnabled (pcStakeDelegationLedger pc))
      , ("pool",                    prEnabled (pcPool pc))
      , ("scripts_datums",          prEnabled (pcScriptsDatums pc))
      , ("governance",              prEnabled (pcGovernance pc))
      , ("cbor",                    prEnabled (pcCbor pc))
      , ("epoch_sync_stats",        prEnabled (pcEpochSyncStats pc))
      , ("epoch_boundary",          prEnabled (pcEpochBoundary pc))
      , ("pool_stats",              prEnabled (pcPoolStats pc))
      , ("epoch",                   prEnabled (pcEpoch pc))
      , ("current_state",           prEnabled (pcCurrentState pc))
      , ("off_chain_pools",         prEnabled (pcOffChainPools pc))
      , ("off_chain_votes",         prEnabled (pcOffChainVotes pc))
      ]

-- | Placeholder extractor — name only, no real extraction logic yet.
stubExtractor :: Text -> ExtractorDef
stubExtractor name = ExtractorDef
  { pdName    = name
  , pdTables  = []
  , pdProcess = \_ -> pure ()  -- no-op stub
  }

-- | Placeholder metrics until Prometheus is wired up.
placeholderMetrics :: Metrics
placeholderMetrics = Metrics 0 0 0 0 0 0 0 0 0

-- | Cardano protocol security parameter @k@. Mainnet and every
-- public testnet have used 2160 since Shelley; the value is part of
-- the protocol parameters and a change would be a hard fork.
cardanoSecurityParam :: Word64
cardanoSecurityParam = 2160

-- ---------------------------------------------------------------------------
-- * Startup
-- ---------------------------------------------------------------------------

-- | Log startup information: version, enabled extractors, config summary.
--
-- Called once at the very start before phase detection.
runStartup :: CoreM ()
runStartup = do
  tracer     <- asks ceTracer
  extractors <- asks ceExtractors
  let projNames = map pdName extractors
      projCount = length projNames

  liftIO $ traceWith tracer $ LogMsg Info "App" "cardano-db-sync starting" Nothing
  liftIO $ traceWith tracer $ LogMsg Info "App"
    ( "Enabled extractors (" <> show projCount <> "): "
      <> showExtractorList projNames
    )
    Nothing

-- | Format a list of extractor names for logging.
showExtractorList :: [Text] -> Text
showExtractorList = mconcat . intersperse ", "

-- ---------------------------------------------------------------------------
-- * Worker setup
-- ---------------------------------------------------------------------------

-- | Spawn the off-chain pool worker iff @off_chain_pools@ is enabled.
-- Each worker owns its own 'Http.Manager' so the per-worker HTTP
-- connection pool is isolated from the rest of the process.
setupOffChainPoolWorker
  :: AppTracer
  -> HasqlSettings.Settings
  -> DbSyncOptions
  -> IO (Maybe OffChainPoolWorker)
setupOffChainPoolWorker tracer hasqlSettings opts
  | prEnabled (pcOffChainPools opts) = do
      manager <- newRestrictedManager
      Just <$>
        mkOffChainPoolWorker
          tracer
          hasqlSettings
          defaultOffChainPoolConfig
          (httpPoolFetcher manager)
  | otherwise = pure Nothing

-- | Spawn the off-chain vote worker iff @off_chain_votes@ is enabled.
setupOffChainVoteWorker
  :: AppTracer
  -> HasqlSettings.Settings
  -> DbSyncOptions
  -> IO (Maybe OffChainVoteWorker)
setupOffChainVoteWorker tracer hasqlSettings opts
  | prEnabled (pcOffChainVotes opts) = do
      manager <- newRestrictedManager
      let cfg = defaultOffChainVoteConfig
      Just <$>
        mkOffChainVoteWorker
          tracer
          hasqlSettings
          cfg
          (httpVoteFetcher manager (ovcIpfsGateways cfg))
  | otherwise = pure Nothing
