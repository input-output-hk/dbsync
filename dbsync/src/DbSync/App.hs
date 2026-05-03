-- | Application entry point.
--
-- Orchestrates the full db-sync lifecycle: environment setup,
-- startup logging, phase detection, and phase transitions
-- (Ingest -> Preparing -> Following).
module DbSync.App
  ( -- * Environment construction
    buildCoreEnv

    -- * Startup
  , runStartup
  ) where

import Cardano.Prelude

import Control.Tracer (traceWith)

import DbSync.Config.Types
  ( NodeConfig
  , SyncOption (..)
  , SyncOptions (..)
  , SyncConfig (..)
  )
import DbSync.Env (CoreEnv (..))
import DbSync.Metrics (Metrics (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.UTxO (utxoExtractor)
import DbSync.Extractor.Metadata (metadataExtractor)
import DbSync.Extractor.MultiAsset (multiAssetExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.Cbor (cborExtractor)
import DbSync.Extractor.EpochBoundary (epochBoundaryExtractor)
import DbSync.Extractor.EpochSyncStats (epochSyncStatsExtractor)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))
import DbSync.AppM (CoreM)

-- ---------------------------------------------------------------------------
-- * Environment construction
-- ---------------------------------------------------------------------------

-- | Build the shared core environment from parsed configs.
--
-- Constructs the tracer, placeholder metrics, and the list of
-- active extractor definitions based on the config.
buildCoreEnv :: AppTracer -> SyncConfig -> NodeConfig -> IO CoreEnv
buildCoreEnv tracer syncCfg nodeCfg = do
  let extractors = buildExtractors (scOptions syncCfg)
      metrics = placeholderMetrics
  pure CoreEnv
    { ceTracer      = tracer
    , ceMetrics     = metrics
    , ceConfig      = syncCfg
    , ceNodeConfig  = nodeCfg
    , ceExtractors = extractors
    }

-- | Build the list of enabled extractors from config.
--
-- 'coreExtractor' is unconditional and always first — every other
-- extractor's tables reference its block/tx/slot_leader rows via
-- foreign keys, so it has no off switch and no entry in 'SyncOptions'.
-- The remaining extractors are opt-in via @db_options@; stubs stand in
-- for those not yet implemented.
buildExtractors :: SyncOptions -> [ExtractorDef]
buildExtractors pc = coreExtractor : mapMaybe mkProj optionalExtractors
  where
    mkProj :: (Text, SyncOption) -> Maybe ExtractorDef
    mkProj (name, cfg)
      | prEnabled cfg = Just $ resolveExtractor name
      | otherwise     = Nothing

    -- | Resolve a named extractor to its real implementation (if available)
    -- or a stub (if not yet implemented).
    resolveExtractor :: Text -> ExtractorDef
    resolveExtractor "utxo"             = utxoExtractor
    resolveExtractor "metadata"         = metadataExtractor
    resolveExtractor "multi_asset"      = multiAssetExtractor
    resolveExtractor "stake_delegation" = stakeDelegationExtractor
    resolveExtractor "pool"             = poolExtractor
    resolveExtractor "cbor"             = cborExtractor
    resolveExtractor "epoch_sync_stats" = epochSyncStatsExtractor
    resolveExtractor "epoch_boundary"   = epochBoundaryExtractor
    resolveExtractor name               = stubExtractor name

    optionalExtractors :: [(Text, SyncOption)]
    optionalExtractors =
      [ ("utxo",             pcUtxo pc)
      , ("multi_asset",      pcMultiAsset pc)
      , ("metadata",         pcMetadata pc)
      , ("stake_delegation", pcStakeDelegation pc)
      , ("pool",             pcPool pc)
      , ("scripts_datums",   pcScriptsDatums pc)
      , ("governance",       pcGovernance pc)
      , ("cbor",             pcCbor pc)
      , ("epoch_sync_stats", pcEpochSyncStats pc)
      , ("epoch_boundary",   pcEpochBoundary pc)
      , ("current_state",    pcCurrentState pc)
      ]

-- | Placeholder extractor — name only, no real extraction logic yet.
stubExtractor :: Text -> ExtractorDef
stubExtractor name = ExtractorDef
  { pdName         = name
  , pdVersion      = 1
  , pdDependencies = []
  , pdTables       = []
  , pdProcess      = \_ _ _ -> pure ()  -- no-op stub
  }

-- | Placeholder metrics until Prometheus is wired up.
placeholderMetrics :: Metrics
placeholderMetrics = Metrics 0 0 0 0 0 0 0 0 0

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
