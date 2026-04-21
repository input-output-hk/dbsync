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
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

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
-- Uses the real 'coreExtractor' for "core"; all other extractors
-- are stubs until they are implemented.
buildExtractors :: SyncOptions -> [ExtractorDef]
buildExtractors pc = mapMaybe mkProj allOptions
  where
    mkProj :: (Text, SyncOption) -> Maybe ExtractorDef
    mkProj (name, cfg)
      | prEnabled cfg = Just $ resolveExtractor name
      | otherwise     = Nothing

    -- | Resolve a named extractor to its real implementation (if available)
    -- or a stub (if not yet implemented).
    resolveExtractor :: Text -> ExtractorDef
    resolveExtractor "core" = coreExtractor
    resolveExtractor name   = stubExtractor name

    allOptions :: [(Text, SyncOption)]
    allOptions =
      [ ("core",             pcCore pc)
      , ("utxo",             pcUtxo pc)
      , ("multi_asset",      pcMultiAsset pc)
      , ("metadata",         pcMetadata pc)
      , ("stake_delegation", pcStakeDelegation pc)
      , ("scripts_datums",   pcScriptsDatums pc)
      , ("governance",       pcGovernance pc)
      , ("cbor",             pcCbor pc)
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
  , pdProcess      = \_ _ _ -> pure ()
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
runStartup :: CoreEnv -> IO ()
runStartup env = do
  let tracer = ceTracer env
      projNames = map pdName (ceExtractors env)
      projCount = length projNames

  traceWith tracer $ LogMsg Info "App" "cardano-db-sync starting" Nothing
  traceWith tracer $ LogMsg Info "App"
    ( "Enabled extractors (" <> show projCount <> "): "
      <> showExtractorList projNames
    )
    Nothing

-- | Format a list of extractor names for logging.
showExtractorList :: [Text] -> Text
showExtractorList = mconcat . intersperse ", "
