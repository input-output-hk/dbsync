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
  , ProjectionConfig (..)
  , ProjectionConfigs (..)
  , SyncConfig (..)
  )
import DbSync.Env (CoreEnv (..))
import DbSync.Metrics (Metrics (..))
import DbSync.Projection (ProjectionDef (..))
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..))

-- ---------------------------------------------------------------------------
-- * Environment construction
-- ---------------------------------------------------------------------------

-- | Build the shared core environment from parsed configs.
--
-- Constructs the tracer, placeholder metrics, and the list of
-- active projection definitions based on the config.
buildCoreEnv :: AppTracer -> SyncConfig -> NodeConfig -> IO CoreEnv
buildCoreEnv tracer syncCfg nodeCfg = do
  let projections = buildProjections (scProjections syncCfg)
      metrics = placeholderMetrics
  pure CoreEnv
    { ceTracer      = tracer
    , ceMetrics     = metrics
    , ceConfig      = syncCfg
    , ceNodeConfig  = nodeCfg
    , ceProjections = projections
    }

-- | Build the list of enabled projections from config.
buildProjections :: ProjectionConfigs -> [ProjectionDef]
buildProjections pc = mapMaybe mkProj allProjections
  where
    mkProj :: (Text, ProjectionConfig) -> Maybe ProjectionDef
    mkProj (name, cfg)
      | prEnabled cfg = Just $ stubProjection name
      | otherwise     = Nothing

    allProjections :: [(Text, ProjectionConfig)]
    allProjections =
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

-- | Placeholder projection — name only, no real extraction logic yet.
stubProjection :: Text -> ProjectionDef
stubProjection name = ProjectionDef
  { pdName         = name
  , pdVersion      = 1
  , pdDependencies = []
  , pdTables       = []
  , pdExtract      = \_ st -> (mempty, st)
  }

-- | Placeholder metrics until Prometheus is wired up.
placeholderMetrics :: Metrics
placeholderMetrics = Metrics 0 0 0 0 0 0 0 0 0

-- ---------------------------------------------------------------------------
-- * Startup
-- ---------------------------------------------------------------------------

-- | Log startup information: version, enabled projections, config summary.
--
-- Called once at the very start before phase detection.
runStartup :: CoreEnv -> IO ()
runStartup env = do
  let tracer = ceTracer env
      projNames = map pdName (ceProjections env)
      projCount = length projNames

  traceWith tracer $ LogMsg Info "App" "cardano-db-sync starting" Nothing
  traceWith tracer $ LogMsg Info "App"
    ( "Enabled projections (" <> show projCount <> "): "
      <> showProjectionList projNames
    )
    Nothing

-- | Format a list of projection names for logging.
showProjectionList :: [Text] -> Text
showProjectionList = mconcat . intersperse ", "
