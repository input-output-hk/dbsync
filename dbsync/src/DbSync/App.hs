-- | Application entry point.
--
-- Orchestrates the full db-sync lifecycle: environment setup,
-- startup logging, phase detection, and phase transitions
-- (Ingest -> Preparing -> Following).
module DbSync.App
  ( -- * Environment construction
    buildCoreEnv

    -- * Extractor list construction (exported for testing)
  , buildExtractors
  , validateExtractorDeps
  , topoSortExtractors

    -- * Startup
  , runStartup
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network)
import Control.Tracer (traceWith)
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import DbSync.Config.Types
  ( LoggingConfig (..)
  , NodeConfig
  , SyncOption (..)
  , SyncOptions (..)
  , SyncConfig (..)
  )
import DbSync.Db.Phase (SyncPhase (..))
import DbSync.Env (CoreEnv (..))
import DbSync.Error (throwInternal)
import DbSync.Metrics (Metrics (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Phase.Current (newCurrentPhase, readCurrentPhase)
import DbSync.Trace.Backend (withPhaseFilter)
import DbSync.Extractor.Core (coreExtractor)
import DbSync.Extractor.UTxO (utxoExtractor)
import DbSync.Extractor.Metadata (metadataExtractor)
import DbSync.Extractor.MultiAsset (multiAssetExtractor)
import DbSync.Extractor.StakeDelegation (stakeDelegationExtractor)
import DbSync.Extractor.Pool (poolExtractor)
import DbSync.Extractor.Cbor (cborExtractor)
import DbSync.Extractor.EpochBoundary (epochBoundaryExtractor)
import DbSync.Extractor.EpochSyncStats (epochSyncStatsExtractor)
import DbSync.Trace.Types (AppTracer, LogMsg (..), Severity (..), severityFromText)
import DbSync.AppM (CoreM)

-- ---------------------------------------------------------------------------
-- * Environment construction
-- ---------------------------------------------------------------------------

-- | Build the shared core environment from parsed configs.
--
-- The phase holder is seeded with 'IngestChainHistory'; the
-- orchestrator in 'DbSync.App.Run' overwrites it immediately after
-- the boot decision so the displayed value is correct from the
-- first subsystem log onwards.
--
-- Throws via 'throwInternal' if 'buildExtractors' rejects the profile.
buildCoreEnv :: AppTracer -> SyncConfig -> NodeConfig -> Network -> IO CoreEnv
buildCoreEnv tracer syncCfg nodeCfg network = do
  extractors <- case buildExtractors (scOptions syncCfg) of
    Left err  -> throwInternal err
    Right xs  -> pure xs
  curPhase <- newCurrentPhase IngestChainHistory
  let phaseAwareTracer = withPhaseFilter (readCurrentPhase curPhase) tracer
  pure CoreEnv
    { ceTracer       = phaseAwareTracer
    , ceMinSeverity  = severityFromText (lgLevel (scLogging syncCfg))
    , ceMetrics      = placeholderMetrics
    , ceConfig       = syncCfg
    , ceNodeConfig   = nodeCfg
    , ceExtractors   = extractors
    , ceNetwork      = network
    , ceCurrentPhase = curPhase
    }

-- | Build the list of enabled extractors from config, validate their
-- dependencies, and return them in dependency-respecting order.
--
-- 'coreExtractor' is unconditional — every other extractor's tables
-- reference its block / tx / slot_leader rows. Optional extractors
-- come from @db_options@; unknown names get a no-op stub so the
-- schema is still created when work has not landed yet.
buildExtractors :: SyncOptions -> Either Text [ExtractorDef]
buildExtractors pc = do
  let raw = coreExtractor : mapMaybe mkProj optionalExtractors
  validateExtractorDeps raw
  topoSortExtractors raw
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

-- ---------------------------------------------------------------------------
-- * Dependency validation + topological sort
-- ---------------------------------------------------------------------------

-- | Check every declared dependency resolves to an enabled extractor
-- of sufficient version. Stops at the first failure — later errors
-- are usually a consequence of the first.
validateExtractorDeps :: [ExtractorDef] -> Either Text ()
validateExtractorDeps exts = traverse_ checkOne exts
  where
    nameMap :: Map.Map Text ExtractorDef
    nameMap = Map.fromList [(pdName e, e) | e <- exts]

    checkOne :: ExtractorDef -> Either Text ()
    checkOne e = traverse_ (checkDep e) (pdDependencies e)

    checkDep :: ExtractorDef -> (Text, Int) -> Either Text ()
    checkDep e (depName, minVer) =
      case Map.lookup depName nameMap of
        Nothing ->
          Left $
            "Extractor '" <> pdName e <> "' is enabled but its dependency '"
              <> depName <> "' is not enabled.\n"
              <> "Add  \"" <> depName <> "\": true  to the db_options section "
              <> "of your dbsync-profile.json."
        Just dep
          | pdVersion dep < minVer ->
              Left $
                "Extractor '" <> pdName e <> "' requires dependency '"
                  <> depName <> "' version >= " <> show minVer
                  <> ", but the enabled '" <> depName
                  <> "' is only version " <> show (pdVersion dep) <> "."
          | otherwise -> Right ()

-- | Topologically sort extractors so producers come before consumers.
-- Cycles are detected via strongly-connected components and reported
-- as errors. Assumes 'validateExtractorDeps' has already passed.
topoSortExtractors :: [ExtractorDef] -> Either Text [ExtractorDef]
topoSortExtractors exts =
  case cycles of
    (c:_) ->
      Left $
        "Cyclic extractor dependencies detected: "
          <> Text.intercalate " -> " (map nameOfVertex c)
          <> ". Remove a dependency edge or split the affected extractors."
    [] ->
      Right $ map extractorOfVertex (Graph.topSort graph)
  where
    -- For each extractor, the names of OTHER extractors that depend on
    -- it. We need the "who depends on me" direction (not "who I depend
    -- on") because 'Graph.topSort' returns vertices in edge-tail-first
    -- order, and we want dependencies before consumers.
    consumersOf :: Map.Map Text [Text]
    consumersOf =
      Map.fromListWith (++)
        [ (depName, [pdName e])
        | e <- exts
        , (depName, _) <- pdDependencies e
        ]

    edges :: [(ExtractorDef, Text, [Text])]
    edges =
      [ (e, pdName e, Map.findWithDefault [] (pdName e) consumersOf)
      | e <- exts
      ]

    graph        :: Graph.Graph
    vertexToNode :: Graph.Vertex -> (ExtractorDef, Text, [Text])
    (graph, vertexToNode, _) = Graph.graphFromEdges edges

    extractorOfVertex :: Graph.Vertex -> ExtractorDef
    extractorOfVertex v = case vertexToNode v of (e, _, _) -> e

    nameOfVertex :: Graph.Vertex -> Text
    nameOfVertex v = case vertexToNode v of (_, n, _) -> n

    -- An SCC with more than one vertex contains a cycle; singletons are
    -- the normal case.
    cycles :: [[Graph.Vertex]]
    cycles =
      [ flat
      | t <- Graph.scc graph
      , let flat = Tree.flatten t
      , length flat > 1
      ]

-- | Placeholder extractor — name only, no real extraction logic yet.
stubExtractor :: Text -> ExtractorDef
stubExtractor name = ExtractorDef
  { pdName         = name
  , pdVersion      = 1
  , pdDependencies = []
  , pdTables       = []
  , pdProcess      = \_ -> pure ()  -- no-op stub
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
