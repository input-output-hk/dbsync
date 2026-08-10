-- | Boot-time construction: the shared 'CoreEnv', the enabled
-- extractor list, the startup log lines, and the off-chain workers.
-- 'DbSync.App.Run' drives the lifecycle itself.
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
import Data.Time.Clock (UTCTime)
import System.Directory (getModificationTime)
import System.Environment (getExecutablePath)
import qualified Data.Map.Strict as Map
import qualified Hasql.Connection.Settings as HasqlSettings

import DbSync.App.Config.Types
  ( LoggingConfig (..)
  , NodeConfig
  , OptionFlag (..)
  , Extractors (..)
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

-- | Build the shared core environment from the parsed configs.
--
-- The phase holder starts at 'IngestChainHistory'. The orchestrator
-- overwrites it right after the boot decision, so the displayed value
-- is correct from the first subsystem log onwards.
buildCoreEnv :: AppTracer -> SyncConfig -> NodeConfig -> Network -> IO CoreEnv
buildCoreEnv tracer syncCfg nodeCfg network = do
  extractors <- case buildExtractors (scExtractors syncCfg) of
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

-- | Build the enabled extractor list from the config, in declaration
-- order.
--
-- 'coreExtractor' leads the list and is unconditional, because every
-- other extractor's tables reference its block, tx, and slot_leader
-- rows. The @extractors@ block supplies the rest, resolved against
-- 'allKnownExtractors'. A name with no implementation gets a no-op
-- stub, so the schema still records that the operator enabled it.
--
-- Returns 'Either' for call-site symmetry; construction cannot fail.
buildExtractors :: Extractors -> Either Text [ExtractorDef]
buildExtractors pc =
  Right (coreExtractor : mapMaybe mkProj optionalExtractors)
  where
    mkProj :: (Text, Bool) -> Maybe ExtractorDef
    mkProj (name, enabled)
      | enabled   = Just (resolveExtractor name)
      | otherwise = Nothing

    resolveExtractor :: Text -> ExtractorDef
    resolveExtractor name =
      Map.findWithDefault (stubExtractor name) name knownByName

    knownByName :: Map.Map Text ExtractorDef
    knownByName = Map.fromList [(pdName e, e) | e <- allKnownExtractors]

    -- (extractor name, enabled?). 'utxo' reads the structured
    -- 'UtxoOption'; the rest read the flat 'OptionFlag'.
    optionalExtractors :: [(Text, Bool)]
    optionalExtractors =
      [ ("utxo",                    uoEnabled (exUtxo pc))
      , ("multi_asset",             prEnabled (exMultiAsset pc))
      , ("metadata",                prEnabled (exMetadata pc))
      , ("stake_delegation",        prEnabled (exStakeDelegation pc))
      , ("stake_delegation_ledger", prEnabled (exStakeDelegationLedger pc))
      , ("pool",                    prEnabled (exPool pc))
      , ("scripts_datums",          prEnabled (exScriptsDatums pc))
      , ("governance",              prEnabled (exGovernance pc))
      , ("cbor",                    prEnabled (exCbor pc))
      , ("epoch_sync_stats",        prEnabled (exEpochSyncStats pc))
      , ("epoch_boundary",          prEnabled (exEpochBoundary pc))
      , ("pool_stats",              prEnabled (exPoolStats pc))
      , ("epoch",                   prEnabled (exEpoch pc))
      , ("current_state",           prEnabled (exCurrentState pc))
      , ("off_chain_pools",         prEnabled (exOffChainPools pc))
      , ("off_chain_votes",         prEnabled (exOffChainVotes pc))
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

-- | Cardano protocol security parameter @k@. Mainnet and every public
-- testnet have used 2160 since Shelley. It is a protocol parameter,
-- so a change needs a hard fork.
cardanoSecurityParam :: Word64
cardanoSecurityParam = 2160

-- ---------------------------------------------------------------------------
-- * Startup
-- ---------------------------------------------------------------------------

-- | Log the binary identity and the enabled extractors. Runs once, at
-- the very start.
runStartup :: CoreM ()
runStartup = do
  tracer     <- asks ceTracer
  extractors <- asks ceExtractors
  let projNames = map pdName extractors
      projCount = length projNames

  -- Identify the running binary by its link mtime, read at runtime so
  -- it cannot disagree with the file that is executing. Any saved log
  -- then names the build that produced it.
  binaryLine <- liftIO $ do
    exePath <- getExecutablePath
    eTime   <- try (getModificationTime exePath)
                 :: IO (Either IOException UTCTime)
    pure $ case eTime of
      Right t -> "binary " <> toS exePath <> " (linked " <> show t <> ")"
      Left _  -> "binary " <> toS exePath

  liftIO $ traceWith tracer $ LogMsg Info "App" "cardano-db-sync starting"
  liftIO $ traceWith tracer $ LogMsg Info "App" binaryLine
  liftIO $ traceWith tracer $ LogMsg Info "App"
    ( "Enabled extractors (" <> show projCount <> "): "
      <> showExtractorList projNames
    )

showExtractorList :: [Text] -> Text
showExtractorList = mconcat . intersperse ", "

-- ---------------------------------------------------------------------------
-- * Worker setup
-- ---------------------------------------------------------------------------

-- | Spawn the off-chain pool worker only when @off_chain_pools@ is
-- enabled. The worker owns its 'Http.Manager', which isolates its HTTP
-- connection pool from the rest of the process.
setupOffChainPoolWorker
  :: AppTracer
  -> HasqlSettings.Settings
  -> Extractors
  -> IO (Maybe OffChainPoolWorker)
setupOffChainPoolWorker tracer hasqlSettings opts
  | prEnabled (exOffChainPools opts) = do
      manager <- newRestrictedManager
      Just <$>
        mkOffChainPoolWorker
          tracer
          hasqlSettings
          defaultOffChainPoolConfig
          (httpPoolFetcher manager)
  | otherwise = pure Nothing

-- | Spawn the off-chain vote worker only when @off_chain_votes@ is
-- enabled.
setupOffChainVoteWorker
  :: AppTracer
  -> HasqlSettings.Settings
  -> Extractors
  -> IO (Maybe OffChainVoteWorker)
setupOffChainVoteWorker tracer hasqlSettings opts
  | prEnabled (exOffChainVotes opts) = do
      manager <- newRestrictedManager
      let cfg = defaultOffChainVoteConfig
      Just <$>
        mkOffChainVoteWorker
          tracer
          hasqlSettings
          cfg
          (httpVoteFetcher manager (ovcIpfsGateways cfg))
  | otherwise = pure Nothing
