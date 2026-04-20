-- | Configuration types with FromJSON instances.
--
-- All configuration types for the db-sync YAML config file.
-- These are network-agnostic — the same config works for mainnet, preprod, etc.
-- Network-specific details come from the node config (passed via CLI).
--
-- Follows the original project's pattern: optional sections with defaults,
-- and a preset + override system for projections.
module DbSync.Config.Types
  ( -- * Top-level config
    SyncConfig (..)
  , DatabaseConfig (..)
  , SyncSettings (..)
  , SyncMode (..)
  , LedgerConfig (..)
  , MetricsConfig (..)
  , LoggingConfig (..)
  , LogFormat (..)

    -- * Projection config
  , ProjectionConfigs (..)
  , ProjectionConfig (..)
  , UTxOVariant (..)
  , MetadataFormat (..)
  , GovernanceVariant (..)

    -- * Defaults
  , defaultSyncSettings
  , defaultLedgerConfig
  , defaultMetricsConfig
  , defaultLoggingConfig
  , defaultProjectionConfigs

    -- * Node config (extracted)
  , NodeConfig (..)
  , NetworkMagicConfig (..)

    -- * Errors
  , ConfigError (..)
  ) where

import Cardano.Prelude

import Data.Aeson (FromJSON (..), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (typeMismatch)

-- ---------------------------------------------------------------------------
-- * Top-level config
-- ---------------------------------------------------------------------------

-- | Top-level sync configuration, parsed from db-sync.yaml.
data SyncConfig = SyncConfig
  { scDatabase    :: !DatabaseConfig
  , scSync        :: !SyncSettings
  , scLedger      :: !LedgerConfig
  , scProjections :: !ProjectionConfigs
  , scMetrics     :: !MetricsConfig
  , scLogging     :: !LoggingConfig
  }
  deriving stock (Eq, Show)

instance FromJSON SyncConfig where
  parseJSON = Aeson.withObject "SyncConfig" $ \o ->
    SyncConfig
      <$> o .:  "database"
      <*> o .:? "sync"        .!= defaultSyncSettings
      <*> o .:? "ledger"      .!= defaultLedgerConfig
      <*> o .:? "projections" .!= defaultProjectionConfigs
      <*> o .:? "metrics"     .!= defaultMetricsConfig
      <*> o .:? "logging"     .!= defaultLoggingConfig

-- | PostgreSQL connection configuration.
data DatabaseConfig = DatabaseConfig
  { dcHost     :: !Text
  , dcPort     :: !Int
  , dcName     :: !Text
  , dcUser     :: !Text
  , dcPassword :: !Text
  }
  deriving stock (Eq, Show)

instance FromJSON DatabaseConfig where
  parseJSON = Aeson.withObject "DatabaseConfig" $ \o ->
    DatabaseConfig
      <$> o .:  "host"
      <*> o .:? "port"     .!= 5432
      <*> o .:  "name"
      <*> o .:? "user"     .!= "postgres"
      <*> o .:? "password" .!= ""

-- | Sync behaviour settings.
data SyncSettings = SyncSettings
  { ssMode            :: !SyncMode
  , ssCheckpointDir   :: !FilePath
  , ssCopyConnections :: !Int
  }
  deriving stock (Eq, Show)

instance FromJSON SyncSettings where
  parseJSON = Aeson.withObject "SyncSettings" $ \o ->
    SyncSettings
      <$> o .:? "mode"             .!= SyncModeAuto
      <*> o .:? "checkpoint_dir"   .!= "/data/checkpoints"
      <*> o .:? "copy_connections" .!= 12

-- | Default sync settings used when the "sync" section is omitted.
defaultSyncSettings :: SyncSettings
defaultSyncSettings = SyncSettings
  { ssMode            = SyncModeAuto
  , ssCheckpointDir   = "/data/checkpoints"
  , ssCopyConnections = 12
  }

-- | How to determine which phase to start in.
data SyncMode
  = SyncModeAuto    -- ^ Detect based on DB state and immutable tip
  | SyncModeIngest  -- ^ Force IngestChainHistory
  | SyncModeFollow  -- ^ Force FollowingChainTip (assumes DB is populated)
  deriving stock (Eq, Show)

instance FromJSON SyncMode where
  parseJSON = Aeson.withText "SyncMode" $ \t ->
    case t of
      "auto"   -> pure SyncModeAuto
      "ingest" -> pure SyncModeIngest
      "follow" -> pure SyncModeFollow
      _        -> Aeson.typeMismatch "SyncMode (auto|ingest|follow)" (Aeson.String t)

-- | Ledger state settings.
data LedgerConfig = LedgerConfig
  { lcEnabled          :: !Bool
  , lcStateDir         :: !FilePath
  , lcSnapshotInterval :: !Int
  }
  deriving stock (Eq, Show)

instance FromJSON LedgerConfig where
  parseJSON = Aeson.withObject "LedgerConfig" $ \o ->
    LedgerConfig
      <$> o .:? "enabled"           .!= True
      <*> o .:? "state_dir"         .!= "/data/ledger"
      <*> o .:? "snapshot_interval" .!= 10

-- | Default ledger config used when the "ledger" section is omitted.
defaultLedgerConfig :: LedgerConfig
defaultLedgerConfig = LedgerConfig
  { lcEnabled          = True
  , lcStateDir         = "/data/ledger"
  , lcSnapshotInterval = 10
  }

-- | Prometheus metrics settings.
data MetricsConfig = MetricsConfig
  { mcPrometheusPort :: !Int
  }
  deriving stock (Eq, Show)

instance FromJSON MetricsConfig where
  parseJSON = Aeson.withObject "MetricsConfig" $ \o ->
    MetricsConfig
      <$> o .:? "prometheus_port" .!= 8080

-- | Default metrics config.
defaultMetricsConfig :: MetricsConfig
defaultMetricsConfig = MetricsConfig
  { mcPrometheusPort = 8080
  }

-- | Logging settings.
data LoggingConfig = LoggingConfig
  { lgLevel  :: !Text
  , lgFormat :: !LogFormat
  }
  deriving stock (Eq, Show)

instance FromJSON LoggingConfig where
  parseJSON = Aeson.withObject "LoggingConfig" $ \o ->
    LoggingConfig
      <$> o .:? "level"  .!= "info"
      <*> o .:? "format" .!= LogFormatText

-- | Default logging config.
defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig
  { lgLevel  = "info"
  , lgFormat = LogFormatText
  }

-- | Output format for log messages.
data LogFormat
  = LogFormatText
  | LogFormatJson
  deriving stock (Eq, Show)

instance FromJSON LogFormat where
  parseJSON = Aeson.withText "LogFormat" $ \t ->
    case t of
      "text" -> pure LogFormatText
      "json" -> pure LogFormatJson
      _      -> Aeson.typeMismatch "LogFormat (text|json)" (Aeson.String t)

-- ---------------------------------------------------------------------------
-- * Projection config
-- ---------------------------------------------------------------------------

-- | Per-projection configuration.
-- Following the original project's pattern: each field is optional and defaults
-- to the value from 'defaultProjectionConfigs'. Unmentioned projections keep
-- their defaults.
data ProjectionConfigs = ProjectionConfigs
  { pcCore            :: !ProjectionConfig
  , pcUtxo            :: !ProjectionConfig
  , pcMultiAsset      :: !ProjectionConfig
  , pcMetadata        :: !ProjectionConfig
  , pcStakeDelegation :: !ProjectionConfig
  , pcScriptsDatums   :: !ProjectionConfig
  , pcGovernance      :: !ProjectionConfig
  , pcCbor            :: !ProjectionConfig
  , pcEpochBoundary   :: !ProjectionConfig
  , pcCurrentState    :: !ProjectionConfig
  }
  deriving stock (Eq, Show)

instance FromJSON ProjectionConfigs where
  parseJSON = Aeson.withObject "ProjectionConfigs" $ \o ->
    ProjectionConfigs
      <$> o .:? "core"             .!= enabled
      <*> o .:? "utxo"             .!= enabled
      <*> o .:? "multi_asset"      .!= enabled
      <*> o .:? "metadata"         .!= enabled
      <*> o .:? "stake_delegation" .!= enabled
      <*> o .:? "scripts_datums"   .!= enabled
      <*> o .:? "governance"       .!= enabled
      <*> o .:? "cbor"             .!= disabled  -- off by default (large)
      <*> o .:? "epoch_boundary"   .!= enabled
      <*> o .:? "current_state"    .!= disabled  -- off by default (needs ledger)
    where
      enabled  = ProjectionConfig True
      disabled = ProjectionConfig False

-- | Default projection config: standard projections enabled,
-- cbor and current_state disabled.
defaultProjectionConfigs :: ProjectionConfigs
defaultProjectionConfigs = ProjectionConfigs
  { pcCore            = ProjectionConfig True
  , pcUtxo            = ProjectionConfig True
  , pcMultiAsset      = ProjectionConfig True
  , pcMetadata        = ProjectionConfig True
  , pcStakeDelegation = ProjectionConfig True
  , pcScriptsDatums   = ProjectionConfig True
  , pcGovernance      = ProjectionConfig True
  , pcCbor            = ProjectionConfig False
  , pcEpochBoundary   = ProjectionConfig True
  , pcCurrentState    = ProjectionConfig False
  }

-- | Configuration for a single projection.
data ProjectionConfig = ProjectionConfig
  { prEnabled :: !Bool
  }
  deriving stock (Eq, Show)

instance FromJSON ProjectionConfig where
  parseJSON = Aeson.withObject "ProjectionConfig" $ \o ->
    ProjectionConfig
      <$> o .:? "enabled" .!= True

-- | UTxO storage variants.
data UTxOVariant
  = UTxOFull
  | UTxOPruned
  | UTxOConsumed
  | UTxOAddressNormalised
  deriving stock (Eq, Show)

-- | Metadata storage format.
data MetadataFormat
  = MetadataText
  | MetadataJsonb
  | MetadataKeysOnly
  deriving stock (Eq, Show)

-- | Governance projection variants.
data GovernanceVariant
  = GovernanceProposalsOnly
  | GovernanceFull
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Node config (extracted)
-- ---------------------------------------------------------------------------

-- | Whether the network requires magic (testnets) or not (mainnet).
-- Mirrors the original's RequiresNetworkMagic from cardano-crypto.
data NetworkMagicConfig
  = RequiresNoMagic   -- ^ Mainnet (magic = 764824073)
  | RequiresMagic     -- ^ Testnet (magic read from genesis)
  deriving stock (Eq, Show)

instance FromJSON NetworkMagicConfig where
  parseJSON = Aeson.withText "NetworkMagicConfig" $ \t ->
    case t of
      "RequiresNoMagic" -> pure RequiresNoMagic
      "RequiresMagic"   -> pure RequiresMagic
      _                 -> Aeson.typeMismatch
                             "NetworkMagicConfig (RequiresNoMagic|RequiresMagic)"
                             (Aeson.String t)

-- | Relevant fields extracted from the cardano-node config JSON.
-- Follows the same key names as the original node config (ByronGenesisFile, etc.)
-- so we can parse real production configs directly.
-- We ignore the logging/tracing keys — only extract what db-sync needs.
data NodeConfig = NodeConfig
  { ncByronGenesisFile     :: !FilePath
  , ncByronGenesisHash     :: !Text
  , ncShelleyGenesisFile   :: !FilePath
  , ncShelleyGenesisHash   :: !Text
  , ncAlonzoGenesisFile    :: !FilePath
  , ncAlonzoGenesisHash    :: !Text
  , ncConwayGenesisFile    :: !FilePath
  , ncConwayGenesisHash    :: !(Maybe Text)
  , ncRequiresNetworkMagic :: !NetworkMagicConfig
  }
  deriving stock (Eq, Show)

instance FromJSON NodeConfig where
  parseJSON = Aeson.withObject "NodeConfig" $ \o ->
    NodeConfig
      <$> o .:  "ByronGenesisFile"
      <*> o .:  "ByronGenesisHash"
      <*> o .:  "ShelleyGenesisFile"
      <*> o .:  "ShelleyGenesisHash"
      <*> o .:  "AlonzoGenesisFile"
      <*> o .:  "AlonzoGenesisHash"
      <*> o .:  "ConwayGenesisFile"
      <*> o .:? "ConwayGenesisHash"
      <*> o .:  "RequiresNetworkMagic"

-- ---------------------------------------------------------------------------
-- * Errors
-- ---------------------------------------------------------------------------

-- | Configuration parsing and validation errors.
data ConfigError
  = ConfigParseError !Text
  | ConfigMissingField !Text
  | ConfigValidationError !Text
  deriving stock (Eq, Show)

instance Exception ConfigError
