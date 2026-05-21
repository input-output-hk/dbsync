-- | Configuration types with FromJSON instances.
--
-- All configuration types for the db-sync profile JSON file.
-- These are network-agnostic — the same config works for mainnet, preprod, etc.
-- Network-specific details come from the node config (passed via CLI).
-- Operational paths (sockets, ledger state dir) live on the CLI rather
-- than in the profile so the profile can travel across environments.
--
-- The @db_options@ block is opt-in: every extractor defaults to
-- disabled and must be enabled explicitly. The @core@ extractor is
-- the sole exception — it is unconditional and not represented in
-- 'SyncOptions' at all.
module DbSync.Config.Types
  ( -- * Top-level config
    SyncConfig (..)
  , DatabaseConfig (..)
  , SyncSettings (..)
  , SyncMode (..)
  , LedgerConfig (..)
  , LedgerBackend (..)
  , MetricsConfig (..)
  , LoggingConfig (..)
  , LogFormat (..)

    -- * Sync options
  , SyncOptions (..)
  , SyncOption (..)
  , UtxoOption (..)
  , UtxoStrategy (..)
  , MetadataFormat (..)
  , GovernanceVariant (..)

    -- * Defaults
  , defaultSyncSettings
  , defaultLedgerConfig
  , defaultLedgerBackend
  , defaultSnapshotNearTipEpoch
  , defaultMetricsConfig
  , defaultLoggingConfig
  , defaultSyncOptions
  , defaultUtxoOption

    -- * DB-sync node config (from db-sync-config.json)
  , DbSyncNodeConfig (..)

    -- * Node config (from config.json)
  , NodeConfig (..)
  , NetworkMagicConfig (..)

    -- * Errors
  , ConfigError (..)
  ) where

import Cardano.Prelude

import Data.Aeson (FromJSON (..), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseFail, typeMismatch)

-- ---------------------------------------------------------------------------
-- * Top-level config
-- ---------------------------------------------------------------------------

-- | Top-level sync configuration, parsed from the profile JSON file.
data SyncConfig = SyncConfig
  { scDatabase :: !DatabaseConfig
  , scSync     :: !SyncSettings
  , scLedger   :: !LedgerConfig
  , scOptions  :: !SyncOptions
  , scMetrics  :: !MetricsConfig
  , scLogging  :: !LoggingConfig
  }
  deriving stock (Eq, Show)

instance FromJSON SyncConfig where
  parseJSON = Aeson.withObject "SyncConfig" $ \o ->
    SyncConfig
      <$> o .:  "database"
      <*> o .:? "sync"           .!= defaultSyncSettings
      <*> o .:? "ledger"         .!= defaultLedgerConfig
      <*> o .:? "db_options"     .!= defaultSyncOptions
      <*> o .:? "metrics"        .!= defaultMetricsConfig
      <*> o .:? "logging"        .!= defaultLoggingConfig

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
  , ssLoaderConnections :: !Int
  }
  deriving stock (Eq, Show)

instance FromJSON SyncSettings where
  parseJSON = Aeson.withObject "SyncSettings" $ \o ->
    SyncSettings
      <$> o .:? "mode"             .!= SyncModeAuto
      <*> o .:? "checkpoint_dir"   .!= "/data/checkpoints"
      <*> o .:? "loader_connections" .!= 12

-- | Default sync settings used when the "sync" section is omitted.
defaultSyncSettings :: SyncSettings
defaultSyncSettings = SyncSettings
  { ssMode            = SyncModeAuto
  , ssCheckpointDir   = "/data/checkpoints"
  , ssLoaderConnections = 12
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

-- | Ledger state settings. Opt-in: @enabled@ defaults to 'False'.
--
-- The runtime ledger-state path comes from the @--ledger-state-dir@
-- CLI flag (operational paths live on the CLI; profile is per-DB
-- shape config and travels across environments).
data LedgerConfig = LedgerConfig
  { lcEnabled              :: !Bool
  , lcBackend              :: !LedgerBackend
  , lcSnapshotNearTipEpoch :: !Word64
    -- ^ Past this epoch number, the ledger writes a snapshot at
    -- every epoch boundary regardless of the in-RAM cadence rules.
    -- Production default is @580@; tests lower it so snapshots fire
    -- on the short fixture chains. Below this threshold the cadence
    -- is /every 10 epochs/ in Ingest and /every epoch when near
    -- tip/ in Follow.
  }
  deriving stock (Eq, Show)

instance FromJSON LedgerConfig where
  parseJSON = Aeson.withObject "LedgerConfig" $ \o ->
    LedgerConfig
      <$> o .:? "enabled" .!= False
      <*> o .:? "backend" .!= defaultLedgerBackend
      <*> o .:? "snapshot_near_tip_epoch" .!= defaultSnapshotNearTipEpoch

-- | Default ledger config used when the @"ledger"@ section is omitted.
defaultLedgerConfig :: LedgerConfig
defaultLedgerConfig = LedgerConfig
  { lcEnabled              = False
  , lcBackend              = defaultLedgerBackend
  , lcSnapshotNearTipEpoch = defaultSnapshotNearTipEpoch
  }

-- | Production default for 'lcSnapshotNearTipEpoch'. Matches the
-- upstream cardano-db-sync heuristic: past epoch 580 the chain is
-- "modern" enough that a per-epoch snapshot is cheap and useful.
defaultSnapshotNearTipEpoch :: Word64
defaultSnapshotNearTipEpoch = 580

-- | Which backend stores the ledger-state UTxO tables.
--
-- Only the on-disk LSM backend is supported: RAM targets rely on the
-- UTxO living on disk, and an in-memory backend would roughly double
-- the testing matrix for no operational gain. The 'FromJSON' instance
-- accepts only @\"lsm\"@ and returns a clear error for the historical
-- @\"inmemory\"@ value.
--
-- The optional 'FilePath' override is not wired through yet;
-- 'Nothing' means \"use the directory passed to 'mkHasLedgerEnv'\"
-- (which is derived from the @--ledger-state-dir@ CLI flag).
data LedgerBackend
  = LedgerBackendLSM !(Maybe FilePath)
  deriving stock (Eq, Show)

-- | Default ledger backend — LSM with no path override.
defaultLedgerBackend :: LedgerBackend
defaultLedgerBackend = LedgerBackendLSM Nothing

instance FromJSON LedgerBackend where
  parseJSON = Aeson.withText "LedgerBackend" $ \case
    "lsm" -> pure (LedgerBackendLSM Nothing)
    "inmemory" ->
      Aeson.parseFail
        "ledger.backend: \"inmemory\" is not supported. Use \"lsm\" — the \
        \in-memory backend would roughly double RAM usage and the testing \
        \matrix for no operational gain."
    other ->
      Aeson.parseFail $
        "unexpected ledger.backend: " <> show other <> ". Expected \"lsm\"."

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
-- * Sync options
-- ---------------------------------------------------------------------------

-- | Per-option configuration.
--
-- Opt-in: every option defaults to disabled. Omit a key to disable;
-- set @"key": true@ to enable. The @core@ extractor is unconditional
-- (every other extractor's tables reference its block / tx /
-- slot_leader rows via FK) and is added by @buildExtractors@.
--
-- 'pcUtxo' is structured because the UTxO extractor has multiple
-- knobs that route different Prep paths; the rest are flat bools.
data SyncOptions = SyncOptions
  { pcUtxo            :: !UtxoOption
  , pcMultiAsset      :: !SyncOption
  , pcMetadata        :: !SyncOption
  , pcStakeDelegation :: !SyncOption
  , pcPool            :: !SyncOption
  , pcScriptsDatums   :: !SyncOption
  , pcGovernance      :: !SyncOption
  , pcCbor            :: !SyncOption
  , pcEpochSyncStats  :: !SyncOption
  , pcEpochBoundary   :: !SyncOption
  , pcCurrentState    :: !SyncOption
  }
  deriving stock (Eq, Show)

instance FromJSON SyncOptions where
  parseJSON = Aeson.withObject "SyncOptions" $ \o ->
    SyncOptions
      <$> o .:? "utxo"             .!= defaultUtxoOption
      <*> o .:? "multi_asset"      .!= disabled
      <*> o .:? "metadata"         .!= disabled
      <*> o .:? "stake_delegation" .!= disabled
      <*> o .:? "pool"             .!= disabled
      <*> o .:? "scripts_datums"   .!= disabled
      <*> o .:? "governance"       .!= disabled
      <*> o .:? "cbor"             .!= disabled
      <*> o .:? "epoch_sync_stats" .!= disabled
      <*> o .:? "epoch_boundary"   .!= disabled
      <*> o .:? "current_state"    .!= disabled
    where
      disabled = SyncOption False

-- | Default option config used when the @"db_options"@ section is
-- omitted: every optional extractor off. The unconditional @core@
-- extractor is added by @buildExtractors@ and is not represented here.
defaultSyncOptions :: SyncOptions
defaultSyncOptions = SyncOptions
  { pcUtxo            = defaultUtxoOption
  , pcMultiAsset      = SyncOption False
  , pcMetadata        = SyncOption False
  , pcStakeDelegation = SyncOption False
  , pcPool            = SyncOption False
  , pcScriptsDatums   = SyncOption False
  , pcGovernance      = SyncOption False
  , pcCbor            = SyncOption False
  , pcEpochSyncStats  = SyncOption False
  , pcEpochBoundary   = SyncOption False
  , pcCurrentState    = SyncOption False
  }

-- | Configuration for a single option.
--
-- Today this just wraps a 'Bool'; the wrapper is intentional so that
-- options needing variants (e.g. multi-asset policy allowlists,
-- metadata key filters, governance subsets) can grow without
-- breaking the @SyncOptions@ record.
data SyncOption = SyncOption
  { prEnabled :: !Bool
  }
  deriving stock (Eq, Show)

-- | Parse a sync option from a plain JSON boolean (e.g. @"multi_asset": true@).
instance FromJSON SyncOption where
  parseJSON = Aeson.withBool "SyncOption" (pure . SyncOption)

-- ---------------------------------------------------------------------------
-- * UTxO extractor option
-- ---------------------------------------------------------------------------

-- | UTxO extractor configuration.
--
-- * 'uoEnabled' — whether the extractor runs; @false@ leaves
--   @tx_in@, @tx_out@, and @ma_tx_out@ empty.
-- * 'uoConsumedByTxId' — whether @tx_out.consumed_by_tx_id@ is
--   populated. The per-epoch 'ConsumedByWorker' covers most rows
--   during Ingest; a Prep residual UPDATE catches cache-misses.
-- * 'uoTxIn' — whether @tx_in@ rows are written.
-- * 'uoStrategy' — see 'UtxoStrategy'.
data UtxoOption = UtxoOption
  { uoEnabled        :: !Bool
  , uoConsumedByTxId :: !Bool
  , uoTxIn           :: !Bool
  , uoStrategy       :: !UtxoStrategy
  }
  deriving stock (Eq, Show)

-- | What @tx_out@ contains and how it gets filled.
data UtxoStrategy
  = -- | Every output ever; per-tx COPY during Ingest.
    StrategyArchive
  | -- | Live UTxO only; DELETE consumed rows at Prep.
    StrategyPrune
  | -- | Live UTxO only; skip @tx_out@ writes during Ingest,
    -- bulk-load from the ledger state at Prep.
    StrategyFromLedger
  deriving stock (Eq, Show)

-- | Extractor off; back-pointer on; tx_in populated; archive coverage.
defaultUtxoOption :: UtxoOption
defaultUtxoOption = UtxoOption
  { uoEnabled        = False
  , uoConsumedByTxId = True
  , uoTxIn           = True
  , uoStrategy       = StrategyArchive
  }

instance FromJSON UtxoOption where
  parseJSON = Aeson.withObject "UtxoOption" $ \o -> do
    enabled        <- o .:? "enabled"           .!= uoEnabled defaultUtxoOption
    consumedByTxId <- o .:? "consumed_by_tx_id" .!= uoConsumedByTxId defaultUtxoOption
    txIn           <- o .:? "tx_in"             .!= uoTxIn defaultUtxoOption
    strategy       <- o .:? "strategy"          .!= uoStrategy defaultUtxoOption
    unless txIn $
      Aeson.parseFail
        "utxo.tx_in: false is not yet implemented. The deposit backfill \
        \joins through tx_in.tx_out_id; an alternate backfill via \
        \tx_out.consumed_by_tx_id is planned but not landed."
    case strategy of
      StrategyArchive    -> pure ()
      StrategyPrune      -> Aeson.parseFail
        "utxo.strategy: \"prune\" is not yet implemented. The Prep \
        \step that DELETEs consumed tx_out rows has not landed."
      StrategyFromLedger -> Aeson.parseFail
        "utxo.strategy: \"from_ledger\" is not yet implemented. The \
        \Prep step that bulk-loads live UTxO from the ledger state \
        \has not landed."
    pure UtxoOption
      { uoEnabled        = enabled
      , uoConsumedByTxId = consumedByTxId
      , uoTxIn           = txIn
      , uoStrategy       = strategy
      }

instance FromJSON UtxoStrategy where
  parseJSON = Aeson.withText "UtxoStrategy" $ \case
    "archive"     -> pure StrategyArchive
    "prune"       -> pure StrategyPrune
    "from_ledger" -> pure StrategyFromLedger
    other         -> Aeson.parseFail $
      "unexpected utxo.strategy: " <> show other
        <> ". Expected \"archive\", \"prune\", or \"from_ledger\"."

-- | Metadata storage format.
data MetadataFormat
  = MetadataText
  | MetadataJsonb
  | MetadataKeysOnly
  deriving stock (Eq, Show)

-- | Governance option variants.
data GovernanceVariant
  = GovernanceProposalsOnly
  | GovernanceFull
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * DB-sync node config (from db-sync-config.json — the book's file)
-- ---------------------------------------------------------------------------

-- | Fields extracted from db-sync-config.json (the file users download from
-- the Cardano book). We only extract what we need — NodeConfigFile to find
-- the real node config, plus optional metadata. All iohk-monitoring keys
-- and insert_options are ignored.
data DbSyncNodeConfig = DbSyncNodeConfig
  { dscNodeConfigFile :: !FilePath     -- ^ Path to the real node config.json (relative)
  , dscNetworkName    :: !(Maybe Text) -- ^ "mainnet", "preprod", etc.
  , dscPrometheusPort :: !(Maybe Int)  -- ^ Prometheus metrics port
  }
  deriving stock (Eq, Show)

instance FromJSON DbSyncNodeConfig where
  parseJSON = Aeson.withObject "DbSyncNodeConfig" $ \o ->
    DbSyncNodeConfig
      <$> o .:  "NodeConfigFile"
      <*> o .:? "NetworkName"
      <*> o .:? "PrometheusPort"

-- ---------------------------------------------------------------------------
-- * Node config (from config.json — the real cardano-node config)
-- ---------------------------------------------------------------------------

-- | Whether the network requires magic (testnets) or not (mainnet).
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

-- | Fields extracted from the cardano-node config.json.
-- Contains genesis file paths, hashes, network magic, and optional
-- hard fork trigger epochs (only present on testnets).
-- All logging/tracing/P2P keys are ignored.
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
    -- Hard fork triggers (optional — only on testnets)
  , ncTestShelleyHardForkAtEpoch :: !(Maybe Word64)
  , ncTestAllegraHardForkAtEpoch :: !(Maybe Word64)
  , ncTestMaryHardForkAtEpoch    :: !(Maybe Word64)
  , ncTestAlonzoHardForkAtEpoch  :: !(Maybe Word64)
  , ncTestBabbageHardForkAtEpoch :: !(Maybe Word64)
  , ncTestConwayHardForkAtEpoch  :: !(Maybe Word64)
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
      <*> o .:? "TestShelleyHardForkAtEpoch"
      <*> o .:? "TestAllegraHardForkAtEpoch"
      <*> o .:? "TestMaryHardForkAtEpoch"
      <*> o .:? "TestAlonzoHardForkAtEpoch"
      <*> o .:? "TestBabbageHardForkAtEpoch"
      <*> o .:? "TestConwayHardForkAtEpoch"

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
