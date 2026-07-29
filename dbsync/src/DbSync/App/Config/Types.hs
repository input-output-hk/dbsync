-- | Types and 'FromJSON' decoders for the dbsync config file.
--
-- The config is network-agnostic and carries no credentials:
-- operational paths (socket, ledger state dir) live on the CLI and
-- the PostgreSQL connection in the @--pg-config@ file, so the same
-- config travels across mainnet, preprod, etc.
--
-- @db_profile@ is opt-in: every extractor defaults to disabled and
-- must be enabled explicitly. The @core@ extractor is unconditional
-- and not represented in 'DbProfile'.
module DbSync.App.Config.Types
  ( -- * Top-level config
    SyncConfig (..)
  , SyncSettings (..)
  , SyncMode (..)
  , LedgerConfig (..)
  , LedgerBackend (..)
  , MetricsConfig (..)
  , LoggingConfig (..)
  , LogFormat (..)

    -- * DB profile
  , DbProfile (..)
  , OptionFlag (..)
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
  , defaultDbProfile
  , defaultUtxoOption

    -- * Node config (from config.json)
  , NodeConfig (..)
  , NetworkMagicConfig (..)

    -- * Errors
  , ConfigError (..)

    -- * Config parsing
  , parseConfig
  , parseConfigBS
  ) where

import Cardano.Prelude

import Data.Aeson (FromJSON (..), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseFail, typeMismatch)
import qualified Data.Yaml as Yaml

-- ---------------------------------------------------------------------------
-- * Top-level config
-- ---------------------------------------------------------------------------

-- | Top-level sync configuration. Every section is optional, so an
-- empty object is a valid (all-defaults) config.
data SyncConfig = SyncConfig
  { scSync      :: !SyncSettings
  , scLedger    :: !LedgerConfig
  , scDbProfile :: !DbProfile
  , scMetrics   :: !MetricsConfig
  , scLogging   :: !LoggingConfig
  }
  deriving stock (Eq, Show)

instance FromJSON SyncConfig where
  parseJSON = Aeson.withObject "SyncConfig" $ \o ->
    SyncConfig
      <$> o .:? "sync"       .!= defaultSyncSettings
      <*> o .:? "ledger"     .!= defaultLedgerConfig
      <*> o .:? "db_profile" .!= defaultDbProfile
      <*> o .:? "metrics"    .!= defaultMetricsConfig
      <*> o .:? "logging"    .!= defaultLoggingConfig

-- | Sync behaviour settings.
data SyncSettings = SyncSettings
  { ssMode :: !SyncMode
  }
  deriving stock (Eq, Show)

instance FromJSON SyncSettings where
  parseJSON = Aeson.withObject "SyncSettings" $ \o ->
    SyncSettings
      <$> o .:? "mode" .!= SyncModeAuto

-- | Default sync settings used when the "sync" section is omitted.
defaultSyncSettings :: SyncSettings
defaultSyncSettings = SyncSettings
  { ssMode = SyncModeAuto
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

-- | Production default for 'lcSnapshotNearTipEpoch'.
defaultSnapshotNearTipEpoch :: Word64
defaultSnapshotNearTipEpoch = 580

-- | Which backend stores the ledger-state UTxO tables. Only the
-- on-disk LSM backend is supported; the 'FromJSON' instance rejects
-- @"inmemory"@ with an explanatory error.
--
-- The optional 'FilePath' is a directory override; 'Nothing' uses
-- the directory passed to @mkHasLedgerEnv@ (derived from
-- @--ledger-state-dir@).
data LedgerBackend
  = LedgerBackendLSM !(Maybe FilePath)
  deriving stock (Eq, Show)

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
-- * DB profile
-- ---------------------------------------------------------------------------

-- | Per-extractor configuration: which tables get populated. Omit a
-- key to disable; set @"key": true@ to enable.
--
-- 'pcUtxo' has its own record because the UTxO extractor needs
-- multiple knobs; the rest are flat bools.
data DbProfile = DbProfile
  { pcUtxo                  :: !UtxoOption
  , pcMultiAsset            :: !OptionFlag
  , pcMetadata              :: !OptionFlag
  , pcStakeDelegation       :: !OptionFlag
  , pcStakeDelegationLedger :: !OptionFlag
  , pcPool                  :: !OptionFlag
  , pcScriptsDatums         :: !OptionFlag
  , pcGovernance            :: !OptionFlag
  , pcCbor                  :: !OptionFlag
  , pcEpochSyncStats        :: !OptionFlag
  , pcEpochBoundary         :: !OptionFlag
  , pcPoolStats             :: !OptionFlag
  , pcEpoch                 :: !OptionFlag
  , pcCurrentState          :: !OptionFlag
  , pcOffChainPools         :: !OptionFlag
  , pcOffChainVotes         :: !OptionFlag
  }
  deriving stock (Eq, Show)

instance FromJSON DbProfile where
  parseJSON = Aeson.withObject "DbProfile" $ \o ->
    DbProfile
      <$> o .:? "utxo"                    .!= defaultUtxoOption
      <*> o .:? "multi_asset"             .!= disabled
      <*> o .:? "metadata"                .!= disabled
      <*> o .:? "stake_delegation"        .!= disabled
      <*> o .:? "stake_delegation_ledger" .!= disabled
      <*> o .:? "pool"                    .!= disabled
      <*> o .:? "scripts_datums"          .!= disabled
      <*> o .:? "governance"              .!= disabled
      <*> o .:? "cbor"                    .!= disabled
      <*> o .:? "epoch_sync_stats"        .!= disabled
      <*> o .:? "epoch_boundary"          .!= disabled
      <*> o .:? "pool_stats"              .!= disabled
      <*> o .:? "epoch"                   .!= epochDefault
      <*> o .:? "current_state"           .!= disabled
      <*> o .:? "off_chain_pools"         .!= disabled
      <*> o .:? "off_chain_votes"         .!= disabled
    where
      disabled     = OptionFlag False
      epochDefault = OptionFlag True

-- | Default profile used when @"db_profile"@ is omitted: every
-- optional extractor off /except/ 'pcEpoch', so the @epoch@ view
-- machinery is available without an explicit opt-in.
defaultDbProfile :: DbProfile
defaultDbProfile = DbProfile
  { pcUtxo                  = defaultUtxoOption
  , pcMultiAsset            = OptionFlag False
  , pcMetadata              = OptionFlag False
  , pcStakeDelegation       = OptionFlag False
  , pcStakeDelegationLedger = OptionFlag False
  , pcPool                  = OptionFlag False
  , pcScriptsDatums         = OptionFlag False
  , pcGovernance            = OptionFlag False
  , pcCbor                  = OptionFlag False
  , pcEpochSyncStats        = OptionFlag False
  , pcEpochBoundary         = OptionFlag False
  , pcPoolStats             = OptionFlag False
  , pcEpoch                 = OptionFlag True
  , pcCurrentState          = OptionFlag False
  , pcOffChainPools         = OptionFlag False
  , pcOffChainVotes         = OptionFlag False
  }

-- | Configuration for a single option.
--
-- Wraps a 'Bool' explicitly so options that grow variants (e.g.
-- multi-asset policy allowlists, metadata key filters) can extend
-- without touching the @DbProfile@ record.
data OptionFlag = OptionFlag
  { prEnabled :: !Bool
  }
  deriving stock (Eq, Show)

-- | Parse a sync option from a plain JSON boolean (e.g. @"multi_asset": true@).
instance FromJSON OptionFlag where
  parseJSON = Aeson.withBool "OptionFlag" (pure . OptionFlag)

-- UTxO option types

-- | UTxO extractor configuration.
data UtxoOption = UtxoOption
  { uoEnabled        :: !Bool
    -- ^ Whether the extractor runs at all. When 'False', @tx_in@,
    -- @tx_out@, and @ma_tx_out@ stay empty.
  , uoConsumedByTxId :: !Bool
    -- ^ Whether @tx_out.consumed_by_tx_id@ is populated. The
    -- per-epoch consumed-by worker covers most rows during Ingest;
    -- a Prep residual UPDATE catches cache-misses.
  , uoTxIn           :: !Bool
    -- ^ Whether @tx_in@ rows are written.
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

-- | Accepts the object form or a bare boolean shorthand
-- (@"utxo": true@ ≡ defaults with @enabled@ set), matching the
-- 'OptionFlag' ergonomics of the sibling options.
instance FromJSON UtxoOption where
  parseJSON (Aeson.Bool b) = pure defaultUtxoOption { uoEnabled = b }
  parseJSON v = flip (Aeson.withObject "UtxoOption") v $ \o -> do
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
-- * Node config (from config.json)
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

-- | Fields we read from the cardano-node config.json: genesis
-- file paths, hashes, network magic, and optional hard-fork
-- trigger epochs (testnets only). Logging, tracing, and P2P keys
-- are ignored.
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

-- ---------------------------------------------------------------------------
-- * Config parsing
-- ---------------------------------------------------------------------------

-- | Parse a dbsync config file (YAML or JSON) from a file path.
parseConfig :: FilePath -> IO (Either ConfigError SyncConfig)
parseConfig fp =
  first (ConfigParseError . show) <$> Yaml.decodeFileEither fp

-- | Parse a db-sync config from a raw ByteString. Useful for testing
-- without disk I/O.
parseConfigBS :: ByteString -> Either ConfigError SyncConfig
parseConfigBS = first (ConfigParseError . show) . Yaml.decodeEither'
