-- | Types and 'FromJSON' decoders for the dbsync config file.
--
-- The config is network-agnostic and carries no credentials. The
-- operational paths live on the CLI and the PostgreSQL connection in
-- the @--pg-config@ file, so one config travels across networks.
--
-- The @core@ extractor is unconditional and absent from 'Extractors'.
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

    -- * Extractors
  , Extractors (..)
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
  , defaultExtractors
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
    -- ^ Unused. No production caller reads this section.
  , scLedger    :: !LedgerConfig
  , scExtractors :: !Extractors
  , scMetrics   :: !MetricsConfig
    -- ^ Unused. No metrics endpoint reads this section.
  , scLogging   :: !LoggingConfig
  }
  deriving stock (Eq, Show)

instance FromJSON SyncConfig where
  parseJSON = Aeson.withObject "SyncConfig" $ \o ->
    SyncConfig
      <$> o .:? "sync"       .!= defaultSyncSettings
      <*> o .:? "ledger"     .!= defaultLedgerConfig
      <*> o .:? "extractors" .!= defaultExtractors
      <*> o .:? "metrics"    .!= defaultMetricsConfig
      <*> o .:? "logging"    .!= defaultLoggingConfig

data SyncSettings = SyncSettings
  { ssMode :: !SyncMode
    -- ^ Unused. 'DbSync.App.Boot.decideBoot' picks the start phase.
  }
  deriving stock (Eq, Show)

instance FromJSON SyncSettings where
  parseJSON = Aeson.withObject "SyncSettings" $ \o ->
    SyncSettings
      <$> o .:? "mode" .!= SyncModeAuto

defaultSyncSettings :: SyncSettings
defaultSyncSettings = SyncSettings
  { ssMode = SyncModeAuto
  }

-- | Requested start phase. Parsed but not honoured: no caller reads
-- 'ssMode'.
data SyncMode
  = SyncModeAuto
  | SyncModeIngest
  | SyncModeFollow
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
    -- ^ Past this epoch the ledger writes a snapshot at every epoch
    -- boundary, whatever the cadence rules say. Below it the cadence
    -- is every 10 epochs in Ingest, and every epoch near tip in
    -- Follow. Tests lower the value so snapshots fire on the short
    -- fixture chains.
  }
  deriving stock (Eq, Show)

instance FromJSON LedgerConfig where
  parseJSON = Aeson.withObject "LedgerConfig" $ \o ->
    LedgerConfig
      <$> o .:? "enabled" .!= False
      <*> o .:? "backend" .!= defaultLedgerBackend
      <*> o .:? "snapshot_near_tip_epoch" .!= defaultSnapshotNearTipEpoch

defaultLedgerConfig :: LedgerConfig
defaultLedgerConfig = LedgerConfig
  { lcEnabled              = False
  , lcBackend              = defaultLedgerBackend
  , lcSnapshotNearTipEpoch = defaultSnapshotNearTipEpoch
  }

defaultSnapshotNearTipEpoch :: Word64
defaultSnapshotNearTipEpoch = 580

-- | Which backend stores the ledger-state UTxO tables. Only the
-- on-disk LSM backend exists.
--
-- The optional 'FilePath' overrides the directory. 'Nothing' uses the
-- directory passed to @mkHasLedgerEnv@.
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

data MetricsConfig = MetricsConfig
  { mcPrometheusPort :: !Int
    -- ^ Unused. The metrics HTTP endpoint is not wired up.
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

data LoggingConfig = LoggingConfig
  { lgLevel  :: !Text
  , lgFormat :: !LogFormat
    -- ^ Unused. The stderr tracer always writes text.
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

-- | Requested log format. Parsed but not honoured: no caller reads
-- 'lgFormat'.
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
-- * Extractors
-- ---------------------------------------------------------------------------

-- | Which extractors populate their tables. Omit a key to disable it;
-- set @"key": true@ to enable it. 'exEpoch' is the one key that
-- defaults to enabled.
--
-- 'exUtxo' has its own record because the UTxO extractor needs
-- several knobs; the rest are flat bools.
data Extractors = Extractors
  { exUtxo                  :: !UtxoOption
  , exMultiAsset            :: !OptionFlag
  , exMetadata              :: !OptionFlag
  , exStakeDelegation       :: !OptionFlag
  , exStakeDelegationLedger :: !OptionFlag
  , exPool                  :: !OptionFlag
  , exScriptsDatums         :: !OptionFlag
  , exGovernance            :: !OptionFlag
  , exCbor                  :: !OptionFlag
  , exEpochSyncStats        :: !OptionFlag
  , exEpochBoundary         :: !OptionFlag
  , exPoolStats             :: !OptionFlag
  , exEpoch                 :: !OptionFlag
  , exCurrentState          :: !OptionFlag
  , exOffChainPools         :: !OptionFlag
  , exOffChainVotes         :: !OptionFlag
  }
  deriving stock (Eq, Show)

instance FromJSON Extractors where
  parseJSON = Aeson.withObject "Extractors" $ \o ->
    Extractors
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

-- | Every optional extractor off except 'exEpoch', so the @epoch@
-- view machinery is available without an explicit opt-in.
defaultExtractors :: Extractors
defaultExtractors = Extractors
  { exUtxo                  = defaultUtxoOption
  , exMultiAsset            = OptionFlag False
  , exMetadata              = OptionFlag False
  , exStakeDelegation       = OptionFlag False
  , exStakeDelegationLedger = OptionFlag False
  , exPool                  = OptionFlag False
  , exScriptsDatums         = OptionFlag False
  , exGovernance            = OptionFlag False
  , exCbor                  = OptionFlag False
  , exEpochSyncStats        = OptionFlag False
  , exEpochBoundary         = OptionFlag False
  , exPoolStats             = OptionFlag False
  , exEpoch                 = OptionFlag True
  , exCurrentState          = OptionFlag False
  , exOffChainPools         = OptionFlag False
  , exOffChainVotes         = OptionFlag False
  }

-- | Wraps a 'Bool' so an option that grows variants can extend
-- without touching the 'Extractors' record.
data OptionFlag = OptionFlag
  { prEnabled :: !Bool
  }
  deriving stock (Eq, Show)

-- | Reads a plain JSON boolean, as in @"multi_asset": true@.
instance FromJSON OptionFlag where
  parseJSON = Aeson.withBool "OptionFlag" (pure . OptionFlag)

-- UTxO option types

data UtxoOption = UtxoOption
  { uoEnabled        :: !Bool
    -- ^ 'False' leaves @tx_in@, @tx_out@, and @ma_tx_out@ empty.
  , uoConsumedByTxId :: !Bool
    -- ^ 'True' populates @tx_out.consumed_by_tx_id@. The per-epoch
    -- worker covers most rows during Ingest; a residual UPDATE in
    -- Prep catches the cache misses.
  , uoTxIn           :: !Bool
    -- ^ 'True' writes @tx_in@ rows.
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

-- | Accepts the object form, or the bare boolean shorthand
-- @"utxo": true@, which matches the sibling 'OptionFlag' options.
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

-- | Metadata storage format. No decoder or caller reads it yet.
data MetadataFormat
  = MetadataText
  | MetadataJsonb
  | MetadataKeysOnly
  deriving stock (Eq, Show)

-- | Governance option variants. No decoder or caller reads it yet.
data GovernanceVariant
  = GovernanceProposalsOnly
  | GovernanceFull
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Node config (from config.json)
-- ---------------------------------------------------------------------------

data NetworkMagicConfig
  = RequiresNoMagic   -- ^ Mainnet
  | RequiresMagic     -- ^ Testnet; the magic comes from the genesis
  deriving stock (Eq, Show)

instance FromJSON NetworkMagicConfig where
  parseJSON = Aeson.withText "NetworkMagicConfig" $ \t ->
    case t of
      "RequiresNoMagic" -> pure RequiresNoMagic
      "RequiresMagic"   -> pure RequiresMagic
      _                 -> Aeson.typeMismatch
                             "NetworkMagicConfig (RequiresNoMagic|RequiresMagic)"
                             (Aeson.String t)

-- | The fields dbsync reads from the cardano-node @config.json@. It
-- ignores the logging, tracing, and P2P keys.
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
    -- Hard fork triggers; testnets only
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

data ConfigError
  = ConfigParseError !Text
  | ConfigMissingField !Text
  | ConfigValidationError !Text
  deriving stock (Eq, Show)

instance Exception ConfigError

-- ---------------------------------------------------------------------------
-- * Config parsing
-- ---------------------------------------------------------------------------

-- | Reads YAML or JSON.
parseConfig :: FilePath -> IO (Either ConfigError SyncConfig)
parseConfig fp =
  first (ConfigParseError . show) <$> Yaml.decodeFileEither fp

-- | Same as 'parseConfig', without disk I/O.
parseConfigBS :: ByteString -> Either ConfigError SyncConfig
parseConfigBS = first (ConfigParseError . show) . Yaml.decodeEither'
