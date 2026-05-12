-- | Node configuration parsing — two stages.
--
-- Stage 1: Parse @db-sync-config.json@ (from the Cardano book) to extract
-- the @NodeConfigFile@ path that points to the real node config.
--
-- Stage 2: Parse the referenced @config.json@ to extract genesis file paths,
-- hashes, network magic, and optional hard fork triggers.
module DbSync.Config.Node
  ( -- * Stage 1: db-sync-config.json
    parseDbSyncNodeConfig

    -- * Stage 2: config.json (the real node config)
  , parseNodeConfig
  ) where

import Cardano.Prelude

import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml

import DbSync.Config.Types (ConfigError (..), DbSyncNodeConfig, NodeConfig)

-- | Stage 1: Parse db-sync-config.json to extract the NodeConfigFile path.
-- Ignores all iohk-monitoring keys and insert_options — only extracts
-- NodeConfigFile, NetworkName, and PrometheusPort.
parseDbSyncNodeConfig :: FilePath -> IO (Either ConfigError DbSyncNodeConfig)
parseDbSyncNodeConfig = parseYamlConfig

-- | Stage 2: Parse the cardano-node config.json.
-- Extracts genesis file paths, hashes, network magic, and optional
-- hard fork triggers. Ignores all logging/tracing/P2P keys.
parseNodeConfig :: FilePath -> IO (Either ConfigError NodeConfig)
parseNodeConfig = parseYamlConfig

-- | Decode a YAML/JSON file, wrapping any parse failure in
-- 'ConfigParseError'.
parseYamlConfig :: FromJSON a => FilePath -> IO (Either ConfigError a)
parseYamlConfig fp =
  first (ConfigParseError . show) <$> Yaml.decodeFileEither fp
