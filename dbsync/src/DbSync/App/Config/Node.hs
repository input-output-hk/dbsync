-- | Node-config parsers.
module DbSync.App.Config.Node
  ( parseDbSyncNodeConfig
  , parseNodeConfig
  ) where

import Cardano.Prelude

import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml

import DbSync.App.Config.Types (ConfigError (..), DbSyncNodeConfig, NodeConfig)

-- | Parse the user-supplied @db-sync-config.json@; its
-- @NodeConfigFile@ field points to the real cardano-node config.
parseDbSyncNodeConfig :: FilePath -> IO (Either ConfigError DbSyncNodeConfig)
parseDbSyncNodeConfig = parseYamlConfig

-- | Parse the cardano-node @config.json@ pointed at by 'DbSyncNodeConfig'.
parseNodeConfig :: FilePath -> IO (Either ConfigError NodeConfig)
parseNodeConfig = parseYamlConfig

-- | Decode a YAML/JSON file, wrapping any parse failure in 'ConfigParseError'.
parseYamlConfig :: FromJSON a => FilePath -> IO (Either ConfigError a)
parseYamlConfig fp =
  first (ConfigParseError . show) <$> Yaml.decodeFileEither fp
