-- | Parser for the cardano-node @config.json@ passed via @--node-config@.
module DbSync.App.Config.Node
  ( parseNodeConfig
  ) where

import Cardano.Prelude

import qualified Data.Yaml as Yaml

import DbSync.App.Config.Types (ConfigError (..), NodeConfig)

parseNodeConfig :: FilePath -> IO (Either ConfigError NodeConfig)
parseNodeConfig fp =
  first (ConfigParseError . show) <$> Yaml.decodeFileEither fp
