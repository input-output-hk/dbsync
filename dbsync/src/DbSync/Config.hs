-- | Configuration file parsing.
--
-- Reads and parses the db-sync YAML configuration file into 'SyncConfig':
-- read file as ByteString, YAML decode via FromJSON instances, return
-- 'Either' on failure.
module DbSync.Config
  ( -- * Parsing
    parseConfig
  , parseConfigBS
  ) where

import Cardano.Prelude

import qualified Data.Yaml as Yaml

import DbSync.Config.Types (ConfigError (..), SyncConfig)

-- | Parse a db-sync YAML config file from a file path.
parseConfig :: FilePath -> IO (Either ConfigError SyncConfig)
parseConfig fp = do
  result <- Yaml.decodeFileEither fp
  pure $ case result of
    Left err  -> Left $ ConfigParseError (show err)
    Right cfg -> Right cfg

-- | Parse a db-sync config from a raw ByteString.
-- Useful for testing without disk I/O.
parseConfigBS :: ByteString -> Either ConfigError SyncConfig
parseConfigBS bs =
  case Yaml.decodeEither' bs of
    Left err  -> Left $ ConfigParseError (show err)
    Right cfg -> Right cfg
