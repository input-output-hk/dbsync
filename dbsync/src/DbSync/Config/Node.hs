-- | Node configuration parsing.
--
-- Extracts the subset of the cardano-node config JSON that db-sync needs.
-- The node config contains many keys (tracing, P2P, etc.) that we ignore —
-- we only parse genesis file paths, hashes, and network magic.
-- Follows the original project's pattern in Cardano.DbSync.Config.Node.
module DbSync.Config.Node
  ( -- * Parsing
    parseNodeConfig
  ) where

import Cardano.Prelude

import qualified Data.Yaml as Yaml

import DbSync.Config.Types (ConfigError (..), NodeConfig)

-- | Parse a cardano-node config JSON file, extracting fields relevant to db-sync.
-- Ignores all logging/tracing/P2P keys — only extracts genesis paths and network magic.
parseNodeConfig :: FilePath -> IO (Either ConfigError NodeConfig)
parseNodeConfig fp = do
  result <- Yaml.decodeFileEither fp
  pure $ case result of
    Left err  -> Left $ ConfigParseError (show err)
    Right cfg -> Right cfg
