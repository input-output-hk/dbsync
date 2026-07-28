-- | PostgreSQL connection settings, read from the @--pg-config@ file.
--
-- Kept separate from the behaviour config so deployments can mount
-- the connection details (and the password file they point at) as
-- secrets without touching the rest of the configuration. The file
-- never carries an inline password — only an optional
-- @password_file@ path.
module DbSync.App.Config.Database
  ( -- * Types
    DatabaseConfig (..)

    -- * Parsing
  , parseDatabaseConfig
  ) where

import Cardano.Prelude

import qualified Control.Exception as Exception
import Data.Aeson (FromJSON (..), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.FilePath (isAbsolute, takeDirectory, (</>))

import DbSync.App.Config.Types (ConfigError (..))

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Connection settings with the password already resolved.
data DatabaseConfig = DatabaseConfig
  { dcHost     :: !Text
  , dcPort     :: !Int
  , dcName     :: !Text
  , dcUser     :: !Text
  , dcPassword :: !Text
    -- ^ Contents of @password_file@ (trailing newlines stripped);
    -- empty when no @password_file@ is configured.
  }
  deriving stock (Eq, Show)

-- | The pg-config file as written on disk: @host@ and @name@ are
-- required, the password only ever arrives via @password_file@.
data RawDatabaseConfig = RawDatabaseConfig
  { rdcHost         :: !Text
  , rdcPort         :: !Int
  , rdcName         :: !Text
  , rdcUser         :: !Text
  , rdcPasswordFile :: !(Maybe FilePath)
  }

instance FromJSON RawDatabaseConfig where
  parseJSON = Aeson.withObject "DatabaseConfig" $ \o ->
    RawDatabaseConfig
      <$> o .:  "host"
      <*> o .:? "port" .!= 5432
      <*> o .:  "name"
      <*> o .:? "user" .!= "postgres"
      <*> o .:? "password_file"

-- ---------------------------------------------------------------------------
-- * Parsing
-- ---------------------------------------------------------------------------

-- | Parse a pg-config file (YAML or JSON) and resolve its
-- @password_file@.
--
-- A relative @password_file@ is resolved against the pg-config
-- file's own directory. Trailing newlines are stripped from the
-- file contents (mounted secrets usually end in one); any other
-- whitespace is preserved.
parseDatabaseConfig :: FilePath -> IO (Either ConfigError DatabaseConfig)
parseDatabaseConfig fp = do
  parsed <- first (ConfigParseError . show) <$> Yaml.decodeFileEither fp
  case parsed of
    Left err  -> pure (Left err)
    Right raw -> resolvePassword fp raw

resolvePassword :: FilePath -> RawDatabaseConfig -> IO (Either ConfigError DatabaseConfig)
resolvePassword fp raw = case rdcPasswordFile raw of
  Nothing -> pure (Right (fromRaw raw ""))
  Just pf -> do
    let path = if isAbsolute pf then pf else takeDirectory fp </> pf
    result <- Exception.try (TIO.readFile path)
    pure $ case result of
      Left (e :: Exception.IOException) ->
        Left (ConfigParseError ("password_file " <> show path <> ": " <> show e))
      Right contents ->
        let !pw = T.dropWhileEnd (\c -> c == '\n' || c == '\r') contents
        in Right (fromRaw raw pw)

fromRaw :: RawDatabaseConfig -> Text -> DatabaseConfig
fromRaw raw pw = DatabaseConfig
  { dcHost     = rdcHost raw
  , dcPort     = rdcPort raw
  , dcName     = rdcName raw
  , dcUser     = rdcUser raw
  , dcPassword = pw
  }
