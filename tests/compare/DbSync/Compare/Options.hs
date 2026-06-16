module DbSync.Compare.Options
  ( Config (..)
  , parseConfig
  ) where

import Cardano.Prelude
import qualified Data.Text as T
import Options.Applicative

-- ---------------------------------------------------------------------------
-- * Configuration
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgOldDb :: !Text
  , cfgNewDb :: !Text
  , cfgHost :: !(Maybe Text)
  , cfgPort :: !(Maybe Int)
  , cfgUser :: !(Maybe Text)
  , cfgPassword :: !(Maybe Text)
  , cfgMaxEpoch :: !(Maybe Int64) -- ^ Overrides the auto-derived epoch ceiling.
  , cfgSeed :: !Int -- ^ Seeds the per-era block sampler; same seed picks the same blocks.
  , cfgSamples :: !Int -- ^ Blocks sampled per era.
  , cfgEras :: !(Maybe [Text]) -- ^ Restrict the spot-check to these eras; Nothing means all.
  , cfgRowCounts :: !Bool -- ^ Opt in to the slow epoch-bounded row counts.
  , cfgSpotCheck :: !Bool -- ^ Run the per-era content spot-check (on by default).
  }

-- ---------------------------------------------------------------------------
-- * Parser
-- ---------------------------------------------------------------------------

parseConfig :: IO Config
parseConfig = execParser opts
  where
    opts =
      info
        (configParser <**> helper)
        ( fullDesc
            <> progDesc
              "Compare an old cardano-db-sync database against a new dbsync database below a common epoch ceiling."
        )

configParser :: Parser Config
configParser =
  Config
    <$> strOption
      (long "old-db" <> metavar "DBNAME" <> value "dbsync_old" <> showDefault <> help "Old (cardano-db-sync) database name")
    <*> strOption
      (long "new-db" <> metavar "DBNAME" <> value "dbsync" <> showDefault <> help "New (dbsync) database name")
    <*> optional (strOption (long "host" <> metavar "HOST" <> help "PostgreSQL host for both databases"))
    <*> optional (option auto (long "port" <> metavar "PORT" <> help "PostgreSQL port for both databases"))
    <*> optional (strOption (long "user" <> metavar "USER" <> help "PostgreSQL user for both databases"))
    <*> optional (strOption (long "password" <> metavar "PASSWORD" <> help "PostgreSQL password for both databases"))
    <*> optional (option auto (long "max-epoch" <> metavar "EPOCH" <> help "Override the comparison epoch ceiling"))
    <*> option auto (long "seed" <> metavar "INT" <> value 42 <> showDefault <> help "Seed for the per-era block sampler")
    <*> option auto (long "samples" <> metavar "N" <> value 10 <> showDefault <> help "Blocks sampled per era")
    <*> optional (splitEras <$> strOption (long "eras" <> metavar "ERAS" <> help "Comma-separated eras to spot-check (default: all)"))
    <*> switch (long "row-counts" <> help "Also run the slow epoch-bounded row counts (scans full tables)")
    <*> flag True False (long "no-spot-check" <> help "Skip the per-era content spot-check")

-- Split a comma list into lower-cased, trimmed era names.
splitEras :: Text -> [Text]
splitEras = filter (not . T.null) . map (T.toLower . T.strip) . T.splitOn ","
