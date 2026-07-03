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
  , cfgMaxEpoch :: !(Maybe Int64) -- ^ Overrides the auto-derived epoch ceiling (spot-check era ranges).
  , cfgMaxBlock :: !(Maybe Int64) -- ^ Overrides the auto-derived block_no ceiling (row counts).
  , cfgSeed :: !Int -- ^ Seeds the per-era block sampler; same seed picks the same blocks.
  , cfgSamples :: !Int -- ^ Blocks sampled per era.
  , cfgEras :: !(Maybe [Text]) -- ^ Restrict the spot-check to these eras; Nothing means all.
  , cfgRowCounts :: !Bool -- ^ Run the block-bounded row counts and dedup content checks (off by default; full-table scans).
  , cfgSpotCheck :: !Bool -- ^ Run the per-era content spot-check (on by default).
  , cfgStructure :: !Bool -- ^ Compare schema structure: columns, foreign keys, uniques (on by default).
  , cfgStatementTimeout :: !Int -- ^ Per-statement timeout in seconds; a slow query fails instead of hanging.
  , cfgVerbose :: !Bool -- ^ Echo every SQL statement to stderr before running it.
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
    <*> optional (option auto (long "max-block" <> metavar "BLOCK" <> help "Override the comparison block_no ceiling"))
    <*> option auto (long "seed" <> metavar "INT" <> value 42 <> showDefault <> help "Seed for the per-era block sampler")
    <*> option auto (long "samples" <> metavar "N" <> value 10 <> showDefault <> help "Blocks sampled per era")
    <*> optional (splitEras <$> strOption (long "eras" <> metavar "ERAS" <> help "Comma-separated eras to spot-check (default: all)"))
    <*> flag False True (long "row-counts" <> help "Run the block-bounded row counts and dedup content checks (slow; full-table scans)")
    <*> flag True False (long "no-spot-check" <> help "Skip the per-era content spot-check")
    <*> flag True False (long "no-structure" <> help "Skip the schema structure comparison (columns, foreign keys, uniques)")
    <*> option auto (long "statement-timeout" <> metavar "SECONDS" <> value 120 <> showDefault <> help "Per-statement timeout; a slow query fails instead of hanging")
    <*> flag False True (long "verbose" <> help "Echo each SQL statement to stderr before running it")

-- Split a comma list into lower-cased, trimmed era names.
splitEras :: Text -> [Text]
splitEras = filter (not . T.null) . map (T.toLower . T.strip) . T.splitOn ","
