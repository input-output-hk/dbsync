-- | CLI argument parsing for cardano-db-sync.
--
-- Parses the four required arguments:
--   @--db-sync-config@ — path to db-sync-config.json (from the Cardano book)
--   @--socket-path@    — path to the cardano-node Unix socket
--   @--state-dir@      — directory for checkpoints and ledger state
--   @--profile@        — path to dbsync-profile.json (database, options, sync mode)
module DbSync.Cli
  ( -- * Types
    CliArgs (..)

    -- * Parser
  , cliArgsParser
  , parseCliArgs
  ) where

import Cardano.Prelude

import Options.Applicative
  ( Parser
  , ParserInfo
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , strOption
  , switch
  )

-- * Types

-- | Parsed CLI arguments.
data CliArgs = CliArgs
  { caDbSyncConfig :: !FilePath  -- ^ Path to db-sync-config.json (from the Cardano book)
  , caSocketPath   :: !FilePath  -- ^ Path to the cardano-node Unix socket
  , caStateDir     :: !FilePath  -- ^ Directory for checkpoints + ledger state
  , caProfile      :: !FilePath  -- ^ Path to dbsync-profile.json (database, options, sync mode)
  , caForceResync  :: !Bool      -- ^ If 'True', drop the existing schema and re-sync from genesis
  }
  deriving stock (Eq, Show)

-- * Parser

-- | Full parser with help text and program description.
cliArgsParser :: ParserInfo CliArgs
cliArgsParser =
  info
    (cliArgsP <**> helper)
    ( fullDesc
        <> progDesc "Cardano blockchain to PostgreSQL synchronisation"
        <> header "cardano-db-sync — blockchain data indexer"
    )

-- | The raw argument parser (without help/info wrapper).
cliArgsP :: Parser CliArgs
cliArgsP =
  CliArgs
    <$> strOption
      ( long "db-sync-config"
          <> metavar "FILEPATH"
          <> help "Path to db-sync-config.json (from the Cardano book)"
      )
    <*> strOption
      ( long "socket-path"
          <> metavar "FILEPATH"
          <> help "Path to the cardano-node Unix socket"
      )
    <*> strOption
      ( long "state-dir"
          <> metavar "DIRPATH"
          <> help "Directory for checkpoints and ledger state"
      )
    <*> strOption
      ( long "profile"
          <> metavar "FILEPATH"
          <> help "Path to profile.json (database, sync options, logging)"
      )
    <*> switch
      ( long "force-resync"
          <> help "Drop any existing schema and re-sync from genesis (destructive)"
      )

-- | Parse CLI args from the process arguments. Exits on failure.
parseCliArgs :: IO CliArgs
parseCliArgs = execParser cliArgsParser
