-- Parses the three required arguments:
--   @--node-config@  — path to the cardano-node config file
--   @--node-socket@  — path to the cardano-node socket
--   @--config@       — path to the db-sync YAML config
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
  , (<**>)
  )

-- * Types

-- | Parsed CLI arguments.
data CliArgs = CliArgs
  { caNodeConfig :: !FilePath  -- ^ Path to the cardano-node config file
  , caNodeSocket :: !FilePath  -- ^ Path to the cardano-node socket
  , caConfig     :: !FilePath  -- ^ Path to the db-sync YAML config
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
      ( long "node-config"
          <> metavar "FILEPATH"
          <> help "Path to the cardano-node config file"
      )
    <*> strOption
      ( long "node-socket"
          <> metavar "FILEPATH"
          <> help "Path to the cardano-node socket"
      )
    <*> strOption
      ( long "config"
          <> metavar "FILEPATH"
          <> help "Path to the db-sync YAML config file"
      )

-- | Parse CLI args from the process arguments. Exits on failure.
parseCliArgs :: IO CliArgs
parseCliArgs = execParser cliArgsParser
