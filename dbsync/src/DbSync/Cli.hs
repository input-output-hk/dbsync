-- | CLI argument parsing for cardano-db-sync.
--
-- Parses the four required arguments:
--   @--db-sync-config@     — path to db-sync-config.json (from the Cardano book)
--   @--socket-path@        — path to the cardano-node Unix socket
--   @--ledger-state-dir@   — parent directory in which the @dbsync-ledger/@
--                            sub-directory will be created and used for the
--                            LSM session and consensus snapshots
--   @--profile@            — path to dbsync-profile.json (database, options, sync mode)
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
  { caDbSyncConfig    :: !FilePath  -- ^ Path to db-sync-config.json (from the Cardano book)
  , caSocketPath      :: !FilePath  -- ^ Path to the cardano-node Unix socket
  , caLedgerStateDir  :: !FilePath  -- ^ Parent directory under which the @dbsync-ledger/@
                                    --   sub-directory is created (LSM session + snapshots)
  , caProfile         :: !FilePath  -- ^ Path to dbsync-profile.json (database, options, sync mode)
  , caResyncFromGenesis :: !Bool    -- ^ If 'True', wipe the schema + ledger state and re-sync from genesis
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
      ( long "ledger-state-dir"
          <> metavar "DIRPATH"
          <> help
              "Parent directory in which a 'dbsync-ledger/' sub-directory \
              \will be created and used for the LSM session and consensus snapshots"
      )
    <*> strOption
      ( long "profile"
          <> metavar "FILEPATH"
          <> help "Path to profile.json (database, sync options, logging)"
      )
    <*> switch
      ( long "resync-from-genesis"
          <> help "Wipe the database schema and ledger state, then re-sync from genesis (destructive)"
      )

-- | Parse CLI args from the process arguments. Exits on failure.
parseCliArgs :: IO CliArgs
parseCliArgs = execParser cliArgsParser
