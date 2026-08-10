-- | CLI argument parsing for cardano-db-sync. Flag meanings live
-- on the @help@ text in 'cliArgsP'.
module DbSync.App.Cli
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
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , strOption
  , switch
  )

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

data CliArgs = CliArgs
  { caConfig          :: !FilePath  -- ^ The dbsync config file
  , caPgConfig        :: !FilePath  -- ^ The PostgreSQL connection file
  , caNodeConfig      :: !FilePath  -- ^ The cardano-node config.json; genesis
                                    --   paths resolve relative to it
  , caSocketPath      :: !FilePath  -- ^ The cardano-node Unix socket
  , caLedgerStateDir  :: !FilePath  -- ^ Parent directory. @dbsync-ledger/@ is
                                    --   created under it
  , caResyncFromGenesis :: !Bool    -- ^ 'True' wipes the schema and the ledger
                                    --   state, then re-syncs from genesis
  , caRollbackToSlot  :: !(Maybe Word64)
    -- ^ Roll the database back to the nearest block at or after this
    -- slot, then boot normally. A recovery hatch with no migration
    -- semantics. 'Nothing' is the normal case.
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Parser
-- ---------------------------------------------------------------------------

cliArgsParser :: ParserInfo CliArgs
cliArgsParser =
  info
    (cliArgsP <**> helper)
    ( fullDesc
        <> progDesc "Cardano blockchain to PostgreSQL synchronisation"
        <> header "cardano-db-sync — blockchain data indexer"
    )

cliArgsP :: Parser CliArgs
cliArgsP =
  CliArgs
    <$> strOption
      ( long "config"
          <> metavar "FILEPATH"
          <> help "Path to the dbsync config file (sync mode, ledger, extractors, logging)"
      )
    <*> strOption
      ( long "pg-config"
          <> metavar "FILEPATH"
          <> help "Path to the PostgreSQL connection file (host, port, name, user, password_file)"
      )
    <*> strOption
      ( long "node-config"
          <> metavar "FILEPATH"
          <> help "Path to the cardano-node config.json; genesis files are resolved relative to it"
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
              \will be created and used for the LSM session and snapshot headers"
      )
    <*> switch
      ( long "resync-from-genesis"
          <> help "Wipe the database schema and ledger state, then re-sync from genesis (destructive)"
      )
    <*> optional
      ( option auto
          ( long "rollback-to-slot"
              <> metavar "SLOTNO"
              <> help
                  "Roll the database back to the nearest block at or after \
                  \SLOTNO before starting the normal sync flow. Empty slots \
                  \are tolerated — the rollback resolves to the smallest \
                  \block with slot_no >= SLOTNO."
          )
      )

-- | Exits on a parse failure.
parseCliArgs :: IO CliArgs
parseCliArgs = execParser cliArgsParser
