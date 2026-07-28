{-# LANGUAGE OverloadedStrings #-}

-- | Inputs to 'DbSync.App.Run.runApp'.
--
-- The executable parses the CLI, reads config / pg-config /
-- node-config / genesis files, and assembles an 'AppArgs'. Tests
-- assemble one directly from a 'MockNode' plus a hand-built
-- 'SyncConfig'. Either way, 'runApp' is the single entry point that
-- carries the orchestration body.
module DbSync.App.Args
  ( AppArgs (..)
  ) where

import Cardano.Prelude

import DbSync.App.Config.Database (DatabaseConfig)
import DbSync.App.Config.Genesis (GenesisConfig)
import DbSync.App.Config.Types (NodeConfig, SyncConfig)
import DbSync.StateQuery.Types (StateQueryVar)

-- | Everything 'DbSync.App.Run.runApp' needs to boot a sync.
--
-- 'aaShutdownSignal' and 'aaStateQueryVar' are test hooks; the
-- executable passes 'Nothing' for both.
data AppArgs = AppArgs
  { aaConfig            :: !SyncConfig
    -- ^ Behaviour config (enabled extractors, ledger flags,
    --   sync mode, logging).
  , aaDatabase          :: !DatabaseConfig
    -- ^ PostgreSQL connection settings, password already resolved.
  , aaNodeConfig        :: !NodeConfig
    -- ^ Parsed cardano-node configuration.
  , aaGenesisConfig     :: !GenesisConfig
    -- ^ Per-era genesis configs (Byron, Shelley, Alonzo, Conway).
  , aaSocketPath        :: !FilePath
    -- ^ Path to the local cardano-node IPC socket.
  , aaLedgerStateDir    :: !FilePath
    -- ^ Parent directory under which @dbsync-ledger\/@ holds ledger
    --   snapshots and the LSM session.
  , aaResyncFromGenesis :: !Bool
    -- ^ When 'True', wipe persistent state and re-sync from origin.
  , aaRollbackToSlot    :: !(Maybe Word64)
    -- ^ Explicit CLI rollback request. Takes precedence over the
    --   'pending_rollback_slot' marker when both are set.
  , aaShutdownSignal    :: !(Maybe (IO ()))
    -- ^ When set, race the Follow loop against this action and exit
    --   when it returns. Lets tests stop the app cleanly.
  , aaStateQueryVar     :: !(Maybe StateQueryVar)
    -- ^ Pre-seeded state-query handle. Production passes 'Nothing'
    --   and the node interpreter is acquired via LocalStateQuery.
    --   Tests against the mock chain-sync server (which stubs
    --   LocalStateQuery) supply a handle pre-seeded from the local
    --   forging interpreter so 'parseBlock' never blocks waiting
    --   for the node.
  }
