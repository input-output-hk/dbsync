{-# LANGUAGE OverloadedStrings #-}

-- | Inputs to 'DbSync.App.Run.runApp'. The executable builds one from
-- the CLI and the on-disk config files; tests build one from a
-- 'MockNode' and a hand-written 'SyncConfig'.
module DbSync.App.Args
  ( AppArgs (..)
  ) where

import Cardano.Prelude

import DbSync.App.Config.Database (DatabaseConfig)
import DbSync.App.Config.Genesis (GenesisConfig)
import DbSync.App.Config.Types (NodeConfig, SyncConfig)
import DbSync.StateQuery.Types (StateQueryVar)

-- | Everything 'DbSync.App.Run.runApp' needs to boot a sync.
data AppArgs = AppArgs
  { aaConfig            :: !SyncConfig
  , aaDatabase          :: !DatabaseConfig
    -- ^ Password already resolved.
  , aaNodeConfig        :: !NodeConfig
  , aaGenesisConfig     :: !GenesisConfig
  , aaSocketPath        :: !FilePath
    -- ^ The local cardano-node IPC socket.
  , aaLedgerStateDir    :: !FilePath
    -- ^ Parent directory. @dbsync-ledger\/@ under it holds the ledger
    --   snapshots and the LSM session.
  , aaResyncFromGenesis :: !Bool
    -- ^ 'True' wipes the persistent state and re-syncs from origin.
  , aaRollbackToSlot    :: !(Maybe Word64)
    -- ^ CLI rollback request. Wins over the
    --   @pending_rollback_slot@ marker when both are set.
  , aaShutdownSignal    :: !(Maybe (IO ()))
    -- ^ Test hook. When set, the Follow loop races this action and
    --   exits when it returns. The executable passes 'Nothing'.
  , aaStateQueryVar     :: !(Maybe StateQueryVar)
    -- ^ Test hook. The mock chain-sync server stubs LocalStateQuery,
    --   so tests pre-seed a handle from the local forging interpreter
    --   and 'parseBlock' never blocks on the node. The executable
    --   passes 'Nothing' and acquires the interpreter over
    --   LocalStateQuery.
  }
