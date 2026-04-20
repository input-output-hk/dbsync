-- | DDL generation.
--
-- Generates CREATE TABLE statements from 'TableDef' definitions.
-- During IngestChainHistory, tables are created as UNLOGGED with no
-- indexes; during PreparingForChainTip they are converted to LOGGED.
module DbSync.Db.Schema.Generate
  ( generateCreateTable
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Types (TableDef)

-- | Generate a CREATE TABLE DDL statement from a 'TableDef'.
generateCreateTable :: TableDef -> Text
generateCreateTable = panic "TODO: not implemented"
