-- | Migration runner.
--
-- Applies schema migrations in order, upgrading the database from
-- one schema version to the next. Migrations are idempotent and
-- run inside transactions.
module DbSync.Db.Schema.Migration
  ( -- TODO: runMigrations, Migration
  ) where

import Cardano.Prelude
