-- | Schema version tracking.
--
-- Manages the schema_version table, which records which schema version
-- is currently deployed. Used to decide whether migrations are needed
-- on startup.
module DbSync.Db.Schema.Version
  ( -- TODO: getSchemaVersion, setSchemaVersion, SchemaVersion
  ) where

import Cardano.Prelude
