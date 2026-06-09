{-# LANGUAGE OverloadedStrings #-}

-- | Per-session @SET@ tuning applied once at connection open.
-- Session-scoped (vs @SET LOCAL@) so subsequent statements pick it
-- up automatically and so @CREATE INDEX CONCURRENTLY@ — which must
-- run outside any transaction block — sees the settings.
module DbSync.Db.Statement.Tuning
  ( prepGucSql
  , followGucSql
  ) where

import Cardano.Prelude

import qualified Data.Text as T

-- | Multi-line @SET@ block for the post-load pass: per-backend memory
-- cap for index builds, parallel-maintenance ceiling, async-commit
-- toggle. Values are interpolated verbatim.
prepGucSql
  :: Text   -- ^ @maintenance_work_mem@ (e.g. @"2GB"@).
  -> Int    -- ^ @max_parallel_maintenance_workers@.
  -> Bool   -- ^ @True@ → @synchronous_commit = off@.
  -> Text
prepGucSql memCap maxParallel asyncCommit = T.unlines
  [ "SET maintenance_work_mem = '" <> memCap <> "';"
  , "SET max_parallel_maintenance_workers = " <> show maxParallel <> ";"
  , "SET synchronous_commit = " <> commitMode asyncCommit <> ";"
  ]

-- | @SET@ applied at Follow connection open. Field-by-field shape
-- mirrors 'prepGucSql' so future Follow knobs slot in without
-- changing the call shape.
followGucSql
  :: Bool   -- ^ @True@ → @synchronous_commit = off@.
  -> Text
followGucSql asyncCommit = T.unlines
  [ "SET synchronous_commit = " <> commitMode asyncCommit <> ";"
  ]

commitMode :: Bool -> Text
commitMode True  = "off"
commitMode False = "on"
