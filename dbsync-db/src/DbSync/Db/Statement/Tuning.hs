{-# LANGUAGE OverloadedStrings #-}

-- | SQL builders for per-session @SET@ tuning.
--
-- Prep opens a dedicated control connection and a backend pool for
-- the parallel-capable steps. Follow opens a long-lived per-phase
-- connection. Both apply session-scoped GUCs once at connection
-- open so subsequent statements pick them up automatically.
--
-- Session-scoped @SET@ persists for the connection's lifetime,
-- which matches both phases' shape. @SET LOCAL@ would have to be
-- reissued per transaction and would also fail for the @CREATE
-- INDEX CONCURRENTLY@ statements that must run outside any
-- transaction block.
module DbSync.Db.Statement.Tuning
  ( prepGucSql
  , followGucSql
  ) where

import Cardano.Prelude

import qualified Data.Text as T

-- | Multi-line @SET@ block applied at the top of the post-load
-- pass. Captures the three knobs Prep depends on: per-backend
-- memory cap for index builds, parallel-maintenance ceiling, and
-- the async-commit toggle.
--
-- The values are interpolated verbatim; callers are responsible
-- for supplying them in PostgreSQL's expected literal form.
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

-- | Single-line @SET@ applied when the Follow connection is opened.
-- Just the async-commit toggle today; the field-by-field shape
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
