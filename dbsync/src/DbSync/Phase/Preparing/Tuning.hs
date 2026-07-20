{-# LANGUAGE OverloadedStrings #-}

-- | Per-connection knobs the post-load pass leans on.
--
-- 'Phase.Preparing.Run.run' opens one dedicated control connection
-- for the duration of the pass. Applying these GUCs once at the top
-- of the run makes every subsequent index build, ANALYZE, and
-- @ALTER SET LOGGED@ run with the same tuning, without the noise of
-- per-statement @SET LOCAL@.
--
-- Why @SET@, not @SET LOCAL@: @SET LOCAL@ scopes to the current
-- transaction, and each step in 'Phase.Preparing.Run.run' goes
-- through its own implicit transaction. Session-scoped @SET@
-- persists for the connection's lifetime, which exactly matches the
-- pass.
module DbSync.Phase.Preparing.Tuning
  ( PrepTuning (..)
  , defaultPrepTuning
  , setPrepSessionGUCs
  , prepSessionGUCsSession
  ) where

import Cardano.Prelude

import qualified Hasql.Session as Sess

import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.Tuning (prepGucSql)
import DbSync.Db.Transaction (HasHasqlConnection (..))

-- | Tuning applied at the start of the post-load pass. Defaults are
-- sized for the 4-core / 16 GB target deployment; tests or operators
-- on different hardware override via 'defaultPrepTuning' record
-- updates.
data PrepTuning = PrepTuning
  { -- | Per-backend RAM cap for sort / index-build buffers. Larger
    -- values cut external-sort I/O for big B-tree builds. Sized
    -- against total RAM minus shared_buffers and OS page cache.
    ptMaintenanceWorkMem     :: !Text
    -- | Upper bound on parallel workers @CREATE INDEX@ and
    -- @VACUUM@ may launch. Silently capped by the server's
    -- @max_parallel_workers@; setting higher than core count is
    -- waste.
  , ptMaxParallelMaintenance :: !Int
    -- | @True@ → @synchronous_commit = off@ for the Prep session.
    -- Prep is idempotent on crash (Ingest's @sync_state@ still says
    -- not-complete until 'markSyncComplete' fires), so a lost
    -- commit just re-runs the pass.
  , ptAsyncCommit            :: !Bool
    -- | Backend count for the parallel-capable Prep steps
    -- (@ALTER … SET LOGGED@ flip and @CREATE INDEX@ build). Matches
    -- the 4-core target; tune down on smaller boxes.
  , ptPoolSize               :: !Int
  }
  deriving stock (Eq, Show)

-- | Defaults for a 4-core / 16 GB box: 2 GB maintenance_work_mem
-- alongside a typical shared_buffers leaves room for one Prep backend
-- plus the cardano-node IPC traffic.
defaultPrepTuning :: PrepTuning
defaultPrepTuning = PrepTuning
  { ptMaintenanceWorkMem     = "2GB"
  , ptMaxParallelMaintenance = 3
  , ptAsyncCommit            = True
  , ptPoolSize               = 4
  }

-- | The GUC-application step as a 'Sess.Session'. Used both by the
-- single-connection path (via 'setPrepSessionGUCs') and by the
-- 'Hasql.Pool' @initSession@ hook so every pool backend boots with
-- the same tuning.
prepSessionGUCsSession :: PrepTuning -> Sess.Session ()
prepSessionGUCsSession t = Sess.script (gucSql t)

-- | Issue the @SET@ statements that bring the env's connection up
-- to the requested 'PrepTuning'. Surfaces driver failures as
-- 'AppDatabaseError' — these are unconditionally valid GUCs, so a
-- failure here points at a connection-level problem.
setPrepSessionGUCs
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => PrepTuning -> m ()
setPrepSessionGUCs t = do
  conn <- asks getHasqlConnection
  useConn "Phase.Preparing.Tuning" conn (prepSessionGUCsSession t)

gucSql :: PrepTuning -> Text
gucSql t =
  prepGucSql
    (ptMaintenanceWorkMem t)
    (ptMaxParallelMaintenance t)
    (ptAsyncCommit t)
