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
-- transaction. Each step in 'Phase.Preparing.Run.run' goes through
-- its own implicit transaction (in fact @CREATE INDEX CONCURRENTLY@
-- in a hypothetical Follow-time variant /must/ run outside a
-- transaction block). Session-scoped @SET@ persists for the
-- connection's lifetime, which exactly matches the pass.
module DbSync.Phase.Preparing.Tuning
  ( PrepTuning (..)
  , defaultPrepTuning
  , setPrepSessionGUCs
  , prepSessionGUCsSession
  ) where

import Cardano.Prelude

import qualified Data.Text as T
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

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

-- | Defaults for a 4-core / 16 GB box. With 4 GB shared_buffers and
-- ~4 GB OS page cache, 2 GB maintenance_work_mem leaves room for
-- one Prep backend plus the cardano-node IPC traffic.
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
-- to the requested 'PrepTuning'. Panics on driver failure — these
-- are unconditionally valid GUCs and a failure here points at a
-- connection-level problem worth surfacing immediately.
setPrepSessionGUCs
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => PrepTuning -> m ()
setPrepSessionGUCs t = do
  conn <- asks getHasqlConnection
  result <- liftIO $ Conn.use conn (prepSessionGUCsSession t)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "Phase.Preparing.Tuning: " <> show e

gucSql :: PrepTuning -> Text
gucSql t = T.unlines
  [ "SET maintenance_work_mem = '" <> ptMaintenanceWorkMem t <> "';"
  , "SET max_parallel_maintenance_workers = " <> show (ptMaxParallelMaintenance t) <> ";"
  , "SET synchronous_commit = " <> (if ptAsyncCommit t then "off" else "on") <> ";"
  ]
