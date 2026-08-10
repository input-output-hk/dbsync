{-# LANGUAGE OverloadedStrings #-}

-- | Per-connection knobs for the post-load pass.
--
-- These use session-scoped @SET@, not @SET LOCAL@: each step in
-- 'Phase.Preparing.Run.run' runs in its own implicit transaction,
-- which @SET LOCAL@ would scope to. A session @SET@ lasts for the
-- connection's lifetime, and that matches the pass.
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

-- | Tuning applied at the start of the post-load pass. Operators on
-- other hardware override 'defaultPrepTuning' by record update.
data PrepTuning = PrepTuning
  { -- | Per-backend RAM cap for sort and index-build buffers. Larger
    -- values cut external-sort I/O on big B-tree builds.
    ptMaintenanceWorkMem     :: !Text
    -- | Upper bound on the parallel workers @CREATE INDEX@ and
    -- @VACUUM@ launch. The server's @max_parallel_workers@ caps this
    -- silently. A value above the core count wastes workers.
  , ptMaxParallelMaintenance :: !Int
    -- | @True@ sets @synchronous_commit = off@ for the Prep session.
    -- Prep is idempotent on crash — @sync_state@ stays
    -- not-complete until 'markSyncComplete' fires — so a lost commit
    -- only re-runs the pass.
  , ptAsyncCommit            :: !Bool
    -- | Backend count for the parallel Prep steps: the
    -- @ALTER … SET LOGGED@ flip and the @CREATE INDEX@ build.
  , ptPoolSize               :: !Int
  }
  deriving stock (Eq, Show)

-- | Defaults for the 4-core / 16 GB target box.
defaultPrepTuning :: PrepTuning
defaultPrepTuning = PrepTuning
  { ptMaintenanceWorkMem     = "2GB"
  , ptMaxParallelMaintenance = 3
  , ptAsyncCommit            = True
  , ptPoolSize               = 4
  }

-- | Both 'setPrepSessionGUCs' and the 'Hasql.Pool' @initSession@
-- hook run this, so every pool backend boots with the same tuning.
prepSessionGUCsSession :: PrepTuning -> Sess.Session ()
prepSessionGUCsSession t = Sess.script (gucSql t)

-- | Raises 'AppDatabaseError' on failure. These GUCs are always
-- valid, so a failure here points at the connection.
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
