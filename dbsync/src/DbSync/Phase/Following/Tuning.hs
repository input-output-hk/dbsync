{-# LANGUAGE OverloadedStrings #-}

-- | Per-connection knobs for the Follow phase.
--
-- @SET synchronous_commit = off@ is the headline tuning: every
-- forward block runs through one @BEGIN@/@COMMIT@ envelope that
-- writes its rows AND advances @last_committed_*@ atomically, so
-- the COMMIT-vs-fsync gap doesn't risk a torn write — either the
-- whole transaction is durable on crash or none of it is, and
-- chainsync replays anything that didn't make it to disk on the
-- next start.
--
-- Why @SET@, not @SET LOCAL@: Follow opens a long-lived per-phase
-- connection. Session-scoped @SET@ persists for the connection's
-- lifetime; @SET LOCAL@ would have to be re-issued per transaction.
module DbSync.Phase.Following.Tuning
  ( FollowTuning (..)
  , defaultFollowTuning
  , setFollowSessionGUCs
  ) where

import Cardano.Prelude

import qualified Hasql.Session as Sess

import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.Tuning (followGucSql)
import DbSync.Db.Transaction (HasHasqlConnection (..))

-- | Tuning applied when the Follow connection is opened.
data FollowTuning = FollowTuning
  { -- | @True@ → @synchronous_commit = off@. Trades a window of
    -- crash-recovery durability for faster per-block COMMITs. Safe
    -- because each per-block transaction is atomic in writes +
    -- sync-state.
    ftAsyncCommit :: !Bool
  }
  deriving stock (Eq, Show)

-- | Async-commit on. Mirrors Prep's default trade-off.
defaultFollowTuning :: FollowTuning
defaultFollowTuning = FollowTuning
  { ftAsyncCommit = True
  }

-- | Issue the @SET@ statements that bring the env's connection up
-- to the requested 'FollowTuning'. Surfaces driver failures as
-- 'AppDatabaseError' — these are unconditionally valid GUCs, so a
-- failure here points at a connection-level problem.
setFollowSessionGUCs
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => FollowTuning -> m ()
setFollowSessionGUCs t = do
  conn <- asks getHasqlConnection
  useConn "Phase.Following.Tuning" conn (Sess.script (gucSql t))

gucSql :: FollowTuning -> Text
gucSql t = followGucSql (ftAsyncCommit t)
