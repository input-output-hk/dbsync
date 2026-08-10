{-# LANGUAGE OverloadedStrings #-}

-- | Per-connection knobs for the Follow phase.
--
-- These use session-scoped @SET@, not @SET LOCAL@: Follow holds one
-- long-lived connection, and @SET LOCAL@ would need a re-issue per
-- transaction.
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

-- | Tuning applied when Follow opens its connection.
data FollowTuning = FollowTuning
  { -- | @True@ sets @synchronous_commit = off@, which trades
    -- crash-recovery durability for faster per-block COMMITs. One
    -- transaction writes the block's rows and advances
    -- @last_committed_*@ together, so a crash loses the whole
    -- transaction or none of it, and chainsync replays the rest.
    ftAsyncCommit :: !Bool
  }
  deriving stock (Eq, Show)

defaultFollowTuning :: FollowTuning
defaultFollowTuning = FollowTuning
  { ftAsyncCommit = True
  }

-- | Raises 'AppDatabaseError' on failure. These GUCs are always
-- valid, so a failure here points at the connection.
setFollowSessionGUCs
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => FollowTuning -> m ()
setFollowSessionGUCs t = do
  conn <- asks getHasqlConnection
  useConn "Phase.Following.Tuning" conn (Sess.script (gucSql t))

gucSql :: FollowTuning -> Text
gucSql t = followGucSql (ftAsyncCommit t)
