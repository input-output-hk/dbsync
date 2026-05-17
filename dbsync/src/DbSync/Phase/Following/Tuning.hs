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

import qualified Data.Text as T
import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Transaction (HasHasqlConnection (..))

-- | Tuning applied when the Follow connection is opened.
data FollowTuning = FollowTuning
  { -- | @True@ → @synchronous_commit = off@. Trades a fraction of a
    -- second of crash-recovery durability for substantially faster
    -- per-block COMMITs. Safe because each per-block transaction is
    -- atomic in writes + sync-state.
    ftAsyncCommit :: !Bool
  }
  deriving stock (Eq, Show)

-- | Async-commit on. Mirrors Prep's default trade-off.
defaultFollowTuning :: FollowTuning
defaultFollowTuning = FollowTuning
  { ftAsyncCommit = True
  }

-- | Issue the @SET@ statements that bring the env's connection up
-- to the requested 'FollowTuning'. Panics on driver failure — these
-- are unconditionally valid GUCs and a failure here points at a
-- connection-level problem worth surfacing immediately.
setFollowSessionGUCs
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => FollowTuning -> m ()
setFollowSessionGUCs t = do
  conn <- asks getHasqlConnection
  result <- liftIO $ Conn.use conn (Sess.script (gucSql t))
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "Phase.Following.Tuning: " <> show e

gucSql :: FollowTuning -> Text
gucSql t = T.unlines
  [ "SET synchronous_commit = " <> (if ftAsyncCommit t then "off" else "on") <> ";"
  ]
