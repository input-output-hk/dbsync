{-# LANGUAGE OverloadedStrings #-}

-- | Build the minimum index set the post-load UPDATEs need.
--
-- Runs at the start of 'DbSync.Phase.Preparing.Run.run' while
-- tables are still UNLOGGED. Non-@CONCURRENTLY@ on purpose: a
-- one-pass build avoids the WAL writes and second-pass scan that
-- @CONCURRENTLY@ would force on an UNLOGGED table with no concurrent
-- writers.
--
-- Everything built here is scaffolding for the resolve + backfill
-- UPDATEs: 'DbSync.Phase.Preparing.Run.run' drops the whole set
-- again before the UNLOGGED → LOGGED flip so the flip rewrites bare
-- heaps. The production index pass after the flip rebuilds the
-- shapes Follow needs under the same names.
module DbSync.Phase.Preparing.PreResolveIndexes
  ( createPreResolveIndexes
  , createPostResolveIndexes
  ) where

import Cardano.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Hasql.Session as Sess

import DbSync.Db.Run (useConn)
import DbSync.Db.Statement.Indexes
  ( IndexStatement (..)
  , postResolveIndexStatements
  , preResolveIndexStatements
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Phase.Preparing.Step (StepKind (..), step)
import DbSync.Trace (HasTracer (..))

-- | Issue the pre-resolve DDL. Each index is logged as its own step
-- so an operator chasing a slow pass sees which build is in flight.
createPreResolveIndexes
  :: (HasTracer env, HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => m ()
createPreResolveIndexes = runStatements preResolveIndexStatements

-- | Indexes on the input tables that the CTAS resolve replaces. Built
-- after the CTAS so they survive the @DROP TABLE@.
createPostResolveIndexes
  :: (HasTracer env, HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => m ()
createPostResolveIndexes = runStatements postResolveIndexStatements

runStatements
  :: (HasTracer env, HasHasqlConnection env, MonadReader env m, MonadUnliftIO m)
  => [IndexStatement] -> m ()
runStatements stmts =
  for_ stmts $ \ix ->
    step IndexStep (isName ix) (runDdl (isSql ix))

runDdl
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Text -> m ()
runDdl ddl = do
  conn <- asks getHasqlConnection
  useConn "Phase.Preparing.PreResolveIndexes" conn (Sess.script ddl)
