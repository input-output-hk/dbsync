{-# LANGUAGE OverloadedStrings #-}

-- | Build the minimum index set the post-load UPDATEs need. These
-- run while the tables are still UNLOGGED, and deliberately not
-- @CONCURRENTLY@: nothing else writes, so a one-pass build skips the
-- WAL writes and the second scan.
--
-- All of it is scaffolding.
-- 'DbSync.Phase.Preparing.Run.run' drops the whole set before the
-- UNLOGGED to LOGGED flip, and the production index pass rebuilds
-- the shapes Follow needs under the same names.
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

-- | Each index logs as its own step, so an operator chasing a slow
-- pass sees which build runs.
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
