{-# LANGUAGE OverloadedStrings #-}

-- | Build the minimum index set the post-load UPDATEs need.
--
-- Runs at the start of 'DbSync.Phase.Preparing.Run.run' while
-- tables are still UNLOGGED. Non-@CONCURRENTLY@ on purpose: a
-- one-pass build avoids the WAL writes and second-pass scan that
-- @CONCURRENTLY@ would force on an UNLOGGED table with no concurrent
-- writers. The full schema-driven index pass later uses
-- @CREATE INDEX CONCURRENTLY IF NOT EXISTS@, which dedupes against
-- anything this module already built.
module DbSync.Phase.Preparing.PreResolveIndexes
  ( createPreResolveIndexes
  , createPostResolveIndexes
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.AppM (LoggingM)
import DbSync.Db.Statement.Indexes
  ( postResolveIndexStatements
  , preResolveIndexStatements
  )
import DbSync.Db.Transaction (HasHasqlConnection (..))
import DbSync.Trace.Timing (timedTrace_)

-- | Issue the pre-resolve DDL. Each statement is logged separately
-- so an operator chasing a slow pass sees which index is building.
createPreResolveIndexes
  :: (LoggingM env m, HasHasqlConnection env)
  => m ()
createPreResolveIndexes =
  runStatements "pre-resolve index" preResolveIndexStatements

-- | Indexes on the input tables that the CTAS resolve replaces. Built
-- after the CTAS so they survive the @DROP TABLE@.
createPostResolveIndexes
  :: (LoggingM env m, HasHasqlConnection env)
  => m ()
createPostResolveIndexes =
  runStatements "post-resolve index" postResolveIndexStatements

runStatements
  :: (LoggingM env m, HasHasqlConnection env)
  => Text -> [Text] -> m ()
runStatements label stmts =
  for_ (zip [1 :: Int ..] stmts) $ \(i, ddl) ->
    timedTrace_ "PreparingForVolatileTail"
      (label <> " " <> show i)
      (runDdl ddl)

runDdl
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => Text -> m ()
runDdl ddl = do
  conn <- asks getHasqlConnection
  result <- liftIO $ Conn.use conn (Sess.script ddl)
  case result of
    Right () -> pure ()
    Left  e  -> panic $
      "Phase.Preparing.PreResolveIndexes: " <> show e <> " for " <> ddl
