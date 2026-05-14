{-# LANGUAGE OverloadedStrings #-}

-- | Build the minimum index set the post-load UPDATEs need.
--
-- Runs at the start of 'DbSync.Phase.PreparingForChainTip.run' while
-- tables are still UNLOGGED. Non-@CONCURRENTLY@ on purpose: a
-- one-pass build avoids the WAL writes and second-pass scan that
-- @CONCURRENTLY@ would force on an UNLOGGED table with no concurrent
-- writers. The full schema-driven index pass later uses
-- @CREATE INDEX CONCURRENTLY IF NOT EXISTS@, which dedupes against
-- anything this module already built.
module DbSync.Phase.PreparingForChainTip.PreResolveIndexes
  ( createPreResolveIndexes
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Statement.Indexes (preResolveIndexStatements)
import DbSync.Trace.Timing (timedTrace_)
import DbSync.Trace.Types (AppTracer)

-- | Issue the pre-resolve DDL. Each statement is logged separately
-- so an operator chasing a slow pass sees which index is building.
createPreResolveIndexes :: AppTracer -> Conn.Connection -> IO ()
createPreResolveIndexes tracer conn =
  for_ (zip [1 :: Int ..] preResolveIndexStatements) $ \(i, ddl) ->
    timedTrace_ tracer "PreparingForChainTip"
      ("pre-resolve index " <> show i)
      (runDdl conn ddl)

runDdl :: Conn.Connection -> Text -> IO ()
runDdl conn ddl = do
  result <- Conn.use conn (Sess.script ddl)
  case result of
    Right () -> pure ()
    Left  e  -> panic $
      "PreparingForChainTip.PreResolveIndexes: " <> show e <> " for " <> ddl
