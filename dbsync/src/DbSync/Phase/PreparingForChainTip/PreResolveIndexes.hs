{-# LANGUAGE OverloadedStrings #-}

-- | Build the minimum index set the post-load UPDATEs need.
--
-- Runs at the start of 'DbSync.Phase.PreparingForChainTip.run', while
-- tables are still UNLOGGED. Building indexes here — before the four
-- resolves and the CTE-backed backfills — lets those UPDATEs use
-- index lookups instead of hash-joining multi-GB heaps in their
-- entirety.
--
-- Non-@CONCURRENTLY@ on purpose: on an UNLOGGED table with no
-- concurrent writers, a one-pass build avoids both the WAL writes
-- and the second-pass scan that @CONCURRENTLY@ would otherwise
-- force. The full post-flip 'DbSync.Phase.PreparingForChainTip.Indexes'
-- pass still runs later and uses @CREATE INDEX CONCURRENTLY IF NOT
-- EXISTS@, which dedupes against whatever this module already built.
module DbSync.Phase.PreparingForChainTip.PreResolveIndexes
  ( createPreResolveIndexes
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Statement.Indexes (preResolveIndexStatements)

-- | Issue the pre-resolve DDL against the supplied connection.
-- Each statement is a single @CREATE [UNIQUE] INDEX IF NOT EXISTS@
-- run via 'Sess.script' (raw, unprepared) so it can take advantage
-- of the autocommit transaction context the rest of the post-load
-- pass uses.
createPreResolveIndexes :: Conn.Connection -> IO ()
createPreResolveIndexes conn =
  for_ preResolveIndexStatements (runDdl conn)

runDdl :: Conn.Connection -> Text -> IO ()
runDdl conn ddl = do
  result <- Conn.use conn (Sess.script ddl)
  case result of
    Right () -> pure ()
    Left  e  -> panic $
      "PreparingForChainTip.PreResolveIndexes: " <> show e <> " for " <> ddl
