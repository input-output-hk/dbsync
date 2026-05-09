{-# LANGUAGE OverloadedStrings #-}

-- | Run @CREATE INDEX CONCURRENTLY@ for every PK and unique
-- constraint declared on the supplied tables.
--
-- The DDL builder lives in 'DbSync.Db.Statement.Indexes'; this
-- module is the connection-side runner. @CONCURRENTLY@ requires
-- autocommit mode, so each statement goes through 'Sess.sql'
-- (raw, unprepared) rather than 'Sess.statement'.
module DbSync.Phase.PreparingForChainTip.Indexes
  ( createIndexes
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Schema.Types (TableDef)
import DbSync.Db.Statement.Indexes (tableIndexStatements)

-- | Issue @CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS@ for each
-- index declared on each table. Tables without a PK or unique
-- constraints contribute zero statements.
createIndexes :: Conn.Connection -> [TableDef] -> IO ()
createIndexes conn tables =
  for_ (concatMap tableIndexStatements tables) (runDdl conn)

runDdl :: Conn.Connection -> Text -> IO ()
runDdl conn ddl = do
  result <- Conn.use conn (Sess.script ddl)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "PreparingForChainTip.Indexes: " <> show e <> " for " <> ddl
