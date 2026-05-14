{-# LANGUAGE OverloadedStrings #-}

-- | Run @CREATE INDEX CONCURRENTLY@ for every PK and unique
-- constraint declared on the supplied tables.
--
-- DDL builders live in 'DbSync.Db.Statement.Indexes'; this module is
-- the connection-side runner. @CONCURRENTLY@ requires autocommit, so
-- each statement goes through 'Sess.sql' (raw, unprepared) rather
-- than 'Sess.statement'.
module DbSync.Phase.PreparingForChainTip.Indexes
  ( createIndexes
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Schema.Types (TableDef (..))
import DbSync.Db.Statement.Indexes (tableIndexStatements)
import DbSync.Trace.Timing (timedTrace_)
import DbSync.Trace.Types (AppTracer)

-- | One log line per index so an operator can see which build is
-- in flight; on mainnet-scale data each can take minutes.
createIndexes :: AppTracer -> Conn.Connection -> [TableDef] -> IO ()
createIndexes tracer conn tables =
  for_ tables $ \td ->
    for_ (zip [1 :: Int ..] (tableIndexStatements td)) $ \(i, ddl) ->
      timedTrace_ tracer "PreparingForChainTip"
        ("index " <> tdName td <> " #" <> show i)
        (runDdl conn ddl)

runDdl :: Conn.Connection -> Text -> IO ()
runDdl conn ddl = do
  result <- Conn.use conn (Sess.script ddl)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "PreparingForChainTip.Indexes: " <> show e <> " for " <> ddl
