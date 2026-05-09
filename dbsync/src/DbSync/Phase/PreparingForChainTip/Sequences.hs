{-# LANGUAGE OverloadedStrings #-}

-- | Reset every @<table>_id_seq@ to @MAX(id) + 1@ so that
-- 'FollowingChainTip' can allocate IDs from the sequence rather
-- than from in-process counters.
--
-- The DDL builder lives in 'DbSync.Db.Statement.Sequences'; this
-- module is the connection-side runner.
module DbSync.Phase.PreparingForChainTip.Sequences
  ( resetSequences
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess

import DbSync.Db.Schema.Types (TableDef (..), TableMode (..))
import DbSync.Db.Statement.Sequences (resetSequenceSql)

-- | Run @setval@ on every @<table>_id_seq@ owned by an UNLOGGED
-- table. Tables that were already LOGGED at schema creation (e.g.
-- @dbsync_sync_state@) manage their own IDs and are skipped.
resetSequences :: Conn.Connection -> [TableDef] -> IO ()
resetSequences conn tables =
  for_ (filter ((== TableUnlogged) . tdMode) tables) $ \td ->
    runDdl conn (resetSequenceSql (tdName td))

runDdl :: Conn.Connection -> Text -> IO ()
runDdl conn ddl = do
  result <- Conn.use conn (Sess.script ddl)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "PreparingForChainTip.Sequences: " <> show e <> " for " <> ddl
