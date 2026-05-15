{-# LANGUAGE OverloadedStrings #-}

-- | Reset every @<table>_id_seq@ to @MAX(id) + 1@ so that
-- 'FollowingChainTip' can allocate IDs from the sequence rather
-- than from in-process counters.
--
-- All setval statements ship in one libpq pipeline so the pass
-- costs a single round-trip rather than one per UNLOGGED table.
-- The SQL builder lives in 'DbSync.Db.Statement.Sequences'.
module DbSync.Phase.PreparingForChainTip.Sequences
  ( resetSequences
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Types (TableDef (..), TableMode (..))
import DbSync.Db.Statement.Sequences (resetSequenceSql)

-- | Run @setval@ on every @<table>_id_seq@ owned by an UNLOGGED
-- table. Tables that were already LOGGED at schema creation (e.g.
-- @dbsync_sync_state@) manage their own IDs and are skipped.
resetSequences :: Conn.Connection -> [TableDef] -> IO ()
resetSequences conn tables = do
  result <- Conn.use conn $ Sess.pipeline $
    traverse_ pipelineSetval (filter ((== TableUnlogged) . tdMode) tables)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "PreparingForChainTip.Sequences: " <> show e
  where
    pipelineSetval td = void (Pipeline.statement () (setvalStmt td))

    -- setval(…) is a SELECT, not a command — drain the returned row
    -- and discard the new sequence value.
    setvalStmt td =
      Stmt.unpreparable
        (resetSequenceSql (tdName td))
        E.noParams
        (D.singleRow (D.column (D.nonNullable D.int8)))
