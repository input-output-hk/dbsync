-- | Reset every @<table>_id_seq@ to @MAX(id) + 1@ so that
-- 'FollowingChainTip' can allocate IDs from the sequence rather
-- than from in-process counters.
--
-- All setval statements ship in one libpq pipeline so the pass
-- costs a single round-trip rather than one per UNLOGGED table.
-- The SQL builder lives in 'DbSync.Db.Statement.Sequences'.
module DbSync.Phase.Preparing.Sequences
  ( resetSequences
  ) where

import Cardano.Prelude

import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess

import DbSync.Db.Run (useConn)
import DbSync.Db.Schema.Types (TableDef (..), TableMode (..))
import DbSync.Db.Statement.Sequences (resetSequenceStmt)
import DbSync.Db.Transaction (HasHasqlConnection (..))

-- | Run @setval@ on every UNLOGGED table's @id@ sequence so that
-- 'FollowingChainTip' allocates ids past the rows Ingest already
-- loaded. Tables that were already LOGGED at schema creation (e.g.
-- @dbsync_sync_state@) manage their own ids and are skipped.
-- 'DbSync.Db.Statement.Sequences.resetSequenceSql' resolves the
-- sequence via @pg_get_serial_sequence@, so explicit and
-- IDENTITY-backed sequences are handled uniformly.
resetSequences
  :: (HasHasqlConnection env, MonadReader env m, MonadIO m)
  => [TableDef] -> m ()
resetSequences tables = do
  conn <- asks getHasqlConnection
  useConn "Phase.Preparing.Sequences" conn $
    Sess.pipeline $
      traverse_ pipelineSetval (filter ((== TableUnlogged) . tdMode) tables)
  where
    pipelineSetval td =
      void (Pipeline.statement () (resetSequenceStmt (tdName td)))
