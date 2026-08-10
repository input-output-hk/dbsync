-- | Reset every @\<table\>_id_seq@ to @MAX(id) + 1@, so
-- 'FollowingChainTip' can allocate ids from the sequence instead of
-- from in-process counters.
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

-- | Skips tables that schema creation made LOGGED, such as
-- @dbsync_sync_state@: those manage their own ids. All the @setval@
-- statements ship in one pipeline, so the pass costs one round-trip.
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
