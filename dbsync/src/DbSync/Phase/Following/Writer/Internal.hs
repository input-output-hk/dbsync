-- | Shared runners used by the per-extractor Follow writers.
--
-- Two flavours:
--
-- * 'runConn'  — run the insert immediately against the connection
--                ("DbSync.Phase.Following.Writer.mkWriter" path).
-- * 'queueBuf' — append the insert to a per-block pipeline buffer
--                ("DbSync.Phase.Following.Writer.mkBufferedWriter" path).
--
-- Per-extractor write functions take one of these as their first
-- argument; this is the only thing that varies between the two flavours.
module DbSync.Phase.Following.Writer.Internal
  ( runConn
  , queueBuf
  , todoWrite
  , todoWriteLeaf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Run (useConn)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer, append)

-- | Run an insert statement immediately against the connection.
-- Failures surface as 'AppDatabaseError'; the per-block transaction
-- envelope catches it and rolls back the block.
runConn :: Conn.Connection -> a -> Stmt.Statement a () -> IO ()
runConn conn p stmt = useConn "Phase.Following.Writer" conn (Sess.statement p stmt)

-- | Append an insert statement to the per-block pipeline buffer.
queueBuf :: WriteBuffer -> a -> Stmt.Statement a () -> IO ()
queueBuf buf p stmt = append buf (Pipeline.statement p stmt)

-- | Panicking stub used by writer records whose Follow-phase insert
-- plumbing is not yet wired. Construction succeeds; calling the
-- field panics with the writer's name.
todoWrite :: Text -> a -> b -> IO ()
todoWrite name _ _ = pure $ panic $ "Phase.Following.Writer." <> name <> " not yet implemented"

-- | Stub variant for leaf-table writers — the @id@ comes from
-- PostgreSQL so the writer field takes just the typed row.
todoWriteLeaf :: Text -> a -> IO ()
todoWriteLeaf name _ = pure $ panic $ "Phase.Following.Writer." <> name <> " not yet implemented"
