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
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Phase.Following.WriteBuffer (WriteBuffer, append)

-- | Run an insert statement immediately against the connection.
-- Panics on PG failure — caller is the per-block transaction
-- envelope, so any error already implies we abort the block.
runConn :: Conn.Connection -> a -> Stmt.Statement a () -> IO ()
runConn conn p stmt = do
  result <- Conn.use conn (Sess.statement p stmt)
  case result of
    Right () -> pure ()
    Left  e  -> panic $ "Insert writer session failed: " <> show e

-- | Append an insert statement to the per-block pipeline buffer.
queueBuf :: WriteBuffer -> a -> Stmt.Statement a () -> IO ()
queueBuf buf p stmt = append buf (Pipeline.statement p stmt)

-- | Placeholder used by per-extractor writers whose insert plumbing
-- has not landed yet (e.g. epoch-boundary tables in Follow mode).
-- Replaces the inline @\_ _ -> todo "writeX"@ that the original
-- monolithic 'Writer' used.
todoWrite :: Text -> a -> b -> IO ()
todoWrite name _ _ = pure $ panic $ "Phase.Following.Writer." <> name <> " not yet implemented"
