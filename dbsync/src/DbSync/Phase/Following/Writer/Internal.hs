-- | Shared runners for the per-extractor Follow writers. Each write
-- function takes one of these as its first argument, and it is the
-- only difference between the @mkWriter@ and @mkBufferedWriter@
-- paths in "DbSync.Phase.Following.Writer".
module DbSync.Phase.Following.Writer.Internal
  ( runConn
  , queueBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Run (useConn)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer, append)

-- | Run the insert now. A failure raises 'AppDatabaseError', and the
-- per-block transaction envelope rolls the block back.
runConn :: Conn.Connection -> a -> Stmt.Statement a () -> IO ()
runConn conn p stmt = useConn "Phase.Following.Writer" conn (Sess.statement p stmt)

-- | Append the insert to the per-block pipeline buffer.
queueBuf :: WriteBuffer -> a -> Stmt.Statement a () -> IO ()
queueBuf buf p stmt = append buf (Pipeline.statement p stmt)
