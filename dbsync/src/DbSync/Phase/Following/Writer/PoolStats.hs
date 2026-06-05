-- | hasql writer for the table owned by the @pool_stats@ extractor.
module DbSync.Phase.Following.Writer.PoolStats
  ( writePoolStatConn
  , writePoolStatBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Pool (PoolStat)
import DbSync.Db.Statement.PoolStat (insertPoolStatRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writePoolStatConn :: Conn.Connection -> PoolStat -> IO ()
writePoolStatConn conn ps = runConn conn ps insertPoolStatRowStmt

writePoolStatBuf :: WriteBuffer -> PoolStat -> IO ()
writePoolStatBuf buf ps = queueBuf buf ps insertPoolStatRowStmt
