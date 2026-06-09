-- | hasql writers for tables owned by the @pool@ extractor.
module DbSync.Phase.Following.Writer.Pool
  ( writePoolHashConn
  , writePoolHashBuf
  , writePoolUpdateConn
  , writePoolUpdateBuf
  , writePoolMetadataRefConn
  , writePoolMetadataRefBuf
  , writePoolOwnerConn
  , writePoolOwnerBuf
  , writePoolRetireConn
  , writePoolRetireBuf
  , writePoolRelayConn
  , writePoolRelayBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids
  ( PoolHashId
  , PoolMetadataRefId
  , PoolUpdateId
  )
import DbSync.Db.Schema.Pool
  ( PoolHash
  , PoolMetadataRef
  , PoolOwner
  , PoolRelay
  , PoolRetire
  , PoolUpdate
  )
import DbSync.Db.Statement.Pool (insertPoolHashRowStmt)
import DbSync.Db.Statement.Pool (insertPoolMetadataRefRowStmt)
import DbSync.Db.Statement.Pool (insertPoolOwnerRowStmt)
import DbSync.Db.Statement.Pool (insertPoolRelayRowStmt)
import DbSync.Db.Statement.Pool (insertPoolRetireRowStmt)
import DbSync.Db.Statement.Pool (insertPoolUpdateRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writePoolHashConn :: Conn.Connection -> PoolHashId -> PoolHash -> IO ()
writePoolHashConn conn pid ph = runConn conn (pid, ph) insertPoolHashRowStmt

writePoolHashBuf :: WriteBuffer -> PoolHashId -> PoolHash -> IO ()
writePoolHashBuf buf pid ph = queueBuf buf (pid, ph) insertPoolHashRowStmt

writePoolUpdateConn :: Conn.Connection -> PoolUpdateId -> PoolUpdate -> IO ()
writePoolUpdateConn conn pid pu = runConn conn (pid, pu) insertPoolUpdateRowStmt

writePoolUpdateBuf :: WriteBuffer -> PoolUpdateId -> PoolUpdate -> IO ()
writePoolUpdateBuf buf pid pu = queueBuf buf (pid, pu) insertPoolUpdateRowStmt

writePoolMetadataRefConn :: Conn.Connection -> PoolMetadataRefId -> PoolMetadataRef -> IO ()
writePoolMetadataRefConn conn pid pm = runConn conn (pid, pm) insertPoolMetadataRefRowStmt

writePoolMetadataRefBuf :: WriteBuffer -> PoolMetadataRefId -> PoolMetadataRef -> IO ()
writePoolMetadataRefBuf buf pid pm = queueBuf buf (pid, pm) insertPoolMetadataRefRowStmt

writePoolOwnerConn :: Conn.Connection -> PoolOwner -> IO ()
writePoolOwnerConn conn po = runConn conn po insertPoolOwnerRowStmt

writePoolOwnerBuf :: WriteBuffer -> PoolOwner -> IO ()
writePoolOwnerBuf buf po = queueBuf buf po insertPoolOwnerRowStmt

writePoolRetireConn :: Conn.Connection -> PoolRetire -> IO ()
writePoolRetireConn conn pr = runConn conn pr insertPoolRetireRowStmt

writePoolRetireBuf :: WriteBuffer -> PoolRetire -> IO ()
writePoolRetireBuf buf pr = queueBuf buf pr insertPoolRetireRowStmt

writePoolRelayConn :: Conn.Connection -> PoolRelay -> IO ()
writePoolRelayConn conn pr = runConn conn pr insertPoolRelayRowStmt

writePoolRelayBuf :: WriteBuffer -> PoolRelay -> IO ()
writePoolRelayBuf buf pr = queueBuf buf pr insertPoolRelayRowStmt
