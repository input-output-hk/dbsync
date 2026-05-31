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
  , PoolOwnerId
  , PoolRelayId
  , PoolRetireId
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
import DbSync.Db.Statement.PoolHash (insertPoolHashRowStmt)
import DbSync.Db.Statement.PoolMetadataRef (insertPoolMetadataRefRowStmt)
import DbSync.Db.Statement.PoolOwner (insertPoolOwnerRowStmt)
import DbSync.Db.Statement.PoolRelay (insertPoolRelayRowStmt)
import DbSync.Db.Statement.PoolRetire (insertPoolRetireRowStmt)
import DbSync.Db.Statement.PoolUpdate (insertPoolUpdateRowStmt)
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

writePoolOwnerConn :: Conn.Connection -> PoolOwnerId -> PoolOwner -> IO ()
writePoolOwnerConn conn pid po = runConn conn (pid, po) insertPoolOwnerRowStmt

writePoolOwnerBuf :: WriteBuffer -> PoolOwnerId -> PoolOwner -> IO ()
writePoolOwnerBuf buf pid po = queueBuf buf (pid, po) insertPoolOwnerRowStmt

writePoolRetireConn :: Conn.Connection -> PoolRetireId -> PoolRetire -> IO ()
writePoolRetireConn conn pid pr = runConn conn (pid, pr) insertPoolRetireRowStmt

writePoolRetireBuf :: WriteBuffer -> PoolRetireId -> PoolRetire -> IO ()
writePoolRetireBuf buf pid pr = queueBuf buf (pid, pr) insertPoolRetireRowStmt

writePoolRelayConn :: Conn.Connection -> PoolRelayId -> PoolRelay -> IO ()
writePoolRelayConn conn pid pr = runConn conn (pid, pr) insertPoolRelayRowStmt

writePoolRelayBuf :: WriteBuffer -> PoolRelayId -> PoolRelay -> IO ()
writePoolRelayBuf buf pid pr = queueBuf buf (pid, pr) insertPoolRelayRowStmt
