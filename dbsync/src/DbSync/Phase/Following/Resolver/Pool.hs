-- | Follow 'IdResolver' fragments for the @pool@ extractor.
module DbSync.Phase.Following.Resolver.Pool
  ( -- * Direct flavour
    resolvePoolHashConn
  , assignPoolUpdateIdConn
  , assignPoolMetadataRefIdConn
  , assignPoolOwnerIdConn
  , assignPoolRetireIdConn
  , assignPoolRelayIdConn

    -- * Buffered flavour
  , resolvePoolHashBuf
  , assignPoolUpdateIdBuf
  , assignPoolMetadataRefIdBuf
  , assignPoolOwnerIdBuf
  , assignPoolRetireIdBuf
  , assignPoolRelayIdBuf
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
import DbSync.Db.Schema.Pool (PoolHash)
import DbSync.Db.Statement.PoolHash (nextPoolHashIdStmt, queryPoolHashIdStmt)
import DbSync.Db.Statement.PoolMetadataRef (nextPoolMetadataRefIdStmt)
import DbSync.Db.Statement.PoolOwner (nextPoolOwnerIdStmt)
import DbSync.Db.Statement.PoolRelay (nextPoolRelayIdStmt)
import DbSync.Db.Statement.PoolRetire (nextPoolRetireIdStmt)
import DbSync.Db.Statement.PoolUpdate (nextPoolUpdateIdStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , resolveDedupSimple
  , runStmt
  )

-- ---------------------------------------------------------------------------
-- * Direct flavour
-- ---------------------------------------------------------------------------

resolvePoolHashConn :: Conn.Connection -> ByteString -> PoolHash -> IO (PoolHashId, Bool)
resolvePoolHashConn conn hash _ph = do
  mId <- runStmt conn hash queryPoolHashIdStmt
  case mId of
    Just phId -> pure (phId, False)
    Nothing   -> do
      phId <- runStmt conn () nextPoolHashIdStmt
      pure (phId, True)

assignPoolUpdateIdConn :: Conn.Connection -> IO PoolUpdateId
assignPoolUpdateIdConn conn = runStmt conn () nextPoolUpdateIdStmt

assignPoolMetadataRefIdConn :: Conn.Connection -> IO PoolMetadataRefId
assignPoolMetadataRefIdConn conn = runStmt conn () nextPoolMetadataRefIdStmt

assignPoolOwnerIdConn :: Conn.Connection -> IO PoolOwnerId
assignPoolOwnerIdConn conn = runStmt conn () nextPoolOwnerIdStmt

assignPoolRetireIdConn :: Conn.Connection -> IO PoolRetireId
assignPoolRetireIdConn conn = runStmt conn () nextPoolRetireIdStmt

assignPoolRelayIdConn :: Conn.Connection -> IO PoolRelayId
assignPoolRelayIdConn conn = runStmt conn () nextPoolRelayIdStmt

-- ---------------------------------------------------------------------------
-- * Buffered flavour
-- ---------------------------------------------------------------------------

resolvePoolHashBuf
  :: Conn.Connection -> BlockDedupCache -> ByteString -> PoolHash -> IO (PoolHashId, Bool)
resolvePoolHashBuf conn cache hash _ph =
  resolveDedupSimple
    conn
    hash
    (bdcPoolHash cache)
    queryPoolHashIdStmt
    nextPoolHashIdStmt

assignPoolUpdateIdBuf :: PreAllocatedIds -> IO PoolUpdateId
assignPoolUpdateIdBuf preAlloc = popHead "assignPoolUpdateId" (paiPoolUpdateIds preAlloc)

assignPoolMetadataRefIdBuf :: PreAllocatedIds -> IO PoolMetadataRefId
assignPoolMetadataRefIdBuf preAlloc = popHead "assignPoolMetadataRefId" (paiPoolMetadataRefIds preAlloc)

assignPoolOwnerIdBuf :: PreAllocatedIds -> IO PoolOwnerId
assignPoolOwnerIdBuf preAlloc = popHead "assignPoolOwnerId" (paiPoolOwnerIds preAlloc)

assignPoolRetireIdBuf :: PreAllocatedIds -> IO PoolRetireId
assignPoolRetireIdBuf preAlloc = popHead "assignPoolRetireId" (paiPoolRetireIds preAlloc)

assignPoolRelayIdBuf :: PreAllocatedIds -> IO PoolRelayId
assignPoolRelayIdBuf preAlloc = popHead "assignPoolRelayId" (paiPoolRelayIds preAlloc)
