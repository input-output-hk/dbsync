-- | Follow 'IdResolver' fragments for the @pool@ extractor.
module DbSync.Phase.Following.Resolver.Pool
  ( -- * Direct flavour
    resolvePoolHashConn
  , assignPoolUpdateIdConn
  , assignPoolMetadataRefIdConn

    -- * Buffered flavour
  , resolvePoolHashBuf
  , assignPoolUpdateIdBuf
  , assignPoolMetadataRefIdBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids (PoolHashId, PoolMetadataRefId, PoolUpdateId)
import DbSync.Db.Schema.Pool (PoolHash)
import DbSync.Db.Statement.PoolHash (nextPoolHashIdStmt, queryPoolHashIdStmt)
import DbSync.Db.Statement.PoolMetadataRef (nextPoolMetadataRefIdStmt)
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
