-- | Follow 'IdResolver' fragments for the @pool@ extractor.
module DbSync.Phase.Following.Resolver.Pool
  ( -- * Direct flavour
    resolvePoolHashConn
  , resolvePoolHashQueryConn
  , assignPoolUpdateIdConn
  , assignPoolMetadataRefIdConn

    -- * Buffered flavour
  , resolvePoolHashBuf
  , resolvePoolHashQueryBuf
  , assignPoolUpdateIdBuf
  , assignPoolMetadataRefIdBuf
  ) where

import Cardano.Prelude

import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Ids (PoolHashId, PoolMetadataRefId, PoolUpdateId)
import DbSync.Db.Schema.Core (PoolHash)
import DbSync.Db.Statement.Core (nextPoolHashIdStmt, queryPoolHashIdStmt)
import DbSync.Db.Statement.Pool (nextPoolMetadataRefIdStmt)
import DbSync.Db.Statement.Pool (nextPoolUpdateIdStmt)
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

-- | Look up a registered pool hash without inserting; 'Nothing' for a
-- genesis-key slot leader.
resolvePoolHashQueryConn :: Conn.Connection -> ByteString -> IO (Maybe PoolHashId)
resolvePoolHashQueryConn conn hash = runStmt conn hash queryPoolHashIdStmt

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

-- | Look up a registered pool hash without inserting, checking the per-block
-- cache before the database; 'Nothing' for a genesis-key slot leader.
resolvePoolHashQueryBuf
  :: Conn.Connection -> BlockDedupCache -> ByteString -> IO (Maybe PoolHashId)
resolvePoolHashQueryBuf conn cache hash = do
  cached <- Map.lookup hash <$> readIORef (bdcPoolHash cache)
  case cached of
    Just phId -> pure (Just phId)
    Nothing   -> runStmt conn hash queryPoolHashIdStmt

assignPoolUpdateIdBuf :: PreAllocatedIds -> IO PoolUpdateId
assignPoolUpdateIdBuf preAlloc = popHead "assignPoolUpdateId" (paiPoolUpdateIds preAlloc)

assignPoolMetadataRefIdBuf :: PreAllocatedIds -> IO PoolMetadataRefId
assignPoolMetadataRefIdBuf preAlloc = popHead "assignPoolMetadataRefId" (paiPoolMetadataRefIds preAlloc)
