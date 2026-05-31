-- | Shared helpers used by the per-extractor Follow resolvers.
--
-- The per-extractor files expose pairs of @Conn@-flavour and
-- @Buf@-flavour functions; the helpers here factor out the bits both
-- flavours share (statement execution, the SELECT-on-key /
-- allocate-on-miss dance, the per-block dedup cache, and the
-- not-yet-implemented stubs).
module DbSync.Phase.Following.Resolver.Internal
  ( -- * Statement execution
    runStmt

    -- * Per-block dedup cache
  , BlockDedupCache (..)
  , newBlockDedupCache
  , cacheInsert

    -- * Dedup helper
  , resolveDedupSimple

    -- * Stubs
  , todoResolve
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Schema.Ids
  ( AddressId
  , MultiAssetId
  , PoolHashId
  , SlotLeaderId
  , StakeAddressId
  )

-- ---------------------------------------------------------------------------
-- * Statement execution
-- ---------------------------------------------------------------------------

-- | Run a hasql statement and panic on failure. The Follow loop
-- wraps each block in its own transaction, so a panic here aborts
-- the block.
runStmt :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
runStmt conn p stmt = do
  result <- Conn.use conn (Sess.statement p stmt)
  case result of
    Right b -> pure b
    Left e  -> panic $ "Follow resolver session failed: " <> show e

-- ---------------------------------------------------------------------------
-- * Per-block dedup cache
-- ---------------------------------------------------------------------------

-- | Per-block in-process dedup cache used by the buffered resolver.
--
-- Shadows not-yet-flushed INSERTs: a second resolve of the same key
-- within the block finds the previously-allocated id without
-- consulting PG. Built fresh per block, discarded after COMMIT.
data BlockDedupCache = BlockDedupCache
  { bdcSlotLeader   :: !(IORef (Map ByteString SlotLeaderId))
  , bdcPoolHash     :: !(IORef (Map ByteString PoolHashId))
  , bdcStakeAddress :: !(IORef (Map ByteString StakeAddressId))
  , bdcMultiAsset   :: !(IORef (Map (ByteString, ByteString) MultiAssetId))
  , bdcAddress      :: !(IORef (Map ByteString AddressId))
  }

newBlockDedupCache :: IO BlockDedupCache
newBlockDedupCache = BlockDedupCache
  <$> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty

cacheInsert :: Ord k => IORef (Map k v) -> k -> v -> IO ()
cacheInsert ref k v = atomicModifyIORef' ref $ \m -> (Map.insert k v m, ())

-- ---------------------------------------------------------------------------
-- * Dedup helper
-- ---------------------------------------------------------------------------

-- | SELECT-on-key, allocate-on-miss with per-block cache shadowing.
-- Shared by 'resolveSlotLeader', 'resolveStakeAddress',
-- 'resolvePoolHash' in the buffered resolver.
resolveDedupSimple
  :: Ord key
  => Conn.Connection
  -> key
  -> IORef (Map key idType)
  -> Stmt.Statement key (Maybe idType)
  -> Stmt.Statement () idType
  -> IO (idType, Bool)
resolveDedupSimple conn key mapRef queryStmt nextStmt = do
  m <- readIORef mapRef
  case Map.lookup key m of
    Just i -> pure (i, False)
    Nothing -> do
      mId <- runStmt conn key queryStmt
      case mId of
        Just i -> do
          cacheInsert mapRef key i
          pure (i, False)
        Nothing -> do
          i <- runStmt conn () nextStmt
          cacheInsert mapRef key i
          pure (i, True)

-- ---------------------------------------------------------------------------
-- * Stubs
-- ---------------------------------------------------------------------------

-- | Stand-in for resolver fields whose extractors have not landed
-- yet. Returns an IO action that panics when forced — so record
-- construction succeeds for tests that never call the field.
todoResolve :: Text -> IO a
todoResolve name = pure $ panic $ "Phase.Following.Resolver." <> name <> " not yet implemented"
