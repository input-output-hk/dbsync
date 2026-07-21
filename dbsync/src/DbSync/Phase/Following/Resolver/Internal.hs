-- | Shared helpers used by the per-extractor Follow resolvers.
--
-- The per-extractor files expose pairs of @Conn@-flavour and
-- @Buf@-flavour functions; the helpers here factor out the bits both
-- flavours share (statement execution, the SELECT-on-key /
-- allocate-on-miss dance, and the per-block dedup cache).
module DbSync.Phase.Following.Resolver.Internal
  ( -- * Statement execution
    runStmt

    -- * Per-block dedup cache
  , BlockDedupCache (..)
  , newBlockDedupCache
  , cacheInsert

    -- * Dedup helpers
  , resolveDedupSimple
  , resolveDedupWith
  ) where

import Cardano.Prelude

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map

import qualified Hasql.Connection as Conn
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt

import DbSync.Db.Run (useConn)
import DbSync.Db.Schema.Ids
  ( AddressId
  , CommitteeHashId
  , CostModelId
  , DatumId
  , DrepHashId
  , GovActionProposalId
  , MultiAssetId
  , PoolHashId
  , RedeemerDataId
  , ScriptId
  , SlotLeaderId
  , StakeAddressId
  , VotingAnchorId
  )
import DbSync.Phase.Ingest.UtxoStore (UtxoTxEntry)

-- ---------------------------------------------------------------------------
-- * Statement execution
-- ---------------------------------------------------------------------------

-- | Run a hasql statement; failures surface as 'AppDatabaseError'.
-- The Follow loop's per-block transaction envelope catches it and
-- rolls back.
runStmt :: Conn.Connection -> a -> Stmt.Statement a b -> IO b
runStmt conn p stmt = useConn "Phase.Following.Resolver" conn (Sess.statement p stmt)

-- ---------------------------------------------------------------------------
-- * Per-block dedup cache
-- ---------------------------------------------------------------------------

-- | Per-block in-process dedup cache used by the buffered resolver.
--
-- Shadows not-yet-flushed INSERTs: a second resolve of the same key
-- within the block finds the previously-allocated id without
-- consulting PG. Built fresh per block, discarded after COMMIT.
data BlockDedupCache = BlockDedupCache
  { bdcSlotLeader        :: !(IORef (Map ByteString SlotLeaderId))
  , bdcPoolHash          :: !(IORef (Map ByteString PoolHashId))
  , bdcStakeAddress      :: !(IORef (Map ByteString StakeAddressId))
  , bdcMultiAsset        :: !(IORef (Map (ByteString, ByteString) MultiAssetId))
  , bdcAddress           :: !(IORef (Map ByteString AddressId))
  , bdcCostModel         :: !(IORef (Map ByteString CostModelId))
  , bdcDatum             :: !(IORef (Map ByteString DatumId))
  , bdcScript            :: !(IORef (Map ByteString ScriptId))
  , bdcRedeemerData      :: !(IORef (Map ByteString RedeemerDataId))
  , bdcDrepHash          :: !(IORef (Map ByteString DrepHashId))
  , bdcCommitteeHash     :: !(IORef (Map ByteString CommitteeHashId))
  , bdcVotingAnchor      :: !(IORef (Map ByteString VotingAnchorId))
  , bdcGovActionProposal :: !(IORef (Map (ByteString, Word64) GovActionProposalId))
  , bdcUtxo              :: !(IORef (Map ByteString UtxoTxEntry))
    -- ^ Outputs of the block's own txs, keyed by tx hash. Lets a
    -- same-block spend resolve its producer while the tx_out INSERT
    -- is still sitting unflushed in the 'WriteBuffer'.
  }

newBlockDedupCache :: IO BlockDedupCache
newBlockDedupCache = BlockDedupCache
  <$> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty
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
resolveDedupSimple conn key mapRef queryStmt nextStmt =
  resolveDedupWith conn key key mapRef queryStmt nextStmt

-- | Like 'resolveDedupSimple' but with the cache key and the SELECT
-- parameter at different shapes. Used by governance dedup tables
-- whose cache key is an encoded ByteString (matching the Ingest
-- 'DedupStore' layout) but whose SELECT keys on a structured tuple
-- (e.g. @(raw, has_script)@ for @drep_hash@).
resolveDedupWith
  :: Ord cacheKey
  => Conn.Connection
  -> cacheKey
  -> selectKey
  -> IORef (Map cacheKey idType)
  -> Stmt.Statement selectKey (Maybe idType)
  -> Stmt.Statement () idType
  -> IO (idType, Bool)
resolveDedupWith conn cacheKey selectKey mapRef queryStmt nextStmt = do
  m <- readIORef mapRef
  case Map.lookup cacheKey m of
    Just i -> pure (i, False)
    Nothing -> do
      mId <- runStmt conn selectKey queryStmt
      case mId of
        Just i -> do
          cacheInsert mapRef cacheKey i
          pure (i, False)
        Nothing -> do
          i <- runStmt conn () nextStmt
          cacheInsert mapRef cacheKey i
          pure (i, True)
