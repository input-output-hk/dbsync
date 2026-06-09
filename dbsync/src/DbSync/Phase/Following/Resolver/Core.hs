-- | Follow 'IdResolver' fragments for the @core@ extractor.
module DbSync.Phase.Following.Resolver.Core
  ( -- * Shared between both flavours
    assignBlockIdFollow
  , resolvePrevBlockFollow

    -- * Direct flavour
  , assignTxIdConn
  , resolveSlotLeaderConn

    -- * Buffered flavour
  , assignTxIdBuf
  , resolveSlotLeaderBuf
  ) where

import Cardano.Prelude

import Data.IORef (IORef, readIORef, writeIORef)

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Core (SlotLeader)
import DbSync.Db.Schema.Ids (BlockId, SlotLeaderId, TxId)
import DbSync.Db.Statement.Core (nextBlockIdStmt)
import DbSync.Db.Statement.Core (nextSlotLeaderIdStmt, querySlotLeaderIdStmt)
import DbSync.Db.Statement.Core (nextTxIdStmt)
import DbSync.Phase.Following.IdAllocator (PreAllocatedIds (..), popHead)
import DbSync.Phase.Following.Resolver.Internal
  ( BlockDedupCache (..)
  , resolveDedupSimple
  , runStmt
  )

-- | Allocate the next 'BlockId' via the sequence and stash it so a
-- subsequent 'resolvePrevBlockFollow' can answer locally. Same in
-- both flavours: block IDs must materialise synchronously because
-- 'resolvePrevBlock' needs the value.
assignBlockIdFollow :: Conn.Connection -> IORef (Maybe BlockId) -> IO BlockId
assignBlockIdFollow conn lastBlock = do
  bid <- runStmt conn () nextBlockIdStmt
  writeIORef lastBlock (Just bid)
  pure bid

-- | Read the most-recently-assigned 'BlockId'. The hash argument is
-- ignored: in Follow the in-process @lastBlock@ ref is authoritative
-- between block writes.
resolvePrevBlockFollow :: IORef (Maybe BlockId) -> ByteString -> IO (Maybe BlockId)
resolvePrevBlockFollow lastBlock _ = readIORef lastBlock

-- ---------------------------------------------------------------------------
-- * Direct flavour
-- ---------------------------------------------------------------------------

assignTxIdConn :: Conn.Connection -> IO TxId
assignTxIdConn conn = runStmt conn () nextTxIdStmt

resolveSlotLeaderConn :: Conn.Connection -> ByteString -> SlotLeader -> IO (SlotLeaderId, Bool)
resolveSlotLeaderConn conn hash _leader = do
  mId <- runStmt conn hash querySlotLeaderIdStmt
  case mId of
    Just sid -> pure (sid, False)
    Nothing  -> do
      sid <- runStmt conn () nextSlotLeaderIdStmt
      pure (sid, True)

-- ---------------------------------------------------------------------------
-- * Buffered flavour
-- ---------------------------------------------------------------------------

assignTxIdBuf :: PreAllocatedIds -> IO TxId
assignTxIdBuf preAlloc = popHead "assignTxId" (paiTxIds preAlloc)

resolveSlotLeaderBuf
  :: Conn.Connection -> BlockDedupCache -> ByteString -> SlotLeader -> IO (SlotLeaderId, Bool)
resolveSlotLeaderBuf conn cache hash _leader =
  resolveDedupSimple
    conn
    hash
    (bdcSlotLeader cache)
    querySlotLeaderIdStmt
    nextSlotLeaderIdStmt
