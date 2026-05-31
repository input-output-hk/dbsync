-- | hasql writers for tables owned by the @core@ extractor.
--
-- Each table gets a 'Conn' variant (immediate INSERT) and a 'Buf'
-- variant (append to a per-block pipeline buffer).
module DbSync.Phase.Following.Writer.Core
  ( writeBlockConn
  , writeBlockBuf
  , writeTxConn
  , writeTxBuf
  , writeSlotLeaderConn
  , writeSlotLeaderBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.Core (Block, SlotLeader, Tx)
import DbSync.Db.Schema.Ids (BlockId, SlotLeaderId, TxId)
import DbSync.Db.Statement.Block (insertBlockRowStmt)
import DbSync.Db.Statement.SlotLeader (insertSlotLeaderRowStmt)
import DbSync.Db.Statement.Tx (insertTxRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeBlockConn :: Conn.Connection -> BlockId -> Block -> IO ()
writeBlockConn conn bid blk = runConn conn (bid, blk) insertBlockRowStmt

writeBlockBuf :: WriteBuffer -> BlockId -> Block -> IO ()
writeBlockBuf buf bid blk = queueBuf buf (bid, blk) insertBlockRowStmt

writeTxConn :: Conn.Connection -> TxId -> Tx -> IO ()
writeTxConn conn tid tx = runConn conn (tid, tx) insertTxRowStmt

writeTxBuf :: WriteBuffer -> TxId -> Tx -> IO ()
writeTxBuf buf tid tx = queueBuf buf (tid, tx) insertTxRowStmt

writeSlotLeaderConn :: Conn.Connection -> SlotLeaderId -> SlotLeader -> IO ()
writeSlotLeaderConn conn slid sl = runConn conn (slid, sl) insertSlotLeaderRowStmt

writeSlotLeaderBuf :: WriteBuffer -> SlotLeaderId -> SlotLeader -> IO ()
writeSlotLeaderBuf buf slid sl = queueBuf buf (slid, sl) insertSlotLeaderRowStmt
