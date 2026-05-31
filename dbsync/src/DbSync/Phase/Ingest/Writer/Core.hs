-- | COPY writers for tables owned by the @core@ extractor.
module DbSync.Phase.Ingest.Writer.Core
  ( writeBlockCopy
  , writeTxCopy
  , writeSlotLeaderCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Core
  ( Block
  , SlotLeader
  , Tx
  , blockTableDef
  , encodeBlockCopy
  , encodeSlotLeaderCopy
  , encodeTxCopy
  , slotLeaderTableDef
  , txTableDef
  )
import DbSync.Db.Schema.Ids (BlockId, SlotLeaderId, TxId)
import DbSync.Db.Schema.Types (TableDef (..))

writeBlockCopy :: LoaderStream -> BlockId -> Block -> IO ()
writeBlockCopy ls bid blk = lsWriteRow ls (tdName blockTableDef) (encodeBlockCopy bid blk)

writeTxCopy :: LoaderStream -> TxId -> Tx -> IO ()
writeTxCopy ls tid tx = lsWriteRow ls (tdName txTableDef) (encodeTxCopy tid tx)

writeSlotLeaderCopy :: LoaderStream -> SlotLeaderId -> SlotLeader -> IO ()
writeSlotLeaderCopy ls slid sl = lsWriteRow ls (tdName slotLeaderTableDef) (encodeSlotLeaderCopy slid sl)
