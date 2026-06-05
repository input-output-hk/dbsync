-- | hasql writers for tables owned by the @epoch_boundary@
-- extractor. All flavours panic via 'todoWrite' until the insert
-- statements are wired.
module DbSync.Phase.Following.Writer.EpochBoundary
  ( writeAdaPotsConn
  , writeAdaPotsBuf
  , writeEpochParamConn
  , writeEpochParamBuf
  , writeEpochStateConn
  , writeEpochStateBuf
  , writeCostModelConn
  , writeCostModelBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.AdaPots (AdaPots)
import DbSync.Db.Schema.EpochBoundary (CostModel, EpochParam, EpochState)
import DbSync.Db.Schema.Ids (CostModelId)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (todoWrite, todoWriteLeaf)

writeAdaPotsConn :: Conn.Connection -> AdaPots -> IO ()
writeAdaPotsConn _ = todoWriteLeaf "writeAdaPots"

writeAdaPotsBuf :: WriteBuffer -> AdaPots -> IO ()
writeAdaPotsBuf _ = todoWriteLeaf "writeAdaPots"

writeEpochParamConn :: Conn.Connection -> EpochParam -> IO ()
writeEpochParamConn _ = todoWriteLeaf "writeEpochParam"

writeEpochParamBuf :: WriteBuffer -> EpochParam -> IO ()
writeEpochParamBuf _ = todoWriteLeaf "writeEpochParam"

writeEpochStateConn :: Conn.Connection -> EpochState -> IO ()
writeEpochStateConn _ = todoWriteLeaf "writeEpochState"

writeEpochStateBuf :: WriteBuffer -> EpochState -> IO ()
writeEpochStateBuf _ = todoWriteLeaf "writeEpochState"

writeCostModelConn :: Conn.Connection -> CostModelId -> CostModel -> IO ()
writeCostModelConn _ = todoWrite "writeCostModel"

writeCostModelBuf :: WriteBuffer -> CostModelId -> CostModel -> IO ()
writeCostModelBuf _ = todoWrite "writeCostModel"
