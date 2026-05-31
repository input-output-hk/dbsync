-- | hasql writers for tables owned by the @epoch_boundary@ extractor.
--
-- Follow-phase insert plumbing not landed yet for any of these
-- tables; all flavours fall through to 'todoWrite' for now.
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
import DbSync.Db.Schema.Ids (AdaPotsId, CostModelId, EpochParamId, EpochStateId)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (todoWrite)

writeAdaPotsConn :: Conn.Connection -> AdaPotsId -> AdaPots -> IO ()
writeAdaPotsConn _ = todoWrite "writeAdaPots"

writeAdaPotsBuf :: WriteBuffer -> AdaPotsId -> AdaPots -> IO ()
writeAdaPotsBuf _ = todoWrite "writeAdaPots"

writeEpochParamConn :: Conn.Connection -> EpochParamId -> EpochParam -> IO ()
writeEpochParamConn _ = todoWrite "writeEpochParam"

writeEpochParamBuf :: WriteBuffer -> EpochParamId -> EpochParam -> IO ()
writeEpochParamBuf _ = todoWrite "writeEpochParam"

writeEpochStateConn :: Conn.Connection -> EpochStateId -> EpochState -> IO ()
writeEpochStateConn _ = todoWrite "writeEpochState"

writeEpochStateBuf :: WriteBuffer -> EpochStateId -> EpochState -> IO ()
writeEpochStateBuf _ = todoWrite "writeEpochState"

writeCostModelConn :: Conn.Connection -> CostModelId -> CostModel -> IO ()
writeCostModelConn _ = todoWrite "writeCostModel"

writeCostModelBuf :: WriteBuffer -> CostModelId -> CostModel -> IO ()
writeCostModelBuf _ = todoWrite "writeCostModel"
