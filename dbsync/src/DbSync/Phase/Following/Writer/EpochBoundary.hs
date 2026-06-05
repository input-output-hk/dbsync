-- | hasql writers for tables owned by the @epoch_boundary@ extractor.
--
-- @ada_pots@, @epoch_param@, @epoch_state@ are IDENTITY leaves;
-- @cost_model@ is counter-managed so the writer takes a
-- caller-allocated 'CostModelId'.
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
import DbSync.Db.Statement.AdaPots (insertAdaPotsRowStmt)
import DbSync.Db.Statement.CostModel (insertCostModelRowStmt)
import DbSync.Db.Statement.EpochParam (insertEpochParamRowStmt)
import DbSync.Db.Statement.EpochState (insertEpochStateRowStmt)
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeAdaPotsConn :: Conn.Connection -> AdaPots -> IO ()
writeAdaPotsConn conn pots = runConn conn pots insertAdaPotsRowStmt

writeAdaPotsBuf :: WriteBuffer -> AdaPots -> IO ()
writeAdaPotsBuf buf pots = queueBuf buf pots insertAdaPotsRowStmt

writeEpochParamConn :: Conn.Connection -> EpochParam -> IO ()
writeEpochParamConn conn ep = runConn conn ep insertEpochParamRowStmt

writeEpochParamBuf :: WriteBuffer -> EpochParam -> IO ()
writeEpochParamBuf buf ep = queueBuf buf ep insertEpochParamRowStmt

writeEpochStateConn :: Conn.Connection -> EpochState -> IO ()
writeEpochStateConn conn es = runConn conn es insertEpochStateRowStmt

writeEpochStateBuf :: WriteBuffer -> EpochState -> IO ()
writeEpochStateBuf buf es = queueBuf buf es insertEpochStateRowStmt

writeCostModelConn :: Conn.Connection -> CostModelId -> CostModel -> IO ()
writeCostModelConn conn cmId cm = runConn conn (cmId, cm) insertCostModelRowStmt

writeCostModelBuf :: WriteBuffer -> CostModelId -> CostModel -> IO ()
writeCostModelBuf buf cmId cm = queueBuf buf (cmId, cm) insertCostModelRowStmt
