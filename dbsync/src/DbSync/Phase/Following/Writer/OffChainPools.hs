-- | hasql writers for tables owned by the @off_chain_pools@ extractor.
module DbSync.Phase.Following.Writer.OffChainPools
  ( writeOffChainPoolDataConn
  , writeOffChainPoolDataBuf
  , writeOffChainPoolFetchErrorConn
  , writeOffChainPoolFetchErrorBuf
  , writeDelistedPoolConn
  , writeDelistedPoolBuf
  , writeReservedPoolTickerConn
  , writeReservedPoolTickerBuf
  ) where

import Cardano.Prelude

import qualified Hasql.Connection as Conn

import DbSync.Db.Schema.OffChainPool (OffChainPoolData, OffChainPoolFetchError)
import DbSync.Db.Schema.Pool (DelistedPool, ReservedPoolTicker)
import DbSync.Db.Statement.Worker.OffChainPool
  ( insertDelistedPoolRowStmt
  , insertOffChainPoolDataRowStmt
  , insertOffChainPoolFetchErrorRowStmt
  , insertReservedPoolTickerRowStmt
  )
import DbSync.Phase.Following.WriteBuffer (WriteBuffer)
import DbSync.Phase.Following.Writer.Internal (queueBuf, runConn)

writeOffChainPoolDataConn :: Conn.Connection -> OffChainPoolData -> IO ()
writeOffChainPoolDataConn conn d = runConn conn d insertOffChainPoolDataRowStmt

writeOffChainPoolDataBuf :: WriteBuffer -> OffChainPoolData -> IO ()
writeOffChainPoolDataBuf buf d = queueBuf buf d insertOffChainPoolDataRowStmt

writeOffChainPoolFetchErrorConn :: Conn.Connection -> OffChainPoolFetchError -> IO ()
writeOffChainPoolFetchErrorConn conn e = runConn conn e insertOffChainPoolFetchErrorRowStmt

writeOffChainPoolFetchErrorBuf :: WriteBuffer -> OffChainPoolFetchError -> IO ()
writeOffChainPoolFetchErrorBuf buf e = queueBuf buf e insertOffChainPoolFetchErrorRowStmt

writeDelistedPoolConn :: Conn.Connection -> DelistedPool -> IO ()
writeDelistedPoolConn conn dp = runConn conn dp insertDelistedPoolRowStmt

writeDelistedPoolBuf :: WriteBuffer -> DelistedPool -> IO ()
writeDelistedPoolBuf buf dp = queueBuf buf dp insertDelistedPoolRowStmt

writeReservedPoolTickerConn :: Conn.Connection -> ReservedPoolTicker -> IO ()
writeReservedPoolTickerConn conn rpt = runConn conn rpt insertReservedPoolTickerRowStmt

writeReservedPoolTickerBuf :: WriteBuffer -> ReservedPoolTicker -> IO ()
writeReservedPoolTickerBuf buf rpt = queueBuf buf rpt insertReservedPoolTickerRowStmt
