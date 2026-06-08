-- | COPY writers for tables owned by the @off_chain_pools@ extractor.
module DbSync.Phase.Ingest.Writer.OffChainPools
  ( writeOffChainPoolDataCopy
  , writeOffChainPoolFetchErrorCopy
  , writeDelistedPoolCopy
  , writeReservedPoolTickerCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.OffChainPool
  ( OffChainPoolData
  , OffChainPoolFetchError
  , encodeOffChainPoolDataCopy
  , encodeOffChainPoolFetchErrorCopy
  , offChainPoolDataTableDef
  , offChainPoolFetchErrorTableDef
  )
import DbSync.Db.Schema.Pool
  ( DelistedPool
  , ReservedPoolTicker
  , delistedPoolTableDef
  , encodeDelistedPoolCopy
  , encodeReservedPoolTickerCopy
  , reservedPoolTickerTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

writeOffChainPoolDataCopy :: LoaderStream -> OffChainPoolData -> IO ()
writeOffChainPoolDataCopy ls d =
  lsWriteRow ls (tdName offChainPoolDataTableDef) (encodeOffChainPoolDataCopy d)

writeOffChainPoolFetchErrorCopy :: LoaderStream -> OffChainPoolFetchError -> IO ()
writeOffChainPoolFetchErrorCopy ls e =
  lsWriteRow ls (tdName offChainPoolFetchErrorTableDef) (encodeOffChainPoolFetchErrorCopy e)

writeDelistedPoolCopy :: LoaderStream -> DelistedPool -> IO ()
writeDelistedPoolCopy ls dp =
  lsWriteRow ls (tdName delistedPoolTableDef) (encodeDelistedPoolCopy dp)

writeReservedPoolTickerCopy :: LoaderStream -> ReservedPoolTicker -> IO ()
writeReservedPoolTickerCopy ls rpt =
  lsWriteRow ls (tdName reservedPoolTickerTableDef) (encodeReservedPoolTickerCopy rpt)
