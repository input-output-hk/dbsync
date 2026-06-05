-- | COPY writer for the table owned by the @pool_stats@ extractor.
module DbSync.Phase.Ingest.Writer.PoolStats
  ( writePoolStatCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Pool
  ( PoolStat
  , encodePoolStatCopy
  , poolStatTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

writePoolStatCopy :: LoaderStream -> PoolStat -> IO ()
writePoolStatCopy ls ps =
  lsWriteRow ls (tdName poolStatTableDef) (encodePoolStatCopy ps)
