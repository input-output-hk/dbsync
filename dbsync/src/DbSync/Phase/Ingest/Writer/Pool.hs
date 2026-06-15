-- | COPY writers for tables owned by the @pool@ extractor.
module DbSync.Phase.Ingest.Writer.Pool
  ( writePoolHashCopy
  , writePoolUpdateCopy
  , writePoolMetadataRefCopy
  , writePoolOwnerCopy
  , writePoolRetireCopy
  , writePoolRelayCopy
  ) where

import Cardano.Prelude

import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.Ids (PoolHashId, PoolMetadataRefId, PoolUpdateId)
import DbSync.Db.Schema.Core
  ( PoolHash
  , encodePoolHashCopy
  , poolHashTableDef
  )
import DbSync.Db.Schema.Pool
  ( PoolMetadataRef
  , PoolOwner
  , PoolRelay
  , PoolRetire
  , PoolUpdate
  , encodePoolMetadataRefCopy
  , encodePoolOwnerCopy
  , encodePoolRelayCopy
  , encodePoolRetireCopy
  , encodePoolUpdateCopy
  , poolMetadataRefTableDef
  , poolOwnerTableDef
  , poolRelayTableDef
  , poolRetireTableDef
  , poolUpdateTableDef
  )
import DbSync.Db.Schema.Types (TableDef (..))

writePoolHashCopy :: LoaderStream -> PoolHashId -> PoolHash -> IO ()
writePoolHashCopy ls pid ph = lsWriteRow ls (tdName poolHashTableDef) (encodePoolHashCopy pid ph)

writePoolUpdateCopy :: LoaderStream -> PoolUpdateId -> PoolUpdate -> IO ()
writePoolUpdateCopy ls puid pu = lsWriteRow ls (tdName poolUpdateTableDef) (encodePoolUpdateCopy puid pu)

writePoolMetadataRefCopy :: LoaderStream -> PoolMetadataRefId -> PoolMetadataRef -> IO ()
writePoolMetadataRefCopy ls pmid pm = lsWriteRow ls (tdName poolMetadataRefTableDef) (encodePoolMetadataRefCopy pmid pm)

writePoolOwnerCopy :: LoaderStream -> PoolOwner -> IO ()
writePoolOwnerCopy ls po = lsWriteRow ls (tdName poolOwnerTableDef) (encodePoolOwnerCopy po)

writePoolRetireCopy :: LoaderStream -> PoolRetire -> IO ()
writePoolRetireCopy ls pr = lsWriteRow ls (tdName poolRetireTableDef) (encodePoolRetireCopy pr)

writePoolRelayCopy :: LoaderStream -> PoolRelay -> IO ()
writePoolRelayCopy ls pr = lsWriteRow ls (tdName poolRelayTableDef) (encodePoolRelayCopy pr)
