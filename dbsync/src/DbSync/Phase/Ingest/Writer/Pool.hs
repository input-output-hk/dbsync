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
import DbSync.Db.Schema.Ids
  ( PoolHashId
  , PoolMetadataRefId
  , PoolOwnerId
  , PoolRelayId
  , PoolRetireId
  , PoolUpdateId
  )
import DbSync.Db.Schema.Pool
  ( PoolHash
  , PoolMetadataRef
  , PoolOwner
  , PoolRelay
  , PoolRetire
  , PoolUpdate
  , encodePoolHashCopy
  , encodePoolMetadataRefCopy
  , encodePoolOwnerCopy
  , encodePoolRelayCopy
  , encodePoolRetireCopy
  , encodePoolUpdateCopy
  , poolHashTableDef
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

writePoolOwnerCopy :: LoaderStream -> PoolOwnerId -> PoolOwner -> IO ()
writePoolOwnerCopy ls poid po = lsWriteRow ls (tdName poolOwnerTableDef) (encodePoolOwnerCopy poid po)

writePoolRetireCopy :: LoaderStream -> PoolRetireId -> PoolRetire -> IO ()
writePoolRetireCopy ls prid pr = lsWriteRow ls (tdName poolRetireTableDef) (encodePoolRetireCopy prid pr)

writePoolRelayCopy :: LoaderStream -> PoolRelayId -> PoolRelay -> IO ()
writePoolRelayCopy ls prid pr = lsWriteRow ls (tdName poolRelayTableDef) (encodePoolRelayCopy prid pr)
