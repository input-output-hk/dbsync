-- | Ingest 'IdResolver' fragments for the @pool@ extractor.
module DbSync.Phase.Ingest.Resolver.Pool
  ( resolvePoolHashIngest
  , assignPoolUpdateIdIngest
  , assignPoolMetadataRefIdIngest
  ) where

import Cardano.Prelude

import qualified Data.ByteString.Short as SBS
import Data.IORef (IORef)

import DbSync.Db.Schema.Ids (PoolHashId (..), PoolMetadataRefId (..), PoolUpdateId (..))
import DbSync.Db.Schema.Pool (PoolHash)
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters (..))
import DbSync.Phase.Ingest.DedupStore (DedupStores (..), lookupOrInsert)
import DbSync.Phase.Ingest.Resolver.Internal (allocateNextId)

-- | Dedup lookup against the LSM-backed pool_hash table.
resolvePoolHashIngest
  :: DedupStores -> ByteString -> PoolHash -> IO (PoolHashId, Bool)
resolvePoolHashIngest dedupStores hash _ph = do
  let !key = SBS.toShort hash
  (phId, isNew) <- lookupOrInsert key (dstPoolHash dedupStores)
  pure (PoolHashId phId, isNew)

assignPoolUpdateIdIngest :: IORef ExtractState -> IO PoolUpdateId
assignPoolUpdateIdIngest extractStateRef =
  allocateNextId extractStateRef icPoolUpdateId (\cs c -> cs { icPoolUpdateId = c }) PoolUpdateId

assignPoolMetadataRefIdIngest :: IORef ExtractState -> IO PoolMetadataRefId
assignPoolMetadataRefIdIngest extractStateRef =
  allocateNextId extractStateRef icPoolMetadataRefId (\cs c -> cs { icPoolMetadataRefId = c }) PoolMetadataRefId
